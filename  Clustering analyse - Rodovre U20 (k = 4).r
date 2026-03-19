# Clustering analyse - Rodovre U20 (k = 4)

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(cluster)
  library(scales)
})

if (!requireNamespace("ggrepel", quietly = TRUE))
  install.packages("ggrepel", quiet = TRUE)
library(ggrepel)

# Farver
SEG_FARVER <- c(
  "Elite"           = "#1B7A34",
  "Udvikling"       = "#E07B00",
  "Udviklingsfokus" = "#C0392B",
  "Saerlig Profil"  = "#1565C0"
)
SEG_LEVELS <- c("Elite", "Udvikling", "Udviklingsfokus", "Saerlig Profil")

COL_TEXT <- "#333333"
COL_MED  <- "#666666"
COL_GRID <- "#E8E8E8"
COL_BG   <- "#FFFFFF"

# Tema
theme_R <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = COL_BG, color = NA),
      panel.background = element_rect(fill = COL_BG, color = NA),
      panel.grid.major = element_line(color = COL_GRID, linewidth = 0.2),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = COL_GRID, fill = NA, linewidth = 0.3),
      plot.title    = element_text(face = "bold", size = rel(1.2), color = COL_TEXT,
                                   hjust = 0, margin = margin(b = 5)),
      plot.subtitle = element_text(size = rel(0.9), color = COL_MED,
                                   hjust = 0, margin = margin(b = 10)),
      axis.title    = element_text(size = rel(0.88), color = COL_MED, face = "bold"),
      axis.text     = element_text(size = rel(0.82), color = COL_MED),
      legend.title  = element_text(face = "bold", size = rel(0.92)),
      legend.text   = element_text(size = rel(0.88)),
      legend.background = element_rect(fill = COL_BG, color = NA),
      plot.caption  = element_text(size = rel(0.72), color = COL_MED, hjust = 0),
      plot.margin   = margin(10, 10, 10, 10)
    )
}


# --- N11 features (bruges kun til segment-info, ikke i clustering) ---

beregn_n11_features <- function(n11_df) {
  date_col <- spiller_col <- NULL
  for (col in c("Dato","Date","date","SESSION_DATE"))
    if (col %in% names(n11_df)) { date_col <- col; break }
  for (col in c("Spiller_Navn","PlayerName","Player_Name","Name","Navn"))
    if (col %in% names(n11_df)) { spiller_col <- col; break }
  if (is.null(date_col) | is.null(spiller_col)) return(NULL)
  
  if (!inherits(n11_df[[date_col]], "Date"))
    n11_df[[date_col]] <- as.Date(n11_df[[date_col]])
  
  n11 <- n11_df %>% filter(!!sym(date_col) >= as.Date("2024-02-01"))
  
  pl_col <- NULL
  for (col in c("Player_Load","PlayerLoad","player_load","Load"))
    if (col %in% names(n11)) { pl_col <- col; break }
  if (!is.null(pl_col) && pl_col != "Player_Load")
    n11 <- n11 %>% rename(Player_Load = !!sym(pl_col))
  if (!"Player_Load" %in% names(n11))
    n11 <- n11 %>% mutate(Player_Load = 1.0)
  
  n11 %>%
    group_by(Spiller_Navn = !!sym(spiller_col)) %>%
    summarise(
      N11_Sessions = n(),
      N11_Avg_Load = mean(Player_Load, na.rm = TRUE),
      N11_Max_Load = max(Player_Load,  na.rm = TRUE),
      .groups = "drop"
    )
}


# --- Dataforberedelse ---

forbered_clustering_data <- function(df) {
  
  var_navne <- c(
    "Alder_2025", "Hojde_cm", "Vaegt_kg", "BMI",
    "Baenkpres_3RM_2024", "Trap_Bar_DL_3RM_2024", "Front_Squat_3RM_2024",
    "Rel_Bench_2024", "Rel_Deadlift_2024", "Rel_Squat_2024",
    "Pull_ups_2024", "Dips_2024", "Laengdehop_Fra_Knae_2024",
    "Baenkpres_AMRAP_70kg_2025", "Squat_AMRAP_100kg_2025",
    "Frivend_AMRAP_60kg_2025", "Pull_ups_2025", "Dips_2025",
    "Laengdehop_Fra_Knae_2025",
    "Pull_ups_Udvikling", "Dips_Udvikling", "Laengdehop_Udvikling",
    "DIU_Leg_Asymmetry_pct", "DIU_Z_Score",
    "Hudl_Kampe_Spillet", "Hudl_Maal", "Hudl_Assists",
    "Hudl_Points_Per_Game", "Hudl_Total_Points"
  )
  
  tilg <- var_navne[var_navne %in% names(df)]
  cdf  <- df[, tilg, drop = FALSE]
  
  for (col in names(cdf))
    if (!is.numeric(cdf[[col]]))
      cdf[[col]] <- suppressWarnings(as.numeric(as.character(cdf[[col]])))
  
  # Fjern kolonner med over 50% NA
  cdf <- cdf[, colMeans(is.na(cdf)) < 0.5, drop = FALSE]
  
  # Imputer med median
  for (col in names(cdf)) {
    idx <- is.na(cdf[[col]])
    if (any(idx)) cdf[[col]][idx] <- median(cdf[[col]], na.rm = TRUE)
  }
  
  # Fjern zero-varians kolonner
  sds <- apply(cdf, 2, sd, na.rm = TRUE)
  cdf <- cdf[, !(is.na(sds) | sds == 0), drop = FALSE]
  
  # Winsorizing 1-99 pct
  for (col in names(cdf)) {
    q <- quantile(cdf[[col]], c(0.01, 0.99), na.rm = TRUE)
    cdf[[col]] <- pmax(pmin(cdf[[col]], q[2]), q[1])
  }
  
  scaled <- scale(cdf)
  scaled[!is.finite(scaled)] <- 0
  
  spiller_info <- df[, intersect(c("Spiller_Nr","Spiller_Navn","Troje_Nr"), names(df)), drop = FALSE]
  
  list(scaled = scaled, original = cdf, spiller_info = spiller_info)
}


# --- Elbow method ---

elbow_method <- function(dat, max_k = 10) {
  wss <- sapply(1:max_k, function(k) {
    set.seed(123); kmeans(dat, centers = k, nstart = 25, iter.max = 100)$tot.withinss
  })
  x <- (0:(max_k-1))/(max_k-1)
  y <- (wss - min(wss))/diff(range(wss))
  d <- sapply(seq_along(x), function(i)
    abs((y[max_k]-y[1])*x[i]-(x[max_k]-x[1])*y[i]+x[max_k]*y[1]-y[max_k]*x[1]) /
      sqrt((y[max_k]-y[1])^2+(x[max_k]-x[1])^2))
  opt_k <- which.max(d)
  
  df_p <- data.frame(k = 1:max_k, WSS = wss)
  p <- ggplot(df_p, aes(k, WSS)) + theme_R() +
    geom_line(color = COL_MED, linewidth = 0.9) +
    geom_point(color = COL_MED, size = 2.5) +
    geom_point(data = df_p[df_p$k == opt_k,],
               fill = "navy", color = "navy", size = 7, shape = 21, stroke = 1.5) +
    geom_vline(xintercept = opt_k, linetype = "dashed", color = "navy", alpha = 0.5) +
    annotate("label", x = opt_k + ifelse(opt_k < max_k - 1, 0.4, -0.4),
             y = max(wss) * 0.82,
             label = paste0("k = ", opt_k), size = 3.8, fontface = "bold",
             color = "navy", fill = "white", label.padding = unit(0.3,"lines")) +
    scale_x_continuous(breaks = 1:max_k) +
    scale_y_continuous(labels = comma_format(big.mark=".", decimal.mark=",")) +
    labs(title    = "Elbow Method",
         subtitle = sprintf("Anbefalet k = %d (WSS knaek)", opt_k),
         x = "Antal clusters (k)", y = "Within-cluster SS")
  list(opt_k = opt_k, plot = p)
}


# --- Gap statistic ---

gap_statistic_method <- function(dat, max_k = 10, B = 50) {
  set.seed(123)
  gap   <- clusGap(dat, FUN = kmeans, nstart = 25, K.max = max_k, B = B, verbose = FALSE)
  opt_k <- maxSE(gap$Tab[,"gap"], gap$Tab[,"SE.sim"], method = "firstSEmax")
  df_p  <- data.frame(k = 1:max_k, gap = gap$Tab[,"gap"], se = gap$Tab[,"SE.sim"])
  
  p <- ggplot(df_p, aes(k, gap)) + theme_R() +
    geom_ribbon(aes(ymin = gap-se, ymax = gap+se), fill = "grey70", alpha = 0.2) +
    geom_errorbar(aes(ymin = gap-se, ymax = gap+se),
                  width = 0.2, color = COL_MED, linewidth = 0.5) +
    geom_line(color = COL_MED, linewidth = 0.9) +
    geom_point(color = COL_MED, size = 2.5) +
    geom_point(data = df_p[df_p$k == opt_k,],
               fill = "navy", color = "navy", size = 7, shape = 21, stroke = 1.5) +
    geom_vline(xintercept = opt_k, linetype = "dashed", color = "navy", alpha = 0.5) +
    annotate("label", x = opt_k + ifelse(opt_k < max_k - 1, 0.4, -0.4),
             y = max(df_p$gap) * 0.97,
             label = paste0("k = ", opt_k), size = 3.8, fontface = "bold",
             color = "navy", fill = "white", label.padding = unit(0.3,"lines")) +
    scale_x_continuous(breaks = 1:max_k) +
    labs(title    = "Gap Statistic",
         subtitle = sprintf("Anbefalet k = %d (firstSEmax)", opt_k),
         x = "Antal clusters (k)", y = "Gap (+/- 1 SE)")
  list(opt_k = opt_k, plot = p)
}


# --- Silhouette ---

silhouette_analyse <- function(dat, k_values = 2:6) {
  scores <- sapply(k_values, function(k) {
    set.seed(123)
    km  <- kmeans(dat, centers = k, nstart = 25, iter.max = 100)
    sil <- silhouette(km$cluster, dist(dat))
    mean(sil[,"sil_width"])
  })
  best_k <- k_values[which.max(scores)]
  df_p   <- data.frame(k = k_values, sil = scores)
  
  p <- ggplot(df_p, aes(k, sil)) + theme_R() +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=0.5,  ymax=Inf,  fill="#1B7A34", alpha=0.06) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=0.25, ymax=0.5,  fill="#E07B00", alpha=0.06) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=0,    ymax=0.25, fill="#C0392B", alpha=0.04) +
    annotate("text", x=min(k_values)+0.05, y=0.52,
             label="God separation (> 0.5)", hjust=0, size=2.8, color="#1B7A34") +
    annotate("text", x=min(k_values)+0.05, y=0.27,
             label="Moderat (0.25-0.5)", hjust=0, size=2.8, color="#E07B00") +
    geom_hline(yintercept = c(0.25, 0.5), linetype="dotted",
               color=COL_MED, linewidth=0.3) +
    geom_line(color=COL_MED, linewidth=0.9) +
    geom_point(color=COL_MED, size=2.5) +
    geom_point(data=df_p[df_p$k==best_k,],
               fill="navy", color="navy", size=7, shape=21, stroke=1.5) +
    geom_vline(xintercept=best_k, linetype="dashed", color="navy", alpha=0.5) +
    annotate("label", x=best_k, y=max(scores)+0.07,
             label=sprintf("k = %d\n%.3f", best_k, max(scores)),
             size=3.8, fontface="bold", color="navy",
             fill="white", label.padding=unit(0.3,"lines")) +
    scale_x_continuous(breaks=k_values) +
    scale_y_continuous(limits=c(0, max(0.75, max(scores)*1.2))) +
    labs(title    = "Silhouette Score",
         subtitle = sprintf("Anbefalet k = %d (score = %.3f)", best_k, max(scores)),
         x = "Antal clusters (k)", y = "Gennemsnitlig silhouette")
  list(best_k=best_k, plot=p, scores=scores)
}


# --- Segment labels ---
# Saerlig Profil = exceptionelle spillere (gode outliers)
# Udviklingsfokus = svage outliers

lav_segment_labels <- function(cluster_labels, orig_data) {
  
  vars_perf <- intersect(
    c("Rel_Squat_2024","Rel_Bench_2024","Rel_Deadlift_2024",
      "Pull_ups_Udvikling","Dips_Udvikling",
      "Hudl_Points_Per_Game","Hudl_Total_Points"),
    names(orig_data)
  )
  
  perf_mat     <- data.frame(lapply(orig_data[, vars_perf, drop = FALSE], as.numeric))
  spiller_perf <- rowMeans(perf_mat, na.rm = TRUE)
  
  df <- data.frame(Perf = spiller_perf, Cluster = cluster_labels)
  
  cluster_stats <- df %>%
    group_by(Cluster) %>%
    summarise(N = n(), Perf = mean(Perf, na.rm = TRUE), .groups = "drop") %>%
    arrange(N)
  
  outlier_cluster  <- cluster_stats$Cluster[1]
  outlier_spillere <- df %>% filter(Cluster == outlier_cluster)
  outlier_median   <- median(outlier_spillere$Perf, na.rm = TRUE)
  overall_median   <- median(spiller_perf, na.rm = TRUE)
  
  rest <- cluster_stats %>%
    filter(Cluster != outlier_cluster) %>%
    arrange(desc(Perf))
  
  label_map <- character()
  
  if (outlier_median > overall_median) {
    label_map[as.character(outlier_cluster)] <- "Saerlig Profil"
    if (nrow(rest) >= 1) label_map[as.character(rest$Cluster[1])] <- "Elite"
    if (nrow(rest) >= 2) label_map[as.character(rest$Cluster[2])] <- "Udvikling"
    if (nrow(rest) >= 3) label_map[as.character(rest$Cluster[3])] <- "Udviklingsfokus"
  } else {
    if (nrow(rest) >= 1) label_map[as.character(rest$Cluster[1])] <- "Saerlig Profil"
    if (nrow(rest) >= 2) label_map[as.character(rest$Cluster[2])] <- "Elite"
    if (nrow(rest) >= 3) label_map[as.character(rest$Cluster[3])] <- "Udvikling"
    label_map[as.character(outlier_cluster)] <- "Udviklingsfokus"
  }
  
  label_map[!is.na(label_map)]
}


# --- PCA visualisering ---

pca_visualisering <- function(dat_scaled, orig_data, spiller_info, k = 4) {
  
  pca     <- prcomp(dat_scaled)
  var_exp <- summary(pca)$importance[2,] * 100
  
  set.seed(123)
  km        <- kmeans(dat_scaled, centers = k, nstart = 100, iter.max = 300)
  label_map <- lav_segment_labels(km$cluster, orig_data)
  
  pca_df <- data.frame(
    PC1     = pca$x[,1],
    PC2     = pca$x[,2],
    Segment = factor(label_map[as.character(km$cluster)], levels = SEG_LEVELS),
    Navn    = spiller_info$Spiller_Navn,
    Nr      = as.character(spiller_info$Troje_Nr)
  )
  
  hull_df <- pca_df %>%
    group_by(Segment) %>%
    filter(n() >= 3) %>%
    slice(chull(PC1, PC2)) %>%
    ungroup()
  
  seg_n   <- table(pca_df$Segment)
  seg_lbl <- setNames(
    sprintf("%s  (n=%d)", names(seg_n), as.integer(seg_n)),
    names(seg_n)
  )
  
  segs_i_brug   <- levels(pca_df$Segment)[levels(pca_df$Segment) %in% unique(pca_df$Segment)]
  farver_i_brug <- SEG_FARVER[segs_i_brug]
  
  p <- ggplot(pca_df, aes(PC1, PC2)) +
    theme_R(base_size = 13) +
    theme(
      legend.position  = "right",
      legend.key.size  = unit(1.1, "cm"),
      legend.spacing.y = unit(0.6, "cm"),
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11)
    ) +
    geom_polygon(
      data = hull_df,
      aes(fill = Segment, color = Segment, group = Segment),
      alpha = 0.13, linewidth = 1.8
    ) +
    geom_point(aes(color = Segment), size = 8, alpha = 0.92, shape = 16) +
    geom_text(
      aes(label = Nr),
      size = 2.7, fontface = "bold", color = "white",
      show.legend = FALSE
    ) +
    geom_text_repel(
      aes(label = Navn, color = Segment),
      size               = 2.6,
      fontface           = "plain",
      box.padding        = 0.4,
      point.padding      = 0.6,
      max.overlaps       = 20,
      segment.size       = 0.3,
      segment.alpha      = 0.4,
      min.segment.length = 0.2,
      show.legend        = FALSE,
      seed               = 42
    ) +
    scale_color_manual(values = farver_i_brug, labels = seg_lbl, name = "Segment") +
    scale_fill_manual( values = farver_i_brug, guide  = "none") +
    labs(
      title    = "Spillersegmentering - Rodovre U20",
      subtitle = sprintf("4 segmenter  |  PC1+PC2 forklarer %.0f%% af variansen", sum(var_exp[1:2])),
      x        = sprintf("PC1  (%.1f%% varians)", var_exp[1]),
      y        = sprintf("PC2  (%.1f%% varians)", var_exp[2]),
      caption  = "Saerlig Profil = Exceptionelle spillere  |  Udviklingsfokus inkl. svage outliers"
    )
  
  list(plot = p, pca_data = pca_df, var_exp = var_exp, km = km)
}


# --- Segment rapport ---

print_segment_rapport <- function(pca_res) {
  for (seg in SEG_LEVELS) {
    spillere <- pca_res$pca_data %>%
      filter(Segment == seg) %>%
      arrange(as.numeric(Nr))
    
    if (nrow(spillere) == 0) next
    
    pct <- round(nrow(spillere) / nrow(pca_res$pca_data) * 100)
    cat(sprintf("%s  (%d spillere, %d%%)\n", seg, nrow(spillere), pct))
    for (i in seq_len(nrow(spillere))) {
      cat(sprintf("   #%-3s  %s\n", spillere$Nr[i], spillere$Navn[i]))
    }
    cat("\n")
  }
}


# --- Hoved-funktion ---

kor_clustering_analyse <- function(final_k = 4, max_k = 10, gap_B = 50) {
  
  if (!exists("Rodovre_Komplet_Fysio_Profil", envir = .GlobalEnv))
    stop("Rodovre_Komplet_Fysio_Profil ikke fundet")
  fysio_df <- get("Rodovre_Komplet_Fysio_Profil", envir = .GlobalEnv)
  
  n11_features <- NULL
  for (nm in c("n11_all","NEXT11_DATA","N11_Data"))
    if (exists(nm, envir = .GlobalEnv)) {
      tryCatch({
        n11_features <- beregn_n11_features(get(nm, envir = .GlobalEnv))
      }, error = function(e) NULL)
      break
    }
  
  prep     <- forbered_clustering_data(fysio_df)
  elbow_res <- elbow_method(prep$scaled, max_k)
  gap_res   <- gap_statistic_method(prep$scaled, max_k, gap_B)
  sil_res   <- silhouette_analyse(prep$scaled, 2:7)
  pca_res   <- pca_visualisering(prep$scaled, prep$original, prep$spiller_info, k = final_k)
  
  print_segment_rapport(pca_res)
  
  combined <- (elbow_res$plot + gap_res$plot) /
    (sil_res$plot + pca_res$plot) +
    plot_layout(heights = c(1, 1.2)) +
    plot_annotation(
      title    = "Clustering Analyse - Rodovre U20",
      subtitle = sprintf(
        "Elbow: k=%d  |  Gap: k=%d  |  Silhouette: k=%d  |  Anvendt: k=%d",
        elbow_res$opt_k, gap_res$opt_k, sil_res$best_k, final_k
      ),
      theme = theme(
        plot.title    = element_text(face="bold", size=16, hjust=0.5, color=COL_TEXT,
                                     margin=margin(b=4)),
        plot.subtitle = element_text(size=11, hjust=0.5, color=COL_MED,
                                     margin=margin(b=16)),
        plot.background = element_rect(fill=COL_BG, color=NA),
        plot.margin   = margin(16,16,16,16)
      )
    )
  
  assign("Clustering_Plot",   combined,          envir = .GlobalEnv)
  assign("PCA_Plot",          pca_res$plot,      envir = .GlobalEnv)
  assign("PCA_Data",          pca_res$pca_data,  envir = .GlobalEnv)
  assign("Segment_Fordeling", table(pca_res$pca_data$Segment), envir = .GlobalEnv)
  
  ggsave("clustering_analyse.png", combined,     width=16, height=12, dpi=300, bg="white")
  ggsave("pca_segmentering.png",   pca_res$plot, width=12, height=8,  dpi=300, bg="white")
  
  print(combined)
  print(pca_res$plot)
  
  invisible(list(prep=prep, elbow=elbow_res, gap=gap_res,
                 sil=sil_res, pca=pca_res, combined=combined))
}


# --- Kør analyse ---

resultater <- kor_clustering_analyse(
  final_k = 4,
  max_k   = 10,
  gap_B   = 50
)