# Spillersegmentering - baseline vs. med N11 data

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(cluster)
  library(scales)
  library(ggrepel)
  library(gridExtra)
  library(grid)
})

SEG_FARVER <- c(
  "Elite"           = "#2E7D32",
  "Udvikling"       = "#F57C00",
  "Udviklingsfokus" = "#C62828",
  "Saerlig Profil"  = "#1565C0"
)
SEG_LEVELS <- c("Elite", "Udvikling", "Udviklingsfokus", "Saerlig Profil")


# --- Segment labels ---

lav_segment_labels <- function(cluster_labels, orig_data) {
  vars_perf <- intersect(
    c("Rel_Squat_2024", "Rel_Bench_2024", "Rel_Deadlift_2024",
      "Pull_ups_Udvikling", "Dips_Udvikling",
      "Hudl_Points_Per_Game", "Hudl_Total_Points"),
    names(orig_data)
  )
  
  perf_mat     <- data.frame(lapply(orig_data[, vars_perf, drop = FALSE], as.numeric))
  spiller_perf <- rowMeans(perf_mat, na.rm = TRUE)
  df           <- data.frame(Perf = spiller_perf, Cluster = cluster_labels)
  
  cluster_stats <- df %>%
    group_by(Cluster) %>%
    summarise(N = n(), Perf = mean(Perf, na.rm = TRUE), .groups = "drop") %>%
    arrange(N)
  
  outlier_cluster <- cluster_stats$Cluster[1]
  outlier_median  <- median(df$Perf[df$Cluster == outlier_cluster], na.rm = TRUE)
  overall_median  <- median(spiller_perf, na.rm = TRUE)
  
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
  
  return(label_map)
}


# --- Forbehandling ---

preprocess <- function(df, vars) {
  tilg <- vars[vars %in% names(df)]
  cdf  <- df[, tilg, drop = FALSE]
  
  for (col in names(cdf))
    if (!is.numeric(cdf[[col]]))
      cdf[[col]] <- suppressWarnings(as.numeric(as.character(cdf[[col]])))
  
  cdf <- cdf[, colMeans(is.na(cdf)) < 0.5, drop = FALSE]
  
  for (col in names(cdf)) {
    idx <- is.na(cdf[[col]])
    if (any(idx)) cdf[[col]][idx] <- median(cdf[[col]], na.rm = TRUE)
  }
  
  sds <- apply(cdf, 2, sd, na.rm = TRUE)
  cdf <- cdf[, !(is.na(sds) | sds == 0), drop = FALSE]
  
  for (col in names(cdf)) {
    q <- quantile(cdf[[col]], c(0.01, 0.99), na.rm = TRUE)
    cdf[[col]] <- pmax(pmin(cdf[[col]], q[2]), q[1])
  }
  
  scaled <- scale(cdf)
  scaled[!is.finite(scaled)] <- 0
  
  list(scaled = scaled, original = cdf)
}

get_hulls <- function(data) {
  data %>%
    group_by(Segment) %>%
    filter(n() >= 3) %>%
    slice(chull(PC1, PC2)) %>%
    ungroup()
}


# --- Data ---

fysio <- get("Rodovre_Komplet_Fysio_Profil", envir = .GlobalEnv) %>%
  group_by(Spiller_Nr) %>% slice(1) %>% ungroup()

n11_sessions <- ls(pattern = "^rod(24|25)_\\d{8}$", envir = .GlobalEnv)
n11_all <- data.frame()
for (session in n11_sessions) {
  df <- get(session, envir = .GlobalEnv)
  if (all(c("Spiller_Navn", "Total Player Load") %in% names(df))) {
    n11_all <- bind_rows(n11_all, df)
  }
}

n11_metrics <- n11_all %>%
  group_by(Spiller_Navn) %>%
  summarise(
    N11_Sessions   = n(),
    N11_Total_Load = sum(`Total Player Load`, na.rm = TRUE),
    N11_Avg_Load   = mean(`Total Player Load`, na.rm = TRUE),
    N11_Max_Load   = max(`Total Player Load`, na.rm = TRUE),
    N11_Load_CV    = sd(`Total Player Load`, na.rm = TRUE) / mean(`Total Player Load`, na.rm = TRUE) * 100,
    .groups = "drop"
  )

n11_matched <- fysio %>%
  inner_join(n11_metrics, by = "Spiller_Navn") %>%
  pull(Spiller_Navn)


# --- Baseline clustering ---

base_vars <- c(
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

baseline_prep <- preprocess(fysio, base_vars)
set.seed(123)
km_baseline     <- kmeans(baseline_prep$scaled, centers = 4, nstart = 100, iter.max = 300)
labels_baseline <- lav_segment_labels(km_baseline$cluster, baseline_prep$original)

baseline <- data.frame(
  Spiller_Nr       = fysio$Spiller_Nr,
  Spiller_Navn     = fysio$Spiller_Navn,
  Troje_Nr         = fysio$Troje_Nr,
  Segment_Baseline = labels_baseline[as.character(km_baseline$cluster)],
  Har_N11          = fysio$Spiller_Navn %in% n11_matched
)


# --- Clustering med N11 ---

fysio_n11 <- fysio %>% inner_join(n11_metrics, by = "Spiller_Navn")
n11_vars  <- c(base_vars, "N11_Sessions", "N11_Total_Load", "N11_Avg_Load", "N11_Max_Load", "N11_Load_CV")
n11_prep  <- preprocess(fysio_n11, n11_vars)
set.seed(123)
km_n11     <- kmeans(n11_prep$scaled, centers = 4, nstart = 100, iter.max = 300)
labels_n11 <- lav_segment_labels(km_n11$cluster, n11_prep$original)

n11_seg <- data.frame(
  Spiller_Nr      = fysio_n11$Spiller_Nr,
  Segment_Med_N11 = labels_n11[as.character(km_n11$cluster)]
)


# --- Kombiner ---

final <- baseline %>%
  left_join(n11_seg, by = "Spiller_Nr") %>%
  mutate(
    Segment_Final = ifelse(!is.na(Segment_Med_N11), Segment_Med_N11, Segment_Baseline),
    Aendret       = !is.na(Segment_Med_N11) & Segment_Baseline != Segment_Med_N11
  )

n_total    <- nrow(final)
n_med_n11  <- sum(final$Har_N11)
n_aendret  <- sum(final$Aendret)
pct_aendret <- round(n_aendret / n_med_n11 * 100, 1)

spillere_aendret <- final %>%
  filter(Aendret) %>%
  arrange(Segment_Baseline, as.numeric(Troje_Nr))


# --- Plot ---

pca_baseline <- prcomp(baseline_prep$scaled, scale. = FALSE)
var_exp      <- round(100 * summary(pca_baseline)$importance[2, 1:2], 1)

plot_data <- data.frame(
  PC1     = pca_baseline$x[,1],
  PC2     = pca_baseline$x[,2],
  Segment = factor(final$Segment_Final, levels = SEG_LEVELS),
  Navn    = final$Spiller_Navn,
  Troje   = final$Troje_Nr,
  Aendret = final$Aendret
)

hulls      <- get_hulls(plot_data)
seg_counts <- plot_data %>%
  group_by(Segment) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(label = sprintf("%s (n=%d)", Segment, n))

p_cluster <- ggplot(plot_data, aes(x = PC1, y = PC2)) +
  geom_polygon(data = hulls, aes(fill = Segment, color = Segment),
               alpha = 0.15, linewidth = 2.8, show.legend = FALSE) +
  geom_point(aes(fill = Segment), size = 9, shape = 21, color = "white", stroke = 2.2) +
  geom_text(aes(label = Troje), size = 3.8, fontface = "bold", color = "white") +
  scale_fill_manual(values = SEG_FARVER, labels = seg_counts$label, name = "Segment") +
  scale_color_manual(values = SEG_FARVER, guide = "none") +
  labs(
    title    = "Spillersegmentering - Rodovre U20",
    subtitle = sprintf("PC1+PC2 forklarer %g%% af variansen | %d med N11 | %d aendrede (%g%%)",
                       sum(var_exp), n_med_n11, n_aendret, pct_aendret),
    x = sprintf("PC1 (%g%% varians)", var_exp[1]),
    y = sprintf("PC2 (%g%% varians)", var_exp[2])
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#fafafa", color = NA),
    panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 22, hjust = 0, color = "#1a1a1a"),
    plot.subtitle    = element_text(size = 12, hjust = 0, lineheight = 1.4, color = "#4a4a4a"),
    axis.title       = element_text(face = "bold", size = 13, color = "#2a2a2a"),
    axis.text        = element_text(size = 12, color = "#3a3a3a"),
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 14),
    legend.text      = element_text(size = 13),
    legend.key.size  = unit(1.3, "cm"),
    panel.border     = element_rect(color = "#c0c0c0", fill = NA, linewidth = 0.8)
  )

seg_comparison <- bind_rows(
  final %>% count(Segment = Segment_Baseline) %>% mutate(Type = "Baseline"),
  final %>% count(Segment = Segment_Final)    %>% mutate(Type = "Efter N11")
) %>%
  mutate(
    Segment = factor(Segment, levels = SEG_LEVELS),
    Type    = factor(Type, levels = c("Baseline", "Efter N11"))
  )

p_comparison <- ggplot(seg_comparison, aes(x = Segment, y = n, fill = Segment)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = n), vjust = -0.5, size = 6, fontface = "bold") +
  facet_wrap(~Type) +
  scale_fill_manual(values = SEG_FARVER) +
  labs(title = "Sammenligning for/efter N11", y = "Antal", x = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 12)),
    strip.text       = element_text(face = "bold", size = 15, margin = margin(b = 10)),
    strip.background = element_rect(fill = "#f0f0f0", color = "#c0c0c0", linewidth = 0.8),
    legend.position  = "none",
    axis.text.x      = element_text(angle = 30, hjust = 1, size = 12, face = "bold"),
    axis.text.y      = element_text(size = 12),
    axis.title.y     = element_text(size = 13, face = "bold"),
    panel.border     = element_rect(color = "#c0c0c0", fill = NA, linewidth = 0.8)
  ) +
  ylim(0, max(seg_comparison$n) * 1.25)

plot_A <- p_cluster + p_comparison +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(
    caption = "Saerlig Profil = Exceptionelle spillere | Udviklingsfokus inkl. svage outliers",
    theme = theme(
      plot.caption    = element_text(size = 10, hjust = 0, margin = margin(t = 12)),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(20, 20, 20, 20)
    )
  )


# --- Gem ---

ggsave("SPILLERSEGMENTERING.png", plot_A, width = 22, height = 14, dpi = 350, bg = "white")
print(plot_A)

assign("Segmentering_Final",   final,            envir = .GlobalEnv)
assign("Segmentering_Aendrede", spillere_aendret, envir = .GlobalEnv)

aendringsliste <- spillere_aendret %>%
  mutate(Aendring = paste0(Segment_Baseline, " -> ", Segment_Final)) %>%
  select(Troje_Nr, Spiller_Navn, Segment_Baseline, Segment_Final, Aendring)
write.csv(aendringsliste, "SEGMENT_AENDRINGER.csv", row.names = FALSE)


# --- Konsoloutput ---

cat(sprintf("Spillere i alt: %d\n", n_total))
cat(sprintf("Med N11 data:   %d\n", n_med_n11))
cat(sprintf("Aendret segment:%d (%g%%)\n", n_aendret, pct_aendret))
cat(sprintf("Uaendret:       %d (%g%%)\n\n", n_med_n11 - n_aendret, 100 - pct_aendret))

cat("Segment fordeling:\n")
for (seg in SEG_LEVELS) {
  n_seg   <- sum(final$Segment_Final == seg)
  pct_seg <- round(n_seg / n_total * 100, 1)
  cat(sprintf("  %-20s %2d spillere (%g%%)\n", seg, n_seg, pct_seg))
}

if (nrow(spillere_aendret) > 0) {
  cat("\nSpillere der skiftede segment:\n")
  for (i in seq_len(nrow(spillere_aendret))) {
    cat(sprintf("  #%-2s  %-25s  %s -> %s\n",
                spillere_aendret$Troje_Nr[i],
                spillere_aendret$Spiller_Navn[i],
                spillere_aendret$Segment_Baseline[i],
                spillere_aendret$Segment_Final[i]))
  }
}