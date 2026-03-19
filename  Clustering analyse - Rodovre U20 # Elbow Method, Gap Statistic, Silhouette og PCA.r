# Clustering analyse - Rodovre U20
# Elbow Method, Gap Statistic, Silhouette og PCA

library(tidyverse)
library(cluster)
library(factoextra)
library(patchwork)
library(scales)

FARVER <- list(
  n11       = "#FF69B4",
  hudl      = "#FF8C00",
  diu       = "#DC143C",
  neutral   = "#4169E1",
  primary   = "#0066CC",
  secondary = "#00AA88",
  warning   = "#FFA500",
  danger    = "#FF4444"
)


# --- Dataforberedelse ---

forbered_clustering_data <- function(df, min_komplethed = 70, max_na_pct = 0.5) {
  
  clustering_var_names <- c(
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
    "Hudl_Points_Per_Game", "Hudl_Total_Points",
    "Injury_Risk", "Performance_Level"
  )
  
  available_vars <- clustering_var_names[clustering_var_names %in% names(df)]
  missing_vars   <- clustering_var_names[!clustering_var_names %in% names(df)]
  
  if (length(missing_vars) > 0) {
    cat("Advarsel: folgende variable ikke fundet:\n")
    cat(paste(" ", missing_vars, collapse = "\n"), "\n\n")
  }
  
  clustering_vars <- df[, available_vars, drop = FALSE]
  
  for (col in names(clustering_vars)) {
    if (!is.numeric(clustering_vars[[col]]))
      clustering_vars[[col]] <- as.numeric(as.character(clustering_vars[[col]]))
  }
  
  na_counts       <- colSums(is.na(clustering_vars))
  max_na_threshold <- nrow(df) * max_na_pct
  valid_cols      <- names(clustering_vars)[na_counts < max_na_threshold]
  clustering_vars <- clustering_vars[, valid_cols, drop = FALSE]
  
  if ("Data_Komplethed_Pct" %in% names(df)) {
    valid_spillere  <- df$Data_Komplethed_Pct >= min_komplethed
    clustering_vars <- clustering_vars[valid_spillere, , drop = FALSE]
    spiller_info    <- df[valid_spillere, c("Spiller_Nr","Spiller_Navn","Troje_Nr"), drop = FALSE]
  } else {
    spiller_info <- df[, c("Spiller_Nr","Spiller_Navn","Troje_Nr"), drop = FALSE]
  }
  
  for (col in names(clustering_vars)) {
    if (sum(is.na(clustering_vars[[col]])) > 0)
      clustering_vars[[col]][is.na(clustering_vars[[col]])] <-
        median(clustering_vars[[col]], na.rm = TRUE)
  }
  
  clustering_scaled <- scale(clustering_vars)
  
  list(
    scaled         = clustering_scaled,
    original       = clustering_vars,
    spiller_info   = spiller_info,
    variable_navne = colnames(clustering_vars)
  )
}


# --- Elbow method ---

elbow_method <- function(data_scaled, max_k = 10) {
  
  wss <- numeric(max_k)
  for (k in 1:max_k) {
    set.seed(123)
    km     <- kmeans(data_scaled, centers = k, nstart = 25, iter.max = 50)
    wss[k] <- km$tot.withinss
  }
  
  wss_diff1 <- diff(wss)
  wss_diff2 <- diff(wss_diff1)
  optimal_k <- which.min(wss_diff2) + 1
  
  elbow_data <- data.frame(k = 1:max_k, WSS = wss)
  
  p_elbow <- ggplot(elbow_data, aes(x = k, y = WSS)) +
    geom_line(color = FARVER$neutral, size = 1.2) +
    geom_point(color = FARVER$neutral, size = 3) +
    geom_point(data = elbow_data[elbow_data$k == optimal_k, ],
               aes(x = k, y = WSS),
               color = FARVER$diu, size = 5, shape = 18) +
    geom_vline(xintercept = optimal_k, linetype = "dashed",
               color = FARVER$diu, alpha = 0.5) +
    scale_x_continuous(breaks = 1:max_k) +
    scale_y_continuous(labels = comma) +
    labs(
      title    = "Elbow Method",
      subtitle = sprintf("Anbefalet k = %d (WSS kurve)", optimal_k),
      x = "Antal clusters (k)",
      y = "Within-cluster Sum of Squares (WSS)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(color = FARVER$diu, size = 11),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "grey80", fill = NA)
    )
  
  list(wss = wss, optimal_k = optimal_k, plot = p_elbow, data = elbow_data)
}


# --- Gap statistic ---

gap_statistic_method <- function(data_scaled, max_k = 10, B = 50) {
  
  set.seed(123)
  gap_result <- clusGap(data_scaled, FUN = kmeans, nstart = 25,
                        K.max = max_k, B = B, verbose = FALSE)
  
  optimal_k <- maxSE(gap_result$Tab[, "gap"], gap_result$Tab[, "SE.sim"],
                     method = "firstSEmax")
  
  gap_data <- data.frame(
    k   = 1:max_k,
    gap = gap_result$Tab[, "gap"],
    se  = gap_result$Tab[, "SE.sim"]
  )
  
  p_gap <- ggplot(gap_data, aes(x = k, y = gap)) +
    geom_line(color = FARVER$neutral, size = 1.2) +
    geom_point(color = FARVER$neutral, size = 3) +
    geom_errorbar(aes(ymin = gap - se, ymax = gap + se),
                  width = 0.2, color = FARVER$neutral, alpha = 0.5) +
    geom_point(data = gap_data[gap_data$k == optimal_k, ],
               aes(x = k, y = gap),
               color = FARVER$diu, size = 5, shape = 18) +
    geom_vline(xintercept = optimal_k, linetype = "dashed",
               color = FARVER$diu, alpha = 0.5) +
    scale_x_continuous(breaks = 1:max_k) +
    labs(
      title    = "Gap Statistic",
      subtitle = sprintf("Anbefalet k = %d (firstSEmax)", optimal_k),
      x = "Antal clusters (k)",
      y = "Gap Statistic (med SE)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(color = FARVER$diu, size = 11),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "grey80", fill = NA)
    )
  
  list(gap_result = gap_result, optimal_k = optimal_k, plot = p_gap, data = gap_data)
}


# --- K-means med silhouette ---

kor_kmeans_analyse <- function(data_scaled, k_values = c(3, 4, 5, 6)) {
  
  resultater        <- list()
  silhouette_scores <- numeric(length(k_values))
  
  for (i in seq_along(k_values)) {
    k <- k_values[i]
    set.seed(123)
    km      <- kmeans(data_scaled, centers = k, nstart = 25, iter.max = 100)
    sil     <- silhouette(km$cluster, dist(data_scaled))
    avg_sil <- mean(sil[, "sil_width"])
    silhouette_scores[i] <- avg_sil
    
    resultater[[sprintf("k%d", k)]] <- list(
      kmeans         = km,
      silhouette     = sil,
      avg_silhouette = avg_sil
    )
  }
  
  best_k   <- k_values[which.max(silhouette_scores)]
  sil_data <- data.frame(k = k_values, silhouette = silhouette_scores)
  
  p_silhouette <- ggplot(sil_data, aes(x = k, y = silhouette)) +
    geom_line(color = FARVER$neutral, size = 1.2) +
    geom_point(color = FARVER$neutral, size = 4) +
    geom_point(data = sil_data[sil_data$k == best_k, ],
               aes(x = k, y = silhouette),
               color = FARVER$hudl, size = 6, shape = 18) +
    geom_vline(xintercept = best_k, linetype = "dashed",
               color = FARVER$hudl, alpha = 0.5) +
    scale_x_continuous(breaks = k_values) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(
      title    = "Silhouette Score",
      subtitle = sprintf("Bedste k = %d (score = %.3f)", best_k, max(silhouette_scores)),
      x = "Antal clusters (k)",
      y = "Gennemsnitlig silhouette score"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(color = FARVER$hudl, size = 11),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "grey80", fill = NA)
    )
  
  list(resultater = resultater, best_k = best_k,
       silhouette_scores = silhouette_scores, plot = p_silhouette, data = sil_data)
}


# --- PCA visualisering ---

kor_pca_analyse <- function(data_scaled, spiller_info, k = 4) {
  
  pca         <- prcomp(data_scaled)
  var_explained <- summary(pca)$importance[2, ] * 100
  
  set.seed(123)
  km <- kmeans(data_scaled, centers = k, nstart = 25, iter.max = 100)
  
  pca_data <- data.frame(
    PC1      = pca$x[, 1],
    PC2      = pca$x[, 2],
    Cluster  = factor(km$cluster),
    Troje_Nr = spiller_info$Troje_Nr
  )
  
  cluster_colors <- c(FARVER$n11, FARVER$hudl, FARVER$diu, FARVER$neutral)[1:k]
  
  p_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 4, alpha = 0.7) +
    geom_text(aes(label = Troje_Nr), size = 3, color = "black",
              fontface = "bold", nudge_y = 0.3) +
    stat_ellipse(aes(color = Cluster), type = "norm", level = 0.68,
                 size = 1, alpha = 0.3) +
    scale_color_manual(values = cluster_colors,
                       labels = sprintf("Segment %d", 1:k)) +
    labs(
      title    = sprintf("PCA Biplot - %d spillersegmenter", k),
      subtitle = sprintf("PC1: %.1f%% | PC2: %.1f%% | Total: %.1f%%",
                         var_explained[1], var_explained[2], sum(var_explained[1:2])),
      x = sprintf("PC1 (%.1f%%)", var_explained[1]),
      y = sprintf("PC2 (%.1f%%)", var_explained[2])
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 10, color = "grey40"),
      legend.position  = "right",
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "grey80", fill = NA)
    )
  
  list(pca = pca, kmeans = km, var_explained = var_explained,
       plot = p_pca, data = pca_data)
}


# --- Rapport og kombineret plot ---

generer_clustering_rapport <- function(elbow_res, gap_res, kmeans_res, pca_res) {
  
  anbefalinger <- c(elbow_res$optimal_k, gap_res$optimal_k, kmeans_res$best_k)
  konsensus_k  <- as.numeric(names(sort(table(anbefalinger), decreasing = TRUE)[1]))
  
  cat(sprintf("Elbow:      k = %d\n", elbow_res$optimal_k))
  cat(sprintf("Gap:        k = %d\n", gap_res$optimal_k))
  cat(sprintf("Silhouette: k = %d\n", kmeans_res$best_k))
  cat(sprintf("Konsensus:  k = %d\n\n", konsensus_k))
  
  k4_results <- kmeans_res$resultater$k4
  if (!is.null(k4_results)) {
    cat(sprintf("k=4 silhouette:    %.3f\n", k4_results$avg_silhouette))
    cat(sprintf("k=4 BSS/TSS:       %.1f%%\n",
                k4_results$kmeans$betweenss / k4_results$kmeans$totss * 100))
    cat(sprintf("k=4 cluster sizes: %s\n\n",
                paste(k4_results$kmeans$size, collapse = ", ")))
  }
  
  cat(sprintf("PC1+PC2: %.1f%% af total varians\n\n", sum(pca_res$var_explained[1:2])))
  
  combined_plot <- (elbow_res$plot + gap_res$plot) /
    (kmeans_res$plot + pca_res$plot) +
    plot_annotation(
      title    = "Clustering Analyse - Rodovre U20",
      subtitle = sprintf("Konsensus: %d segmenter | Elbow, Gap Statistic, Silhouette, PCA",
                         konsensus_k),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40")
      )
    )
  
  list(
    konsensus_k  = konsensus_k,
    anbefalinger = data.frame(Metode = c("Elbow","Gap","Silhouette"), K = anbefalinger),
    combined_plot = combined_plot
  )
}


# --- Hoved-funktion ---

kor_komplet_clustering_analyse <- function(df = NULL, max_k = 10,
                                           gap_B = 50, k_values = c(3, 4, 5, 6),
                                           final_k = 4) {
  
  if (is.null(df)) {
    if (exists("Rodovre_Komplet_Fysio_Profil", envir = .GlobalEnv)) {
      df <- get("Rodovre_Komplet_Fysio_Profil", envir = .GlobalEnv)
    } else {
      stop("Data ikke fundet - angiv df eller load Rodovre_Komplet_Fysio_Profil")
    }
  }
  
  data_prep  <- forbered_clustering_data(df)
  elbow_res  <- elbow_method(data_prep$scaled, max_k = max_k)
  gap_res    <- gap_statistic_method(data_prep$scaled, max_k = max_k, B = gap_B)
  kmeans_res <- kor_kmeans_analyse(data_prep$scaled, k_values = k_values)
  pca_res    <- kor_pca_analyse(data_prep$scaled, data_prep$spiller_info, k = final_k)
  rapport    <- generer_clustering_rapport(elbow_res, gap_res, kmeans_res, pca_res)
  
  assign("Clustering_Plot", rapport$combined_plot, envir = .GlobalEnv)
  
  resultater <- list(
    data_prep     = data_prep,
    elbow         = elbow_res,
    gap           = gap_res,
    kmeans        = kmeans_res,
    pca           = pca_res,
    rapport       = rapport,
    combined_plot = rapport$combined_plot
  )
  
  assign("Clustering_Analyse_Resultater", resultater, envir = .GlobalEnv)
  
  print(rapport$combined_plot)
  
  return(resultater)
}