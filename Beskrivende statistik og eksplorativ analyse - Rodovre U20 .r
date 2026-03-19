# Beskrivende statistik og eksplorativ analyse - Rodovre U20

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(knitr)
  library(kableExtra)
  library(moments)
  library(effectsize)
  library(ggtext)
  library(patchwork)
  library(ggridges)
  library(colorspace)
  library(slider)
})

vis_alle_dataframes()


# --- Farver og tema ---

col_n11          <- "#FF1493"
col_diu          <- "#DC143C"
col_hudl         <- "#FF8C00"
col_neutral_dark <- "#1E3A5F"
col_neutral_mid  <- "#4A7BA7"
col_neutral_light <- "#7BA5D1"
col_neutral_pale <- "#B8D4E8"
col_white        <- "#FFFFFF"
col_light_gray   <- "#E8EFF5"
col_highlight    <- "#FFD700"

gradient_n11  <- colorRampPalette(c("#FFE6F0", "#FFB3D9", col_n11,  darken(col_n11,  0.3)))
gradient_diu  <- colorRampPalette(c("#FFE8E8", "#FFB3B3", col_diu,  darken(col_diu,  0.3)))
gradient_hudl <- colorRampPalette(c("#FFF4E6", "#FFD9B3", col_hudl, darken(col_hudl, 0.3)))

theme_rodovre_ba <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.3), hjust = 0,
                                      margin = margin(b = 8), color = col_neutral_dark),
      plot.subtitle    = element_text(size = rel(0.95), hjust = 0,
                                      margin = margin(b = 12), color = col_neutral_mid),
      plot.caption     = element_text(size = rel(0.8), hjust = 0,
                                      margin = margin(t = 8), color = col_neutral_mid),
      plot.background  = element_rect(fill = col_white, color = NA),
      panel.background = element_rect(fill = col_white, color = NA),
      panel.grid.major = element_line(color = col_light_gray, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(face = "bold", size = rel(1), color = col_neutral_dark),
      axis.text        = element_text(size = rel(0.9), color = col_neutral_dark),
      axis.line        = element_line(color = col_neutral_mid, linewidth = 0.4),
      legend.position  = "top",
      legend.title     = element_text(face = "bold", size = rel(0.95)),
      legend.text      = element_text(size = rel(0.85)),
      strip.text       = element_text(face = "bold", size = rel(1), color = col_neutral_dark),
      strip.background = element_rect(fill = col_neutral_pale, color = NA)
    )
}

theme_set(theme_rodovre_ba())


# --- Hjaelpefunktioner ---

compute_descriptive_stats <- function(x, var_name = "Variable", conf_level = 0.95) {
  x_clean <- x[!is.na(x) & is.finite(x)]
  n       <- length(x_clean)
  
  if (n < 2) {
    return(tibble(Variable = var_name, N = n,
                  Mean = NA_real_, SD = NA_real_, SE = NA_real_,
                  CI_Lower = NA_real_, CI_Upper = NA_real_,
                  Median = NA_real_, IQR = NA_real_,
                  Q25 = NA_real_, Q75 = NA_real_,
                  Min = NA_real_, Max = NA_real_, Range = NA_real_,
                  CV_pct = NA_real_, Skewness = NA_real_, Kurtosis = NA_real_))
  }
  
  m      <- mean(x_clean)
  s      <- sd(x_clean)
  se     <- s / sqrt(n)
  t_crit <- qt((1 + conf_level) / 2, df = n - 1)
  
  tibble(
    Variable = var_name, N = n,
    Mean     = m, SD = s, SE = se,
    CI_Lower = m - t_crit * se,
    CI_Upper = m + t_crit * se,
    Median   = median(x_clean),
    IQR      = IQR(x_clean),
    Q25      = quantile(x_clean, 0.25),
    Q75      = quantile(x_clean, 0.75),
    Min      = min(x_clean),
    Max      = max(x_clean),
    Range    = max(x_clean) - min(x_clean),
    CV_pct   = (s / m) * 100,
    Skewness = moments::skewness(x_clean),
    Kurtosis = moments::kurtosis(x_clean)
  )
}

format_academic_table <- function(df, caption = "", digits = 2) {
  df %>% kable(format = "simple", digits = digits, caption = caption,
               align = c("l", rep("r", ncol(df) - 1)))
}

test_normality <- function(x, var_name = "Variable") {
  x_clean <- x[!is.na(x) & is.finite(x)]
  if (length(x_clean) < 3)
    return(tibble(Variable = var_name, Shapiro_W = NA, Shapiro_p = NA,
                  Interpretation = "Insufficient data"))
  if (length(x_clean) > 5000) x_clean <- sample(x_clean, 5000)
  sw <- shapiro.test(x_clean)
  tibble(
    Variable       = var_name,
    Shapiro_W      = sw$statistic,
    Shapiro_p      = sw$p.value,
    Interpretation = case_when(
      sw$p.value > 0.05 ~ "Normalfordelt (p > 0.05)",
      sw$p.value > 0.01 ~ "Marginal afvigelse (0.01 < p <= 0.05)",
      TRUE              ~ "Signifikant afvigelse (p <= 0.01)"
    )
  )
}

compare_groups <- function(x1, x2, group1_name = "Group 1", group2_name = "Group 2") {
  x1c <- x1[!is.na(x1) & is.finite(x1)]
  x2c <- x2[!is.na(x2) & is.finite(x2)]
  if (length(x1c) < 2 || length(x2c) < 2)
    return(tibble(Comparison = paste(group1_name, "vs", group2_name),
                  Mean_Diff = NA, t_statistic = NA, df = NA,
                  p_value = NA, Cohens_d = NA, Effect_Size = NA))
  tt         <- t.test(x1c, x2c, var.equal = FALSE)
  pooled_sd  <- sqrt(((length(x1c)-1)*var(x1c) + (length(x2c)-1)*var(x2c)) /
                       (length(x1c) + length(x2c) - 2))
  cohens_d   <- (mean(x1c) - mean(x2c)) / pooled_sd
  tibble(
    Comparison  = paste(group1_name, "vs", group2_name),
    Mean_Diff   = mean(x1c) - mean(x2c),
    t_statistic = tt$statistic,
    df          = tt$parameter,
    p_value     = tt$p.value,
    Cohens_d    = cohens_d,
    Effect_Size = case_when(abs(cohens_d) < 0.2 ~ "Neglible",
                            abs(cohens_d) < 0.5 ~ "Small",
                            abs(cohens_d) < 0.8 ~ "Medium",
                            TRUE                ~ "Large")
  )
}


# --- Identificer datasaet ---

identify_primary_data <- function() {
  primary <- list()
  for (c in c("n11_all","all_n11"))
    if (exists(c, envir = .GlobalEnv)) {
      primary$n11 <- get(c, envir = .GlobalEnv)
      cat(sprintf("Next11: '%s'  (%d rk, %d kol)\n", c, nrow(primary$n11), ncol(primary$n11)))
      break
    }
  for (c in c("Rodovre_Komplet_Fysio_Profil","spillerdata","fysio_data"))
    if (exists(c, envir = .GlobalEnv)) {
      primary$diu <- get(c, envir = .GlobalEnv)
      cat(sprintf("DIU:    '%s'  (%d rk, %d kol)\n", c, nrow(primary$diu), ncol(primary$diu)))
      break
    }
  for (c in c("hudl_all","hudl_kampoversigt"))
    if (exists(c, envir = .GlobalEnv)) {
      primary$hudl <- get(c, envir = .GlobalEnv)
      cat(sprintf("Hudl:   '%s'  (%d rk, %d kol)\n", c, nrow(primary$hudl), ncol(primary$hudl)))
      break
    }
  primary
}

data_sources <- identify_primary_data()
if (length(data_sources) == 0)
  stop("Ingen primaere datasaet fundet - indlaes n11_all, Rodovre_Komplet_Fysio_Profil og hudl_all")


# --- Standardiser datastrukturer ---

if (!is.null(data_sources$n11)) {
  n11_data   <- data_sources$n11
  col_mapping <- c("Total Player Load" = "PlayerLoad", "Load/Min" = "LoadPerMin",
                   "Session Date" = "Date", "Spiller_Navn" = "PlayerName")
  for (old in names(col_mapping))
    if (old %in% names(n11_data) && !col_mapping[old] %in% names(n11_data))
      n11_data <- n11_data %>% rename(!!col_mapping[old] := !!old)
  n11_data <- n11_data %>%
    mutate(
      Date       = as.Date(Date),
      PlayerLoad = as.numeric(PlayerLoad),
      Year       = year(Date),
      Month      = month(Date, label = TRUE, abbr = FALSE),
      Week       = week(Date),
      Season_Phase = case_when(
        Month %in% c("maj","juni","juli","august")                                       ~ "Pre-season",
        Month %in% c("september","oktober","november","december","januar","februar","marts") ~ "Competitive",
        TRUE ~ "Other"
      )
    ) %>%
    filter(!is.na(PlayerLoad), !is.na(Date), is.finite(PlayerLoad))
  data_sources$n11 <- n11_data
  cat(sprintf("Next11: %d obs  (%s til %s)\n",
              nrow(n11_data),
              format(min(n11_data$Date), "%d-%b-%Y"),
              format(max(n11_data$Date), "%d-%b-%Y")))
}

if (!is.null(data_sources$diu)) {
  diu_data    <- data_sources$diu
  numeric_cols <- diu_data %>%
    select(where(is.numeric)) %>%
    select(-matches("ID|id|Nr|nummer|age|cm|kg|Troje")) %>%
    names()
  cat(sprintf("DIU: %d spillere, %d numeriske tests\n", nrow(diu_data), length(numeric_cols)))
  data_sources$diu <- diu_data
}

if (!is.null(data_sources$hudl)) {
  hudl_data <- data_sources$hudl
  if (any(str_detect(names(hudl_data), regex("event|type", ignore_case = TRUE)))) {
    event_col <- names(hudl_data)[str_detect(names(hudl_data), regex("event|type", ignore_case = TRUE))][1]
    if (event_col != "EventType")
      hudl_data <- hudl_data %>% rename(EventType = !!sym(event_col))
  }
  cat(sprintf("Hudl: %d events\n", nrow(hudl_data)))
  data_sources$hudl <- hudl_data
}
cat("\n")


# --- Tabel 1: Datasaet oversigt ---

tabel_1_data <- tibble(
  Datasaet      = character(), Kilde = character(),
  Observationer = integer(),   Variabler = integer(),
  Tidsperiode   = character(), Primaer_Metric = character()
)

if (!is.null(data_sources$n11))
  tabel_1_data <- tabel_1_data %>% add_row(
    Datasaet = "Next11 Workload", Kilde = "Next11-AP116",
    Observationer = nrow(data_sources$n11), Variabler = ncol(data_sources$n11),
    Tidsperiode = sprintf("%s til %s",
                          format(min(data_sources$n11$Date), "%b %Y"),
                          format(max(data_sources$n11$Date), "%b %Y")),
    Primaer_Metric = "Player Load (AU)"
  )

if (!is.null(data_sources$diu))
  tabel_1_data <- tabel_1_data %>% add_row(
    Datasaet = "DIU Fysiologisk Profil", Kilde = "Dansk Ishockey Union",
    Observationer = nrow(data_sources$diu), Variabler = ncol(data_sources$diu),
    Tidsperiode = "2024-2025", Primaer_Metric = "3RM Styrke (kg)"
  )

if (!is.null(data_sources$hudl))
  tabel_1_data <- tabel_1_data %>% add_row(
    Datasaet = "Hudl Kampstatistik", Kilde = "Hudl video analysis",
    Observationer = nrow(data_sources$hudl), Variabler = ncol(data_sources$hudl),
    Tidsperiode = "2020-2025", Primaer_Metric = "Event counts"
  )

print(format_academic_table(tabel_1_data,
                            caption = "Tabel 1: Primaere datasaet anvendt i analysen"))
cat("\nNote: AU = Arbitrary Units. Pre-processeret og kvalitetssikret.\n\n")


# --- Sektion A: Next11 workload ---

if (!is.null(data_sources$n11)) {
  n11_data <- data_sources$n11
  
  # Tabel 2: Player Load distribution
  preseason_stats    <- n11_data %>% filter(Season_Phase == "Pre-season")  %>%
    pull(PlayerLoad) %>% compute_descriptive_stats("Pre-season")
  competitive_stats  <- n11_data %>% filter(Season_Phase == "Competitive") %>%
    pull(PlayerLoad) %>% compute_descriptive_stats("Competitive")
  
  tabel_2 <- bind_rows(
    compute_descriptive_stats(n11_data$PlayerLoad, "Player Load (samlet)"),
    preseason_stats, competitive_stats
  ) %>% select(Variable, N, Mean, SD, CV_pct, Median, IQR, CI_Lower, CI_Upper, Skewness, Kurtosis)
  
  print(format_academic_table(tabel_2,
                              caption = "Tabel 2: Deskriptiv statistik for Player Load (Next11)", digits = 1))
  cat("\nNote: CV% = Coefficient of Variation. CI = 95% konfidensinterval.\n")
  cat("Pre-season = maj-aug  |  Competitive = sep-mar\n\n")
  
  # Tabel 3: Pre-season vs Competitive
  preseason_pl   <- n11_data %>% filter(Season_Phase == "Pre-season")  %>% pull(PlayerLoad)
  competitive_pl <- n11_data %>% filter(Season_Phase == "Competitive") %>% pull(PlayerLoad)
  
  print(format_academic_table(
    compare_groups(preseason_pl, competitive_pl, "Pre-season", "Competitive"),
    caption = "Tabel 3: Welch t-test - Pre-season vs Competitive", digits = 3))
  cat("\nNote: Welch t-test (antager ikke ens varianser).\n")
  cat("Cohen's d: <0.2 = neglible, 0.2-0.5 = small, 0.5-0.8 = medium, >0.8 = large (Cohen, 1988).\n\n")
  
  # Tabel 4: Normalitetstest
  tabel_4 <- bind_rows(
    test_normality(n11_data$PlayerLoad, "Player Load (samlet)"),
    test_normality(preseason_pl,        "Pre-season"),
    test_normality(competitive_pl,      "Competitive")
  )
  print(format_academic_table(tabel_4, caption = "Tabel 4: Shapiro-Wilk normalitetstest", digits = 4))
  cat("\nNote: H0 = normalfordelt. Forkast ved p < 0.05. Subsample n=5000 ved store datasaet.\n\n")
  
  # Tabel 5: Ugentlig trendanalyse
  weekly_trends <- n11_data %>%
    mutate(Week_Date = floor_date(Date, "week")) %>%
    group_by(Week_Date, Season_Phase) %>%
    summarise(N = n(), Mean_PL = mean(PlayerLoad, na.rm = TRUE),
              SD_PL = sd(PlayerLoad, na.rm = TRUE), .groups = "drop") %>%
    filter(N >= 3) %>%
    mutate(Week_Number = as.numeric(Week_Date - min(Week_Date)) / 7)
  
  lm_trend   <- lm(Mean_PL ~ Week_Number, data = weekly_trends)
  lm_summary <- summary(lm_trend)
  
  tabel_5 <- tibble(
    Model          = "PL ~ Week",
    Intercept      = coef(lm_trend)[1],
    Slope_per_week = coef(lm_trend)[2],
    R_squared      = lm_summary$r.squared,
    F_statistic    = lm_summary$fstatistic[1],
    p_value        = pf(lm_summary$fstatistic[1], lm_summary$fstatistic[2],
                        lm_summary$fstatistic[3], lower.tail = FALSE)
  )
  print(format_academic_table(tabel_5, caption = "Tabel 5: Linjaer trendanalyse - ugentlig Player Load", digits = 3))
  cat(sprintf("\nPlayer Load aendrer sig med %.2f AU per uge (95%% CI: [%.2f, %.2f]).\n",
              coef(lm_trend)[2], confint(lm_trend)[2,1], confint(lm_trend)[2,2]))
  cat(ifelse(tabel_5$p_value < 0.05,
             "Trenden er statistisk signifikant (p < 0.05).\n\n",
             "Trenden er ikke statistisk signifikant (p >= 0.05).\n\n"))
}


# --- Sektion B: DIU fysiologisk profil ---

if (!is.null(data_sources$diu)) {
  diu_data <- data_sources$diu
  
  # Tabel 6: 3RM styrke
  strength_cols_3rm <- names(diu_data)[str_detect(names(diu_data), "3RM|_2024")]
  strength_cols_3rm <- strength_cols_3rm[str_detect(strength_cols_3rm, "Baenk|Trap|Squat|Press|Dead")]
  
  if (length(strength_cols_3rm) > 0) {
    tabel_6 <- map_dfr(strength_cols_3rm, function(col)
      compute_descriptive_stats(diu_data[[col]],
                                var_name = str_remove(col, "_2024|_3RM") %>% str_replace_all("_", " "))
    ) %>% select(Variable, N, Mean, SD, CV_pct, Median, Min, Max)
    print(format_academic_table(tabel_6, caption = "Tabel 6: 3RM styrke tests (2024)", digits = 1))
    cat("\nNote: 3RM = 3 Repetition Maximum (kg). Ref: Burr et al. (2008).\n\n")
  }
  
  # Tabel 7: Relativ styrke
  rel_cols <- names(diu_data)[str_detect(names(diu_data), "Rel_|BMI|Vaegt|Hojde")]
  if (length(rel_cols) > 0) {
    tabel_7 <- map_dfr(rel_cols, function(col)
      compute_descriptive_stats(diu_data[[col]], var_name = str_replace_all(col, "_", " "))
    ) %>% select(Variable, N, Mean, SD, CV_pct, Median)
    print(format_academic_table(tabel_7, caption = "Tabel 7: Relativ styrke og antropometri", digits = 2))
    cat("\nNote: Relativ styrke = absolut styrke / kropsvaeget. BMI = kg/m^2.\n\n")
  }
  
  # Tabel 8: Ben-asymmetri
  if ("DIU_Leg_Asymmetry_pct" %in% names(diu_data)) {
    asymmetry_cats <- diu_data %>%
      mutate(Risk = case_when(
        is.na(DIU_Leg_Asymmetry_pct)    ~ NA_character_,
        DIU_Leg_Asymmetry_pct < 5       ~ "Lav (<5%)",
        DIU_Leg_Asymmetry_pct <= 10     ~ "Moderat (5-10%)",
        TRUE                            ~ "Hoj (>10%)"
      )) %>%
      count(Risk) %>%
      mutate(Percentage = n / sum(n) * 100)
    
    print(format_academic_table(
      compute_descriptive_stats(diu_data$DIU_Leg_Asymmetry_pct, "Leg Asymmetry (%)") %>%
        select(Variable, N, Mean, SD, Median, Q25, Q75),
      caption = "Tabel 8: Ben-asymmetri", digits = 2))
    print(format_academic_table(asymmetry_cats, caption = "Asymmetri risikokategorier", digits = 1))
    cat("\nNote: Graensevaerdier fra Impellizzeri et al. (2007).\n")
    cat("  <5%   = Fysiologisk normal\n  5-10% = Potentiel opmaerksomhed\n  >10%  = Forhojet skaderisiko (OR 2.5-4.7)\n\n")
  }
}


# --- Sektion C: Hudl kampprastation ---

if (!is.null(data_sources$hudl)) {
  hudl_data <- data_sources$hudl
  
  # Tabel 9: Event distribution
  if ("EventType" %in% names(hudl_data)) {
    tabel_9 <- hudl_data %>%
      count(EventType, sort = TRUE) %>%
      mutate(Percentage = n / sum(n) * 100, Cumulative_Pct = cumsum(Percentage)) %>%
      head(15)
    print(format_academic_table(tabel_9, caption = "Tabel 9: Top 15 event-typer (Hudl)", digits = 1))
    cat(sprintf("\nTotal events: %s  |  Unikke typer: %d  |  Top 15 udgaer %.1f%%\n\n",
                format(nrow(hudl_data), big.mark = "."),
                n_distinct(hudl_data$EventType),
                tabel_9$Cumulative_Pct[15]))
  }
  
  # Tabel 10: Kamp-niveau
  if ("GameDate" %in% names(hudl_data) || any(str_detect(names(hudl_data), "Game|Match"))) {
    game_col     <- names(hudl_data)[str_detect(names(hudl_data), "Game|Match")][1]
    game_summary <- hudl_data %>%
      group_by(!!sym(game_col)) %>%
      summarise(Total_Events = n(), Unique_Event_Types = n_distinct(EventType), .groups = "drop")
    print(format_academic_table(
      compute_descriptive_stats(game_summary$Total_Events, "Events per kamp") %>%
        select(Variable, N, Mean, SD, Median, Min, Max),
      caption = "Tabel 10: Kamp-niveau event statistik", digits = 1))
    cat(sprintf("\nKampe i alt: %d  |  Gns. event-typer per kamp: %.1f\n\n",
                nrow(game_summary), mean(game_summary$Unique_Event_Types, na.rm = TRUE)))
  }
}


# --- Visualiseringer ---

# Viz 1: Temporal workload
if (!is.null(data_sources$n11)) {
  n11_data     <- data_sources$n11
  overall_mean <- mean(n11_data$PlayerLoad, na.rm = TRUE)
  overall_sd   <- sd(n11_data$PlayerLoad,   na.rm = TRUE)
  
  weekly_data <- n11_data %>%
    mutate(Week_Date = floor_date(Date, "week")) %>%
    group_by(Week_Date, Season_Phase) %>%
    summarise(Mean_PL = mean(PlayerLoad, na.rm = TRUE),
              SD_PL   = sd(PlayerLoad,   na.rm = TRUE),
              N       = n(), .groups = "drop") %>%
    mutate(SE = SD_PL / sqrt(N),
           CI_Lower = Mean_PL - qt(0.975, N-1) * SE,
           CI_Upper = Mean_PL + qt(0.975, N-1) * SE) %>%
    filter(N >= 3)
  
  viz_1 <- weekly_data %>%
    ggplot(aes(x = Week_Date, y = Mean_PL, color = Season_Phase)) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = overall_mean - overall_sd, ymax = overall_mean + overall_sd,
             fill = col_neutral_pale, alpha = 0.4) +
    geom_hline(yintercept = overall_mean, linetype = "dashed",
               color = col_neutral_dark, linewidth = 0.7) +
    geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper, fill = Season_Phase),
                alpha = 0.2, color = NA) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.3) +
    geom_line(linewidth = 1, alpha = 0.7) +
    geom_point(aes(size = N), alpha = 0.8, shape = 21, fill = col_white, stroke = 1) +
    annotate("text", x = min(weekly_data$Week_Date), y = overall_mean,
             label = sprintf("Saesongennemsnit: %.0f AU", overall_mean),
             hjust = 0, vjust = -0.5, size = 3.2, color = col_neutral_dark, fontface = "italic") +
    scale_color_manual(values = c("Pre-season" = col_hudl, "Competitive" = col_n11), name = "Saesonfase") +
    scale_fill_manual( values = c("Pre-season" = col_hudl, "Competitive" = col_n11), guide = "none") +
    scale_size_continuous(name = "Sessioner/uge", range = c(2, 5)) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.1))) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
    labs(title    = "Temporal Workload Pattern - Next11 Player Load",
         subtitle = str_wrap("Ugentlig mean +/- 95% CI med LOESS trendlinje. Graa zone = +/-1 SD fra saesongennemsnit.", width = 95),
         x = "Dato (ugebasis)", y = "Player Load (AU)",
         caption = "Kilde: Next11-AP116, Rodovre U20. Uger med min. 3 sessioner. LOESS smoothing.") +
    theme(legend.position = "top", legend.box = "horizontal",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(viz_1)
}

# Viz 2: Ridgeline distribution
if (!is.null(data_sources$n11)) {
  n11_data    <- data_sources$n11
  monthly_dist <- n11_data %>%
    filter(!is.na(PlayerLoad), !is.na(Month)) %>%
    mutate(Month = factor(Month, levels = rev(unique(Month))))
  monthly_stats <- monthly_dist %>%
    group_by(Month) %>%
    summarise(Mean = mean(PlayerLoad), Median = median(PlayerLoad), N = n(), .groups = "drop")
  
  viz_2 <- monthly_dist %>%
    ggplot(aes(x = PlayerLoad, y = Month, fill = Month)) +
    ggridges::geom_density_ridges(
      aes(point_color = Month, point_fill = Month),
      alpha = 0.7, scale = 2.2, rel_min_height = 0.01, color = col_white, size = 0.6,
      jittered_points = TRUE, point_shape = "|", point_size = 2, point_alpha = 0.3,
      quantile_lines = TRUE, quantiles = 2
    ) +
    geom_point(data = monthly_stats, aes(x = Mean, y = Month),
               color = col_neutral_dark, size = 3, shape = 23,
               fill = col_highlight, stroke = 1.2, inherit.aes = FALSE) +
    geom_text(data = monthly_stats,
              aes(x = max(monthly_dist$PlayerLoad) * 1.05, y = Month,
                  label = paste0("n=", N)),
              size = 3, hjust = 0, color = col_neutral_dark, fontface = "bold", inherit.aes = FALSE) +
    scale_fill_manual(values = gradient_n11(12), guide = "none") +
    scale_color_manual(values = gradient_n11(12), guide = "none") +
    scale_point_color_manual(values = gradient_n11(12), guide = "none") +
    scale_point_fill_manual( values = gradient_n11(12), guide = "none") +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.15))) +
    labs(title    = "Maanedlig Player Load distribution",
         subtitle = "Density ridgeline med median (hvid linje) og mean (gul diamant).",
         x = "Player Load (AU)", y = "Maaned",
         caption = "Kilde: Next11-AP116, Rodovre U20.") +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(face = "bold"))
  
  print(viz_2)
}

# Viz 3: DIU Z-score heatmap
if (!is.null(data_sources$diu)) {
  diu_data  <- data_sources$diu
  test_cols <- names(diu_data)[sapply(diu_data, is.numeric)]
  test_cols <- test_cols[!str_detect(test_cols, "ID|id|Nr|nummer|age|cm|kg|BMI|Troje")]
  
  if (length(test_cols) >= 3) {
    diu_zscores <- diu_data %>%
      mutate(across(all_of(test_cols), list(Z = ~scale(.)[,1]), .names = "{.col}_Z"))
    zscore_cols <- names(diu_zscores)[str_detect(names(diu_zscores), "_Z$")]
    zscore_variability <- diu_zscores %>%
      summarise(across(all_of(zscore_cols), sd, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "Test", values_to = "SD") %>%
      arrange(desc(SD)) %>% slice_head(n = 8) %>% pull(Test)
    
    viz_3 <- diu_zscores %>%
      mutate(PlayerID = row_number(),
             Total_Z  = rowMeans(across(all_of(zscore_variability)), na.rm = TRUE)) %>%
      arrange(desc(Total_Z)) %>%
      select(PlayerID, all_of(zscore_variability)) %>%
      pivot_longer(-PlayerID, names_to = "Test", values_to = "Zscore") %>%
      mutate(Test = str_remove(Test, "_Z$") %>% str_remove("_2024|_2025") %>%
               str_replace_all("_", " ")) %>%
      filter(!is.na(Zscore)) %>%
      ggplot(aes(x = Test, y = factor(PlayerID), fill = Zscore)) +
      geom_tile(color = col_white, linewidth = 0.6) +
      scale_fill_gradient2(low = darken(col_diu, 0.3), mid = col_white,
                           high = lighten(col_diu, 0.2), midpoint = 0,
                           limits = c(-3, 3), name = "Z-score", breaks = seq(-3, 3, 1),
                           guide = guide_colorbar(barwidth = 1, barheight = 12, title.position = "top")) +
      labs(title    = "Fysiologisk profil heatmap (DIU)",
           subtitle = sprintf("Z-score for %d mest variable tests. Rangeret efter samlet performance.",
                              length(zscore_variability)),
           x = "Fysiologisk test", y = "Spillere (rangeret)",
           caption = "Kilde: DIU tests, Rodovre U20. Z-scores relativt til holdgennemsnit.") +
      coord_fixed(ratio = 0.4) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
            panel.grid = element_blank(), legend.position = "right")
    
    print(viz_3)
  }
}

# Viz 4: ACWR risiko-distribution
if (!is.null(data_sources$n11)) {
  n11_data  <- data_sources$n11
  acwr_data <- n11_data %>%
    arrange(PlayerName, Date) %>%
    group_by(PlayerName) %>%
    mutate(Acute_PL   = slide_sum(PlayerLoad, before = 6,  complete = TRUE),
           Chronic_PL = slide_sum(PlayerLoad, before = 27, complete = TRUE),
           ACWR       = Acute_PL / Chronic_PL) %>%
    ungroup() %>%
    filter(!is.na(ACWR), is.finite(ACWR), ACWR > 0, ACWR < 5) %>%
    mutate(ACWR_Category = factor(case_when(
      ACWR < 0.8  ~ "Underload (<0.8)",
      ACWR <= 1.3 ~ "Optimal (0.8-1.3)",
      ACWR <= 1.5 ~ "Moderat risiko (1.3-1.5)",
      TRUE        ~ "Hoj risiko (>1.5)"
    ), levels = c("Underload (<0.8)","Optimal (0.8-1.3)",
                  "Moderat risiko (1.3-1.5)","Hoj risiko (>1.5)")))
  
  if (nrow(acwr_data) > 0) {
    zone_stats <- acwr_data %>% count(ACWR_Category) %>% mutate(Pct = n / sum(n) * 100)
    
    viz_4 <- acwr_data %>%
      ggplot(aes(x = ACWR)) +
      annotate("rect", xmin = 0.8, xmax = 1.3, ymin = 0, ymax = Inf,
               fill = col_neutral_light, alpha = 0.25) +
      annotate("rect", xmin = 1.5, xmax = Inf, ymin = 0, ymax = Inf,
               fill = col_diu, alpha = 0.2) +
      annotate("text", x = 1.05, y = Inf, label = "OPTIMAL", vjust = 1.5, size = 4,
               fontface = "bold", color = col_neutral_dark, alpha = 0.7) +
      annotate("text", x = 2, y = Inf, label = "HOJ RISIKO", vjust = 1.5, size = 4,
               fontface = "bold", color = col_diu, alpha = 0.9) +
      geom_histogram(aes(fill = ACWR_Category), bins = 50, alpha = 0.8,
                     color = col_white, linewidth = 0.3) +
      geom_density(aes(y = after_stat(count)), color = col_neutral_dark,
                   linewidth = 1.2, alpha = 0.8) +
      geom_vline(xintercept = c(0.8, 1.3, 1.5), linetype = "dashed",
                 color = c(col_neutral_mid, col_neutral_dark, col_diu), linewidth = 1) +
      geom_vline(xintercept = median(acwr_data$ACWR), linetype = "solid",
                 color = col_neutral_dark, linewidth = 1.3) +
      annotate("text", x = median(acwr_data$ACWR), y = 0,
               label = sprintf("Median: %.2f", median(acwr_data$ACWR)),
               vjust = -0.5, hjust = -0.1, size = 3.5, fontface = "bold", color = col_neutral_dark) +
      scale_fill_manual(values = c("Underload (<0.8)"      = lighten(col_neutral_mid, 0.3),
                                   "Optimal (0.8-1.3)"     = col_neutral_mid,
                                   "Moderat risiko (1.3-1.5)" = col_hudl,
                                   "Hoj risiko (>1.5)"     = col_diu), name = "ACWR Zone") +
      scale_x_continuous(breaks = seq(0, 3, 0.25), limits = c(0, 3), expand = c(0, 0)) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
      labs(title    = "ACWR distribution og risikostratifikation",
           subtitle = str_wrap(sprintf("7-dages/28-dages ratio. %.1f%% optimal zone, %.1f%% hoj risiko. Median = %.2f.",
                                       zone_stats$Pct[zone_stats$ACWR_Category == "Optimal (0.8-1.3)"],
                                       zone_stats$Pct[zone_stats$ACWR_Category == "Hoj risiko (>1.5)"],
                                       median(acwr_data$ACWR)), width = 100),
           x = "ACWR", y = "Antal observationer",
           caption = "Kilde: Next11, Rodovre U20. Optimal zone 0.8-1.3 (Gabbett, 2016).") +
      theme(legend.position = "right", legend.key.height = unit(1.3, "lines"))
    
    print(viz_4)
  }
}

# Viz 5: Korrelationsmatrix
if (!is.null(data_sources$n11)) {
  n11_data <- data_sources$n11
  cor_vars <- n11_data %>%
    select(where(is.numeric)) %>% select(-matches("ID|id|Nr")) %>%
    select(where(~sum(!is.na(.)) > 100)) %>% names()
  
  if (length(cor_vars) >= 2) {
    cor_data <- n11_data %>% select(all_of(cor_vars)) %>% drop_na()
    if (nrow(cor_data) > 10) {
      cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
      cor_long   <- as.data.frame(as.table(cor_matrix)) %>%
        rename(Correlation = Freq) %>%
        mutate(Var1 = str_trunc(Var1, 15), Var2 = str_trunc(Var2, 15))
      
      viz_5 <- cor_long %>%
        ggplot(aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile(color = col_white, linewidth = 0.8) +
        geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3.5, fontface = "bold",
                  color = ifelse(abs(cor_long$Correlation) > 0.5, col_white, col_neutral_dark)) +
        scale_fill_gradient2(low = col_neutral_dark, mid = col_white, high = col_n11,
                             midpoint = 0, limits = c(-1, 1), breaks = seq(-1, 1, 0.25),
                             name = "Pearson's r",
                             guide = guide_colorbar(barwidth = 12, barheight = 1,
                                                    title.position = "top", title.hjust = 0.5)) +
        labs(title    = "Korrelationsmatrix - workload metrics",
             subtitle = "Pearson korrelationer mellem Next11 variable.",
             caption  = "Kilde: Next11-AP116, Rodovre U20. Pairwise complete obs.") +
        coord_fixed() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              axis.title = element_blank(), panel.grid = element_blank(),
              legend.position = "top")
      
      print(viz_5)
    }
  }
}

# Viz 6: Hudl event distribution
if (!is.null(data_sources$hudl) && "EventType" %in% names(data_sources$hudl)) {
  hudl_data    <- data_sources$hudl
  event_summary <- hudl_data %>%
    count(EventType, sort = TRUE) %>% filter(n >= 100) %>%
    mutate(EventType = fct_reorder(EventType, n), Pct = n / sum(n) * 100) %>%
    head(12)
  
  viz_6 <- event_summary %>%
    ggplot(aes(x = n, y = EventType, fill = n)) +
    geom_col(alpha = 0.85, color = col_white, linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.1f%%", Pct)), hjust = -0.15, size = 3.3,
              fontface = "bold", color = col_neutral_dark) +
    scale_fill_gradientn(colors = gradient_hudl(100), guide = "none") +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
    labs(title    = "Hudl event type distribution",
         subtitle = sprintf("Top 12 event-typer med min. 100 registreringer (n = %s total)",
                            format(sum(event_summary$n), big.mark = ".")),
         x = "Antal events", y = "Event type",
         caption = "Kilde: Hudl kampstatistik, Rodovre U20 (2020-2023)") +
    theme(panel.grid.major.y = element_blank())
  
  print(viz_6)
}


# --- Noegletal ---

key_findings <- list()

if (!is.null(data_sources$n11)) {
  n11_data <- data_sources$n11
  pre_mean  <- mean(n11_data$PlayerLoad[n11_data$Season_Phase == "Pre-season"],  na.rm = TRUE)
  comp_mean <- mean(n11_data$PlayerLoad[n11_data$Season_Phase == "Competitive"], na.rm = TRUE)
  key_findings$n11 <- list(
    total_sessions     = length(unique(n11_data$Date)),
    total_observations = nrow(n11_data),
    mean_pl            = mean(n11_data$PlayerLoad, na.rm = TRUE),
    sd_pl              = sd(n11_data$PlayerLoad,   na.rm = TRUE),
    cv_pct             = sd(n11_data$PlayerLoad, na.rm = TRUE) / mean(n11_data$PlayerLoad, na.rm = TRUE) * 100,
    phase_diff_pct     = (comp_mean - pre_mean) / pre_mean * 100
  )
  cat(sprintf("Next11:  %d sessioner  |  %s obs  |  Mean PL %.1f +/- %.1f AU (CV %.1f%%)\n",
              key_findings$n11$total_sessions,
              format(key_findings$n11$total_observations, big.mark = "."),
              key_findings$n11$mean_pl, key_findings$n11$sd_pl, key_findings$n11$cv_pct))
  cat(sprintf("         Fase-forskel: %.1f%% (competitive vs pre-season)\n\n",
              abs(key_findings$n11$phase_diff_pct)))
}

if (!is.null(data_sources$diu)) {
  diu_data <- data_sources$diu
  cat(sprintf("DIU:     %d spillere  |  %d numeriske tests\n", nrow(diu_data), sum(sapply(diu_data, is.numeric))))
  if ("DIU_Leg_Asymmetry_pct" %in% names(diu_data))
    cat(sprintf("         Hoj-risiko asymmetri (>10%%): %.1f%%\n\n",
                mean(diu_data$DIU_Leg_Asymmetry_pct > 10, na.rm = TRUE) * 100))
}

if (!is.null(data_sources$hudl))
  cat(sprintf("Hudl:    %s events  |  %d unikke event-typer\n\n",
              format(nrow(data_sources$hudl), big.mark = "."),
              n_distinct(data_sources$hudl$EventType)))

if (exists("acwr_data") && nrow(acwr_data) > 0)
  cat(sprintf("ACWR:    Median %.2f  |  Optimal zone %.1f%%  |  Hoj risiko %.1f%%\n\n",
              median(acwr_data$ACWR, na.rm = TRUE),
              mean(acwr_data$ACWR >= 0.8 & acwr_data$ACWR <= 1.3, na.rm = TRUE) * 100,
              mean(acwr_data$ACWR > 1.5, na.rm = TRUE) * 100))