# N11 spillerliste og segment-oversigt

library(tidyverse)


# --- Indlaes N11 data ---

n11_dfs <- ls(pattern = "^rod(24|25)_\\d{8}$", envir = .GlobalEnv)

n11_combined <- data.frame()
for (df_name in n11_dfs) {
  df <- get(df_name, envir = .GlobalEnv)
  if ("Spiller_Navn" %in% names(df) && "Total Player Load" %in% names(df)) {
    n11_combined <- bind_rows(n11_combined, df)
  }
}

n11_metrics <- n11_combined %>%
  group_by(Spiller_Navn) %>%
  summarise(
    N11_Antal_Sessioner      = n(),
    N11_Total_Load           = sum(`Total Player Load`,    na.rm = TRUE),
    N11_Avg_Load_Per_Session = mean(`Total Player Load`,   na.rm = TRUE),
    N11_Max_Load             = max(`Total Player Load`,    na.rm = TRUE),
    N11_Avg_Load_Per_Min     = mean(`Load/Min`,            na.rm = TRUE),
    N11_Total_Explosive_Sec  = sum(`Explosive [seconds]`,  na.rm = TRUE),
    N11_Total_VeryHigh_Sec   = sum(`Very High [seconds]`,  na.rm = TRUE),
    N11_Total_High_Sec       = sum(`High [seconds]`,       na.rm = TRUE),
    N11_Variability_CV       = sd(`Total Player Load`, na.rm = TRUE) /
      mean(`Total Player Load`, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat(sprintf("N11 dataframes: %d  |  Spillere med N11 data: %d\n\n",
            length(n11_dfs), nrow(n11_metrics)))


# --- Merge med segment-data ---

if (exists("N11_Sammenligning_Rigtige", envir = .GlobalEnv)) {
  
  sammenligning <- get("N11_Sammenligning_Rigtige", envir = .GlobalEnv)
  
  n11_liste <- sammenligning %>%
    select(Spiller_Nr, Spiller_Navn,
           Segment_Uden_N11 = Segment_UDEN,
           Segment_Med_N11  = Segment_MED,
           Segment_Aendring) %>%
    left_join(n11_metrics, by = "Spiller_Navn") %>%
    arrange(Spiller_Nr)
  
} else {
  
  fysio_data <- Rodovre_Komplet_Fysio_Profil %>%
    group_by(Spiller_Nr) %>% slice(1) %>% ungroup()
  
  n11_liste <- fysio_data %>%
    filter(Spiller_Navn %in% n11_metrics$Spiller_Navn) %>%
    select(Spiller_Nr, Spiller_Navn, Troje_Nr) %>%
    left_join(n11_metrics, by = "Spiller_Navn") %>%
    arrange(Spiller_Nr)
  
  cat("Segment-data ikke fundet - koen 004_1_2segemntering scriptet forst\n\n")
}


# --- Konsoloutput ---

cat(sprintf("Spillere med N11 data: %d\n\n", nrow(n11_liste)))

if ("Segment_Uden_N11" %in% names(n11_liste)) {
  for (i in seq_len(nrow(n11_liste))) {
    s      <- n11_liste[i, ]
    skift  <- if (!is.na(s$Segment_Aendring) && s$Segment_Aendring) "*" else " "
    cat(sprintf("%s #%-3s  %-25s\n", skift, s$Spiller_Nr, s$Spiller_Navn))
    cat(sprintf("      Uden N11: %-18s  Med N11: %s\n",
                s$Segment_Uden_N11, s$Segment_Med_N11))
    cat(sprintf("      %d sessioner  Avg: %.0f  Max: %.0f  CV: %.0f%%\n\n",
                s$N11_Antal_Sessioner, s$N11_Avg_Load_Per_Session,
                s$N11_Max_Load, s$N11_Variability_CV))
  }
} else {
  for (i in seq_len(nrow(n11_liste))) {
    s <- n11_liste[i, ]
    cat(sprintf("#%-3s  %-25s\n", s$Spiller_Nr, s$Spiller_Navn))
    cat(sprintf("      %d sessioner  Avg: %.0f  Max: %.0f  CV: %.0f%%\n\n",
                s$N11_Antal_Sessioner, s$N11_Avg_Load_Per_Session,
                s$N11_Max_Load, s$N11_Variability_CV))
  }
}


# --- Statistik ---

if ("Segment_Aendring" %in% names(n11_liste)) {
  n_skift   <- sum(n11_liste$Segment_Aendring, na.rm = TRUE)
  n_uaendret <- nrow(n11_liste) - n_skift
  cat(sprintf("Skiftet segment: %d (%.0f%%)  |  Uaendret: %d (%.0f%%)\n\n",
              n_skift,    n_skift    / nrow(n11_liste) * 100,
              n_uaendret, n_uaendret / nrow(n11_liste) * 100))
  
  skift <- n11_liste %>% filter(Segment_Aendring)
  if (nrow(skift) > 0) {
    cat("Spillere der skiftede segment:\n")
    for (i in seq_len(nrow(skift))) {
      cat(sprintf("  %-25s  %s -> %s\n",
                  skift$Spiller_Navn[i],
                  skift$Segment_Uden_N11[i],
                  skift$Segment_Med_N11[i]))
    }
    cat("\n")
  }
}

cat(sprintf("Gns. sessioner:      %.1f\n", mean(n11_liste$N11_Antal_Sessioner)))
cat(sprintf("Gns. load/session:   %.0f\n", mean(n11_liste$N11_Avg_Load_Per_Session)))
cat(sprintf("Hoejeste max load:   %.0f  (%s)\n",
            max(n11_liste$N11_Max_Load),
            n11_liste$Spiller_Navn[which.max(n11_liste$N11_Max_Load)]))
cat(sprintf("Laveste max load:    %.0f  (%s)\n\n",
            min(n11_liste$N11_Max_Load),
            n11_liste$Spiller_Navn[which.min(n11_liste$N11_Max_Load)]))


# --- Gem output ---

write_csv(n11_liste, "N11_Spillere_Liste.csv")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  library(openxlsx)
  
  wb <- createWorkbook()
  addWorksheet(wb, "N11 Spillere")
  
  header_style <- createStyle(
    fontSize       = 12,
    fontColour     = "white",
    fgFill         = "#1565C0",
    halign         = "center",
    textDecoration = "bold",
    border         = "bottom"
  )
  
  writeData(wb, "N11 Spillere", n11_liste, headerStyle = header_style)
  
  if ("Segment_Aendring" %in% names(n11_liste)) {
    skift_style <- createStyle(fgFill = "#FFF3CD", border = "bottom")
    skift_rows  <- which(n11_liste$Segment_Aendring) + 1
    if (length(skift_rows) > 0) {
      addStyle(wb, "N11 Spillere", skift_style,
               rows = skift_rows, cols = 1:ncol(n11_liste), gridExpand = TRUE)
    }
  }
  
  setColWidths(wb, "N11 Spillere", cols = 1:ncol(n11_liste), widths = "auto")
  saveWorkbook(wb, "N11_Spillere_Liste.xlsx", overwrite = TRUE)
}

assign("N11_Spillere_Komplet_Liste", n11_liste, envir = .GlobalEnv)


# --- Tjek matching mellem N11 og fysio ---

fysio_data <- Rodovre_Komplet_Fysio_Profil %>%
  group_by(Spiller_Nr) %>% slice(1) %>% ungroup()

n11_spillere <- n11_combined %>%
  distinct(Spiller_Navn) %>%
  arrange(Spiller_Navn)

n11_i_fysio   <- n11_spillere %>%
  mutate(I_Fysio = Spiller_Navn %in% fysio_data$Spiller_Navn)

n11_med_fysio  <- sum(n11_i_fysio$I_Fysio)
n11_uden_fysio <- sum(!n11_i_fysio$I_Fysio)
fysio_uden_n11 <- fysio_data %>%
  filter(!Spiller_Navn %in% n11_spillere$Spiller_Navn) %>% nrow()

cat(sprintf("N11 spillere i alt:          %d\n", nrow(n11_spillere)))
cat(sprintf("Matches med fysio:           %d\n", n11_med_fysio))
cat(sprintf("N11 uden fysio-match:        %d\n", n11_uden_fysio))
cat(sprintf("Fysio spillere uden N11:     %d\n\n", fysio_uden_n11))

if (n11_uden_fysio > 0) {
  cat("Spillere i N11 uden fysio-match:\n")
  manglende <- n11_i_fysio %>% filter(!I_Fysio)
  for (i in seq_len(nrow(manglende))) {
    cat(sprintf("  %s\n", manglende$Spiller_Navn[i]))
  }
  cat("\n")
}

cat("Spillere i N11 data:\n")
for (i in seq_len(nrow(n11_spillere))) {
  cat(sprintf("  %2d.  %s\n", i, n11_spillere$Spiller_Navn[i]))
}