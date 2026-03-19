# Tidslinje og overlap-analyse - Rodovre U20

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

BRAND_FARVER <- c(
  "Fysiologisk Profil (DIU)" = "#DC143C",
  "Next11 Workload"          = "#FF1493",
  "Hudl Kampstatistik"       = "#FF8C00"
)


# --- Tilfoej Kamp_Dato til Hudl dataframes ---

udtaek_hudl_dato <- function(df_navn) {
  dato_match <- str_extract(df_navn, "\\d{2}_\\d{2}_\\d{4}")
  if (!is.na(dato_match)) {
    return(dmy(str_replace_all(dato_match, "_", "/")))
  }
  return(NA)
}

hudl_dfs <- ls(pattern = "^hudl_.*U20", envir = .GlobalEnv)
for (df_navn in hudl_dfs) {
  df <- get(df_navn, envir = .GlobalEnv)
  df$Kamp_Dato <- udtaek_hudl_dato(df_navn)
  assign(df_navn, df, envir = .GlobalEnv)
}


# --- Udtaek tidsperioder ---

n11_dfs    <- ls(pattern = "^rod(24|25)_", envir = .GlobalEnv)
n11_datoer <- map_dfr(n11_dfs, ~{
  df <- get(.x, envir = .GlobalEnv)
  if ("dato" %in% names(df)) tibble(Dato = ymd(df$dato))
}) %>% pull(Dato) %>% na.omit() %>% as.Date(origin = "1970-01-01")

hudl_datoer <- map_dfr(hudl_dfs, ~{
  df <- get(.x, envir = .GlobalEnv)
  if ("Kamp_Dato" %in% names(df)) tibble(Dato = unique(as.Date(df$Kamp_Dato)))
}) %>% pull(Dato) %>% na.omit()

n11_tidslinje <- tibble(
  Datakilde        = "Next11 Workload",
  Forste_dato      = min(n11_datoer),
  Seneste_dato     = max(n11_datoer),
  Antal_dataframes = length(n11_dfs)
)

hudl_tidslinje <- tibble(
  Datakilde        = "Hudl Kampstatistik",
  Forste_dato      = min(hudl_datoer),
  Seneste_dato     = max(hudl_datoer),
  Antal_dataframes = length(hudl_dfs)
)

diu_tidslinje <- tibble(
  Datakilde        = "Fysiologisk Profil (DIU)",
  Forste_dato      = as.Date("2025-01-01"),
  Seneste_dato     = as.Date("2025-12-31"),
  Antal_dataframes = 1
)

cat(sprintf("Next11: %s til %s (%d dataframes)\n",
            format(n11_tidslinje$Forste_dato,  "%d-%m-%Y"),
            format(n11_tidslinje$Seneste_dato, "%d-%m-%Y"),
            n11_tidslinje$Antal_dataframes))
cat(sprintf("Hudl:   %s til %s (%d dataframes)\n",
            format(hudl_tidslinje$Forste_dato,  "%d-%m-%Y"),
            format(hudl_tidslinje$Seneste_dato, "%d-%m-%Y"),
            hudl_tidslinje$Antal_dataframes))
cat("DIU:    2025 (fysiologiske tests)\n\n")


# --- Plot 1: N11 + DIU tidslinje ---

n11_diu_data <- bind_rows(n11_tidslinje, diu_tidslinje) %>%
  arrange(Forste_dato) %>%
  mutate(y_pos = row_number())

n11_overlap    <- max(n11_diu_data$Forste_dato)
n11_slut       <- min(n11_diu_data$Seneste_dato)
overlap_maaneder <- round(as.numeric(difftime(n11_slut, n11_overlap, units = "days")) / 30.44, 1)

p_n11_diu <- ggplot(n11_diu_data, aes(y = reorder(Datakilde, -y_pos))) +
  geom_segment(aes(x = Forste_dato, xend = Seneste_dato,
                   yend = reorder(Datakilde, -y_pos), color = Datakilde),
               linewidth = 10, alpha = 0.85) +
  geom_point(aes(x = Forste_dato,  color = Datakilde), size = 6, shape = 16) +
  geom_point(aes(x = Seneste_dato, color = Datakilde), size = 6, shape = 17) +
  {if (n11_overlap <= n11_slut)
    annotate("rect", xmin = n11_overlap, xmax = n11_slut,
             ymin = 0.5, ymax = 2.5, alpha = 0.12, fill = "gray40")} +
  scale_color_manual(values = BRAND_FARVER) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y",
               expand = expansion(mult = 0.05),
               limits = c(as.Date("2024-04-01"), as.Date("2026-01-31"))) +
  labs(title    = "Tidslinje: Next11 Workload og DIU Fysiologisk Profil",
       subtitle = sprintf("Overlap: %s til %s (%.1f maaneder)",
                          format(n11_overlap, "%b %Y"),
                          format(n11_slut,    "%b %Y"),
                          overlap_maaneder),
       x = NULL, y = NULL,
       caption = "Cirkel = Start  Trekant = Slut  |  Graa zone = Overlap") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle      = element_text(size = 11, color = "gray30", hjust = 0),
    axis.text.y        = element_text(size = 13, face = "bold"),
    axis.text.x        = element_text(size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption       = element_text(hjust = 0, size = 9, color = "gray50"),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

print(p_n11_diu)


# --- Plot 2: Hudl tidslinje ---

p_hudl <- ggplot(hudl_tidslinje, aes(y = Datakilde)) +
  geom_segment(aes(x = Forste_dato, xend = Seneste_dato, yend = Datakilde),
               color = BRAND_FARVER["Hudl Kampstatistik"],
               linewidth = 10, alpha = 0.85) +
  geom_point(aes(x = Forste_dato),  color = BRAND_FARVER["Hudl Kampstatistik"],
             size = 6, shape = 16) +
  geom_point(aes(x = Seneste_dato), color = BRAND_FARVER["Hudl Kampstatistik"],
             size = 6, shape = 17) +
  annotate("segment",
           x    = as.Date(c("2019-09-01","2020-09-01","2021-09-01",
                            "2022-09-01","2023-09-01","2024-09-01")),
           xend = as.Date(c("2019-09-01","2020-09-01","2021-09-01",
                            "2022-09-01","2023-09-01","2024-09-01")),
           y = 0.7, yend = 1.3, linetype = "dashed", color = "gray40", alpha = 0.5) +
  annotate("text",
           x = as.Date(c("2020-01-01","2021-01-01","2022-01-01",
                         "2023-01-01","2024-01-01","2025-01-01")),
           y = 1.4,
           label = c("19/20","20/21","21/22","22/23","23/24","24/25"),
           size = 3, color = "gray40", fontface = "bold") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
               expand = expansion(mult = 0.02)) +
  labs(title    = "Tidslinje: Hudl Kampstatistik",
       subtitle = "TalentLiga kampdata (2019-2025) | 79 kampe fordelt over 6 saesoner",
       x = NULL, y = NULL,
       caption = "Cirkel = Forste kamp  Trekant = Seneste kamp  |  Stiplede linjer = Saesonstart") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle      = element_text(size = 11, color = "gray30", hjust = 0),
    axis.text.y        = element_text(size = 13, face = "bold"),
    axis.text.x        = element_text(size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption       = element_text(hjust = 0, size = 9, color = "gray50"),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

print(p_hudl)


# --- Plot 3: N11 maanedlig fordeling ---

n11_monthly <- tibble(Dato = n11_datoer) %>%
  mutate(Aar_Maaned = floor_date(Dato, "month")) %>%
  count(Aar_Maaned)

p_n11_monthly <- ggplot(n11_monthly, aes(x = Aar_Maaned, y = n)) +
  geom_col(fill = BRAND_FARVER["Next11 Workload"], alpha = 0.85, width = 25) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5,
            color = "gray30", fontface = "bold") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y",
               expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     breaks = seq(0, max(n11_monthly$n) + 200, 200)) +
  labs(title    = "Next11 Workload - maanedlig fordeling",
       subtitle = sprintf("Trainingssessioner per maaned | Total: %d sessioner",
                          sum(n11_monthly$n)),
       x = NULL, y = "Antal sessioner") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle      = element_text(size = 11, color = "gray30", hjust = 0),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

print(p_n11_monthly)


# --- Plot 4: Hudl maanedlig fordeling ---

hudl_monthly <- tibble(Dato = hudl_datoer) %>%
  mutate(Aar_Maaned = floor_date(Dato, "month")) %>%
  count(Aar_Maaned)

saeson_start <- as.Date(c("2019-09-01","2020-09-01","2021-09-01",
                          "2022-09-01","2023-09-01","2024-09-01"))

p_hudl_monthly <- ggplot(hudl_monthly, aes(x = Aar_Maaned, y = n)) +
  geom_vline(xintercept = as.numeric(saeson_start),
             linetype = "dashed", color = "gray40", alpha = 0.5, linewidth = 0.8) +
  geom_col(fill = BRAND_FARVER["Hudl Kampstatistik"], alpha = 0.85, width = 25) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5,
            color = "gray30", fontface = "bold") +
  annotate("text",
           x = saeson_start + 120,
           y = max(hudl_monthly$n) * 0.95,
           label = c("19/20","20/21","21/22","22/23","23/24","24/25"),
           size = 3.5, color = "gray40", fontface = "bold") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y",
               expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     breaks = seq(0, max(hudl_monthly$n) + 2, 2)) +
  labs(title    = "Hudl Kampstatistik - maanedlig fordeling",
       subtitle = sprintf("Kampe per maaned | Total: %d kampe over 6 saesoner",
                          sum(hudl_monthly$n)),
       x = NULL, y = "Antal kampe") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle      = element_text(size = 11, color = "gray30", hjust = 0),
    axis.text.x        = element_text(size = 9),
    axis.text.y        = element_text(size = 10),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

print(p_hudl_monthly)


# --- Overlap-analyse ---

alle_tre <- bind_rows(n11_tidslinje, hudl_tidslinje, diu_tidslinje)

overlap_analyse <- tibble(
  Sammenligning = c("N11 vs DIU", "Hudl vs DIU", "Hudl vs N11", "Alle tre"),
  Start = c(
    max(n11_tidslinje$Forste_dato,   diu_tidslinje$Forste_dato),
    max(hudl_tidslinje$Forste_dato,  diu_tidslinje$Forste_dato),
    max(hudl_tidslinje$Forste_dato,  n11_tidslinje$Forste_dato),
    max(alle_tre$Forste_dato)
  ),
  Slut = c(
    min(n11_tidslinje$Seneste_dato,  diu_tidslinje$Seneste_dato),
    min(hudl_tidslinje$Seneste_dato, diu_tidslinje$Seneste_dato),
    min(hudl_tidslinje$Seneste_dato, n11_tidslinje$Seneste_dato),
    min(alle_tre$Seneste_dato)
  )
) %>%
  mutate(
    Dage      = as.numeric(difftime(Slut, Start, units = "days")),
    Maaneder  = round(Dage / 30.44, 1),
    Har_overlap = Start <= Slut
  ) %>%
  filter(Har_overlap)

for (i in seq_len(nrow(overlap_analyse))) {
  cat(sprintf("%-15s  %s til %s  (%d dage / %.1f maaneder)\n",
              overlap_analyse$Sammenligning[i],
              format(overlap_analyse$Start[i], "%b %Y"),
              format(overlap_analyse$Slut[i],  "%b %Y"),
              overlap_analyse$Dage[i],
              overlap_analyse$Maaneder[i]))
}
cat("\n")

overlap_analyse <- overlap_analyse %>%
  mutate(
    Type  = ifelse(Sammenligning == "Alle tre", "Faelles overlap", "Pairwise overlap"),
    Farve = case_when(
      Sammenligning == "Alle tre"    ~ "#27ae60",
      str_detect(Sammenligning, "N11")  ~ "#FF1493",
      str_detect(Sammenligning, "DIU")  ~ "#DC143C",
      str_detect(Sammenligning, "Hudl") ~ "#FF8C00"
    )
  )


# --- Plot 5: Overlap-maaneder ---

p_overlap <- ggplot(overlap_analyse,
                    aes(x = reorder(Sammenligning, Maaneder), y = Maaneder)) +
  geom_col(aes(fill = Type), alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f mdr\n(%d dage)", Maaneder, Dage)),
            vjust = -0.3, size = 4, fontface = "bold", color = "gray30") +
  scale_fill_manual(values = c("Faelles overlap"  = "#27ae60",
                               "Pairwise overlap" = "#3498db")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)),
                     breaks = seq(0, 20, 5)) +
  coord_flip() +
  labs(title    = "Overlap-perioder mellem datakilder",
       subtitle = "Maaneder med faelles data",
       x = NULL, y = "Maaneder med overlap", fill = "Type") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle      = element_text(size = 12, color = "gray30", hjust = 0),
    axis.text.y        = element_text(size = 13, face = "bold"),
    axis.text.x        = element_text(size = 11),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA)
  )

print(p_overlap)


# --- Plot 6: Zoom paa faelles overlap ---

faelles <- overlap_analyse %>% filter(Sammenligning == "Alle tre")

if (nrow(faelles) > 0) {
  zoom_data <- alle_tre %>%
    mutate(
      Zoom_start = pmax(Forste_dato,  faelles$Start[1]),
      Zoom_slut  = pmin(Seneste_dato, faelles$Slut[1])
    ) %>%
    arrange(desc(Zoom_start)) %>%
    mutate(y_pos = row_number())
  
  p_zoom <- ggplot(zoom_data, aes(y = reorder(Datakilde, y_pos))) +
    annotate("rect",
             xmin = faelles$Start[1], xmax = faelles$Slut[1],
             ymin = 0.5, ymax = 3.5, alpha = 0.15, fill = "#27ae60") +
    geom_segment(aes(x = Zoom_start, xend = Zoom_slut,
                     yend = reorder(Datakilde, y_pos), color = Datakilde),
                 linewidth = 12, alpha = 0.9) +
    geom_point(aes(x = Zoom_start, color = Datakilde), size = 7, shape = 16) +
    geom_point(aes(x = Zoom_slut,  color = Datakilde), size = 7, shape = 17) +
    scale_color_manual(values = c(
      "Fysiologisk Profil (DIU)" = "#DC143C",
      "Next11 Workload"          = "#FF1493",
      "Hudl Kampstatistik"       = "#FF8C00"
    )) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y",
                 expand = expansion(mult = 0.05),
                 limits = c(faelles$Start[1] - 30, faelles$Slut[1] + 30)) +
    labs(title    = "Faelles overlap-periode (zoom)",
         subtitle = sprintf("%s til %s | %.1f maaneder med data fra alle tre kilder",
                            format(faelles$Start[1], "%d. %b %Y"),
                            format(faelles$Slut[1],  "%d. %b %Y"),
                            faelles$Maaneder[1]),
         x = NULL, y = NULL,
         caption = "Cirkel = Start  Trekant = Slut  |  Groen zone = Faelles overlap") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position    = "none",
      plot.title         = element_text(face = "bold", size = 18, hjust = 0),
      plot.subtitle      = element_text(size = 11, color = "gray30", hjust = 0),
      axis.text.y        = element_text(size = 13, face = "bold"),
      axis.text.x        = element_text(size = 10),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.caption       = element_text(hjust = 0, size = 9, color = "gray50"),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.background    = element_rect(fill = "white", color = NA)
    )
  
  print(p_zoom)
}


# --- Plot 7: Overlap matrix ---

overlap_matrix <- expand_grid(
  Kilde1 = c("DIU", "N11", "Hudl"),
  Kilde2 = c("DIU", "N11", "Hudl")
) %>%
  filter(Kilde1 != Kilde2) %>%
  rowwise() %>%
  mutate(
    Overlap_maaneder = case_when(
      (Kilde1 == "DIU"  & Kilde2 == "N11")  | (Kilde1 == "N11"  & Kilde2 == "DIU")  ~
        overlap_analyse$Maaneder[overlap_analyse$Sammenligning == "N11 vs DIU"],
      (Kilde1 == "DIU"  & Kilde2 == "Hudl") | (Kilde1 == "Hudl" & Kilde2 == "DIU")  ~
        overlap_analyse$Maaneder[overlap_analyse$Sammenligning == "Hudl vs DIU"],
      (Kilde1 == "N11"  & Kilde2 == "Hudl") | (Kilde1 == "Hudl" & Kilde2 == "N11")  ~
        overlap_analyse$Maaneder[overlap_analyse$Sammenligning == "Hudl vs N11"],
      TRUE ~ 0
    )
  ) %>%
  ungroup()

p_matrix <- ggplot(overlap_matrix,
                   aes(x = Kilde1, y = Kilde2, fill = Overlap_maaneder)) +
  geom_tile(color = "white", linewidth = 3) +
  geom_text(aes(label = sprintf("%.1f\nmaaneder", Overlap_maaneder)),
            size = 5, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#e74c3c", high = "#27ae60",
                      breaks = seq(0, 20, 5)) +
  coord_equal() +
  labs(title    = "Overlap matrix",
       subtitle = "Maaneder med faelles data mellem hvert kildepar",
       x = NULL, y = NULL, fill = "Overlap\n(maaneder)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle   = element_text(size = 11, color = "gray30", hjust = 0),
    axis.text       = element_text(size = 13, face = "bold"),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 11),
    panel.grid      = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

print(p_matrix)


# --- Tidsperiode-oversigt fra filnavne ---

hudl_kampe  <- ls(pattern = "^hudl_.*_U20_.*_\\d{2}_\\d{2}_\\d{4}")
hudl_datoer2 <- str_extract_all(hudl_kampe, "\\d{2}_\\d{2}_\\d{4}") %>%
  unlist() %>% str_replace_all("_", "-") %>% dmy()

n11_2024       <- ls(pattern = "^rod24_")
n11_2025       <- ls(pattern = "^rod25_")
n11_2024_datoer <- str_extract(n11_2024, "\\d{8}") %>% ymd()
n11_2025_datoer <- str_extract(n11_2025, "\\d{8}") %>% ymd()

fysio_kolonner      <- colnames(Rodovre_Komplet_Fysio_Profil)
fysio_2024_kolonner <- fysio_kolonner[str_detect(fysio_kolonner, "2024")]
fysio_2025_kolonner <- fysio_kolonner[str_detect(fysio_kolonner, "2025")]

cat(sprintf("Hudl: %s - %s (%d saesoner)\n",
            year(min(hudl_datoer2, na.rm = TRUE)),
            year(max(hudl_datoer2, na.rm = TRUE)),
            length(unique(year(hudl_datoer2)))))
cat(sprintf("N11:  %s til %s\n",
            format(min(c(n11_2024_datoer, n11_2025_datoer), na.rm = TRUE), "%d-%m-%Y"),
            format(max(c(n11_2024_datoer, n11_2025_datoer), na.rm = TRUE), "%d-%m-%Y")))
cat(sprintf("Fysio 2024: %d kolonner\n", length(fysio_2024_kolonner)))
cat(sprintf("Fysio 2025: %d kolonner\n\n", length(fysio_2025_kolonner)))

print(table(year(hudl_datoer2)))


# --- Samlet tidslinje (simpel) ---

tidslinje_data <- bind_rows(
  tibble(Kilde = "Hudl Kamp",
         Start = min(hudl_datoer2, na.rm = TRUE),
         Slut  = max(hudl_datoer2, na.rm = TRUE)),
  tibble(Kilde = "Next11",
         Start = min(c(n11_2024_datoer, n11_2025_datoer), na.rm = TRUE),
         Slut  = max(c(n11_2024_datoer, n11_2025_datoer), na.rm = TRUE)),
  tibble(Kilde = "Fysio Tests",
         Start = as.Date("2024-01-01"),
         Slut  = as.Date("2025-12-31"))
)

p_samlet <- ggplot(tidslinje_data, aes(y = Kilde)) +
  geom_segment(aes(x = Start, xend = Slut, yend = Kilde),
               size = 8, color = "steelblue") +
  geom_point(aes(x = Start), size = 4, color = "darkgreen") +
  geom_point(aes(x = Slut),  size = 4, color = "darkred") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title    = "Tidsperioder i datagrundlag - Rodovre U20",
       subtitle = "Groen = Start  |  Rod = Slut",
       x = "Dato", y = "Datakilde") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text  = element_text(size = 12)
  )

print(p_samlet)