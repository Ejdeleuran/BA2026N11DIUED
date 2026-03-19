library(tidyverse)
library(stringdist)
library(ggplot2)

# Farver
rodovre_blaa  <- "#1E3A8A"
rodovre_rod   <- "#DC2626"
rodovre_groen <- "#059669"
rodovre_graa  <- "#6B7280"

n11_pink           <- "#EC4899"
diu_rod            <- "#DC2626"
hudl_orange        <- "#F97316"
standardiseret_groen <- "#059669"
midter_graa        <- "#6B7280"
graa_tekst         <- "#6B7280"
mork_tekst         <- "#1F2937"


# --- Fuzzy matching ---

fuzzy_match_navne <- function(navn1, navn2, threshold = 0.15) {
  n1 <- tolower(trimws(gsub("[^A-Za-zÆØÅæøå ]", "", navn1)))
  n2 <- tolower(trimws(gsub("[^A-Za-zÆØÅæøå ]", "", navn2)))
  dist <- stringdist(n1, n2, method = "jw")
  return(dist < threshold)
}


# --- Navnestandardisering ---

standardiser_navn <- function(navne, kilde) {
  sapply(navne, function(navn) {
    navn_clean <- trimws(navn)
    
    if (kilde == "DIU") {
      if (str_detect(navn_clean, ",")) {
        dele <- str_split(navn_clean, ",")[[1]]
        return(paste(trimws(dele[2]), trimws(dele[1])))
      }
    } else if (kilde == "Hudl") {
      navn_clean <- str_replace_all(navn_clean, "\\.", "")
      navn_clean <- str_replace_all(navn_clean, "/", " ")
    }
    
    return(navn_clean)
  }, USE.NAMES = FALSE)
}

standardiser_datakilder <- function() {
  
  # Next11 data
  rod_dfs <- ls(pattern = "^rod(24|25)_", envir = .GlobalEnv)
  for (df_navn in rod_dfs) {
    df <- get(df_navn, envir = .GlobalEnv)
    player_col <- names(df)[str_detect(tolower(names(df)), "player.*tag")]
    if (length(player_col) > 0) {
      df <- df %>%
        mutate(
          Spiller_Navn_Original = !!sym(player_col[1]),
          Spiller_Navn = standardiser_navn(!!sym(player_col[1]), "Next11")
        )
      assign(df_navn, df, envir = .GlobalEnv)
    }
  }
  
  # Hudl data
  hudl_dfs <- ls(pattern = "^hudl_\\d{8}", envir = .GlobalEnv)
  for (df_navn in hudl_dfs) {
    df <- get(df_navn, envir = .GlobalEnv)
    player_col <- names(df)[str_detect(tolower(names(df)), "^player$")]
    if (length(player_col) > 0) {
      df <- df %>%
        mutate(
          Spiller_Navn_Original = !!sym(player_col[1]),
          Spiller_Navn = standardiser_navn(!!sym(player_col[1]), "Hudl")
        )
      assign(df_navn, df, envir = .GlobalEnv)
    }
  }
  
  # DIU / fysiologisk profil
  if (exists("Rodovre_Komplet_Fysio_Profil", envir = .GlobalEnv)) {
    Rodovre_Komplet_Fysio_Profil <<- Rodovre_Komplet_Fysio_Profil %>%
      mutate(
        Spiller_Navn_Original = Spiller_Navn,
        Spiller_Navn = standardiser_navn(Spiller_Navn, "DIU")
      )
  }
}

standardiser_datakilder()


# --- Match spillere på tværs af kilder ---

match_spillere_tvaers_kilder <- function(fysio_profil, andre_data, threshold = 0.15) {
  resultat <- andre_data %>%
    rowwise() %>%
    mutate(
      Match_Spiller_Nr = {
        matches <- sapply(fysio_profil$Spiller_Navn, function(fysio_navn) {
          fuzzy_match_navne(Spiller_Navn, fysio_navn, threshold)
        })
        if (any(matches)) {
          fysio_profil$Spiller_Nr[which(matches)[1]]
        } else {
          NA_integer_
        }
      }
    ) %>%
    ungroup()
  return(resultat)
}


# --- Visualisering af navnestandardisering ---

create_name_standardization_plot <- function() {
  
  plot_data <- data.frame(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(3.5, 3.5, 3.5, 1, 1, 1),
    Kilde  = rep(c("Next11", "DIU", "Hudl"), 2),
    Status = rep(c("Original", "Standardiseret"), each = 3),
    Tekst_top = c("Fornavn Efternavn", "Efternavn, Fornavn", "F. Efternavn / Fornavn E.",
                  "Fornavn Efternavn", "Fornavn Efternavn", "Fornavn Efternavn"),
    Tekst_bund = c("Magnus Nielsen", "Nielsen, Magnus", "M. Nielsen",
                   "Magnus Nielsen", "Magnus Nielsen", "Magnus Nielsen")
  )
  
  plot_data$fill_color <- c(n11_pink, diu_rod, hudl_orange,
                            standardiseret_groen, standardiseret_groen, standardiseret_groen)
  
  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_tile(aes(fill = I(fill_color)), width = 0.85, height = 0.8,
              color = "white", size = 3) +
    geom_text(aes(label = Tekst_top, y = y + 0.25),
              size = 4.2, color = "white", fontface = "plain", alpha = 0.85) +
    geom_text(aes(label = Tekst_bund, y = y - 0.25),
              size = 5.5, color = "white", fontface = "bold.italic") +
    scale_x_continuous(breaks = 1:3, labels = c("Next11", "DIU", "Hudl"),
                       limits = c(0.3, 3.7)) +
    annotate("rect", xmin = 0.35, xmax = 3.65, ymin = 2.15, ymax = 2.55,
             fill = midter_graa, color = "white", size = 2) +
    annotate("text", x = 2, y = 2.35,
             label = "R Script: Tekstmanipulation & Fuzzy Matching",
             size = 6.5, color = "white", fontface = "bold") +
    annotate("segment", x = 1, xend = 1, y = 3.1, yend = 2.6,
             arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
             color = midter_graa, size = 2, lineend = "round") +
    annotate("segment", x = 2, xend = 2, y = 3.1, yend = 2.6,
             arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
             color = midter_graa, size = 2, lineend = "round") +
    annotate("segment", x = 3, xend = 3, y = 3.1, yend = 2.6,
             arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
             color = midter_graa, size = 2, lineend = "round") +
    annotate("segment", x = 1, xend = 1, y = 2.1, yend = 1.45,
             arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
             color = midter_graa, size = 2, lineend = "round") +
    annotate("segment", x = 2, xend = 2, y = 2.1, yend = 1.45,
             arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
             color = midter_graa, size = 2, lineend = "round") +
    annotate("segment", x = 3, xend = 3, y = 2.1, yend = 1.45,
             arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
             color = midter_graa, size = 2, lineend = "round") +
    annotate("text", x = 1, y = 3.95, label = "ORIGINAL FORMAT",
             size = 3.5, fontface = "bold", color = graa_tekst) +
    annotate("text", x = 2, y = 3.95, label = "ORIGINAL FORMAT",
             size = 3.5, fontface = "bold", color = graa_tekst) +
    annotate("text", x = 3, y = 3.95, label = "ORIGINAL FORMAT",
             size = 3.5, fontface = "bold", color = graa_tekst) +
    annotate("text", x = 1, y = 0.55, label = "STANDARDISERET",
             size = 3.5, fontface = "bold", color = standardiseret_groen) +
    annotate("text", x = 2, y = 0.55, label = "STANDARDISERET",
             size = 3.5, fontface = "bold", color = standardiseret_groen) +
    annotate("text", x = 3, y = 0.55, label = "STANDARDISERET",
             size = 3.5, fontface = "bold", color = standardiseret_groen) +
    labs(
      title    = "Navnestandardisering pa tvaers af datakilder",
      subtitle = "Fuzzy String Matching haandterer forskellige navneformater og sikrer konsistent data"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 20, face = "bold", color = mork_tekst,
                                   hjust = 0.5, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 13, color = graa_tekst, hjust = 0.5,
                                   margin = margin(b = 25)),
      axis.text.y   = element_blank(),
      axis.text.x   = element_text(size = 15, color = mork_tekst, face = "bold",
                                   margin = margin(t = 15)),
      axis.title    = element_blank(),
      panel.grid    = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin   = margin(30, 40, 30, 40)
    ) +
    coord_cartesian(ylim = c(0.4, 4.2), xlim = c(0.3, 3.7))
  
  return(p)
}

p <- create_name_standardization_plot()
print(p)

ggsave("navne_standardisering.png", p, width = 16, height = 10, dpi = 300, bg = "white")