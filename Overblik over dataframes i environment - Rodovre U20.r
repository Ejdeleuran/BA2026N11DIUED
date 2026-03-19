# Overblik over dataframes i environment - Rodovre U20

library(tidyverse)
library(knitr)


# --- Oversigt over alle dataframes ---

vis_alle_dataframes <- function() {
  
  alle_objekter <- ls(envir = .GlobalEnv)
  dataframes <- alle_objekter[sapply(alle_objekter, function(x) {
    is.data.frame(get(x, envir = .GlobalEnv))
  })]
  
  if (length(dataframes) == 0) {
    cat("Ingen dataframes fundet i environment.\n")
    return(invisible(NULL))
  }
  
  oversigt <- data.frame(
    Dataframe_Navn = character(),
    Raekker        = integer(),
    Kolonner       = integer(),
    Stoerrelse_MB  = numeric(),
    Kategori       = character(),
    stringsAsFactors = FALSE
  )
  
  for (df_navn in dataframes) {
    df <- get(df_navn, envir = .GlobalEnv)
    
    kategori <- case_when(
      grepl("^rod24_",       df_navn)                     ~ "N11 Data 2024",
      grepl("^rod25_",       df_navn)                     ~ "N11 Data 2025",
      grepl("^hudl_\\d{8}", df_navn)                     ~ "Hudl Kamp Data",
      grepl("fysio|profil",  df_navn, ignore.case = TRUE) ~ "Fysiologisk Profil",
      grepl("traening.*land",df_navn, ignore.case = TRUE) ~ "Traening paa Land",
      grepl("kampoversigt",  df_navn, ignore.case = TRUE) ~ "Hudl Oversigt",
      TRUE ~ "Andet"
    )
    
    stoerrelse <- object.size(df) / 1024^2
    
    oversigt <- rbind(oversigt, data.frame(
      Dataframe_Navn = df_navn,
      Raekker        = nrow(df),
      Kolonner       = ncol(df),
      Stoerrelse_MB  = round(stoerrelse, 2),
      Kategori       = kategori,
      stringsAsFactors = FALSE
    ))
  }
  
  oversigt <- oversigt %>% arrange(Kategori, Dataframe_Navn)
  print(kable(oversigt, format = "simple", align = c("l","r","r","r","l")))
  
  cat(sprintf("\nAntal dataframes: %d\n", nrow(oversigt)))
  cat(sprintf("Total stoerrelse: %.2f MB\n\n", sum(oversigt$Stoerrelse_MB)))
  
  kategori_oversigt <- oversigt %>%
    group_by(Kategori) %>%
    summarise(
      Antal         = n(),
      Total_Raekker = sum(Raekker),
      Total_MB      = round(sum(Stoerrelse_MB), 2)
    )
  
  print(kable(kategori_oversigt, format = "simple", align = c("l","r","r","r")))
  cat("\n")
  
  return(invisible(oversigt))
}


# --- Detaljeret inspektion af enkelt dataframe ---

inspicer_dataframe <- function(df_navn) {
  
  if (!exists(df_navn, envir = .GlobalEnv)) {
    cat(sprintf("Dataframe '%s' findes ikke i environment.\n", df_navn))
    return(invisible(NULL))
  }
  
  df <- get(df_navn, envir = .GlobalEnv)
  
  cat(sprintf("\n%s\n", df_navn))
  cat(sprintf("  Raekker:    %d\n", nrow(df)))
  cat(sprintf("  Kolonner:   %d\n", ncol(df)))
  cat(sprintf("  Stoerrelse: %.2f MB\n\n", object.size(df) / 1024^2))
  
  kolonne_info <- data.frame(
    Nr          = 1:ncol(df),
    Kolonne     = names(df),
    Type        = sapply(df, function(x) class(x)[1]),
    NA_Antal    = sapply(df, function(x) sum(is.na(x))),
    NA_Procent  = round(sapply(df, function(x) sum(is.na(x)) / length(x) * 100), 1),
    stringsAsFactors = FALSE
  )
  
  print(kable(kolonne_info, format = "simple", align = c("r","l","l","r","r")))
  
  cat("\nForste 5 raekker:\n")
  print(head(df, 5))
  
  central_cols <- names(df)[grepl("spiller|player|navn|name|dato|date",
                                  names(df), ignore.case = TRUE)]
  if (length(central_cols) > 0) {
    cat("\nUnikke vaerdier i centrale kolonner:\n")
    for (col in head(central_cols, 5)) {
      cat(sprintf("  %s: %d unikke\n", col, length(unique(df[[col]]))))
    }
  }
  cat("\n")
}


# --- N11 oversigt ---

vis_n11_oversigt <- function() {
  
  n11_2024 <- ls(pattern = "^rod24_", envir = .GlobalEnv)
  n11_2025 <- ls(pattern = "^rod25_", envir = .GlobalEnv)
  
  cat(sprintf("N11 dataframes: %d i 2024, %d i 2025\n\n", length(n11_2024), length(n11_2025)))
  
  if (length(n11_2024) > 0) {
    cat("2024:\n")
    n11_2024_info <- data.frame(
      Dataframe = character(), Dato = character(),
      Raekker = integer(), Kolonner = integer(),
      stringsAsFactors = FALSE
    )
    for (df_navn in n11_2024) {
      df   <- get(df_navn, envir = .GlobalEnv)
      dato <- gsub("rod24_", "", df_navn)
      n11_2024_info <- rbind(n11_2024_info, data.frame(
        Dataframe = df_navn, Dato = dato,
        Raekker = nrow(df), Kolonner = ncol(df),
        stringsAsFactors = FALSE
      ))
    }
    print(kable(n11_2024_info %>% arrange(Dato), format = "simple",
                align = c("l","l","r","r")))
    cat("\n")
  }
  
  if (length(n11_2025) > 0) {
    cat("2025:\n")
    n11_2025_info <- data.frame(
      Dataframe = character(), Dato = character(),
      Raekker = integer(), Kolonner = integer(),
      stringsAsFactors = FALSE
    )
    for (df_navn in n11_2025) {
      df   <- get(df_navn, envir = .GlobalEnv)
      dato <- gsub("rod25_", "", df_navn)
      n11_2025_info <- rbind(n11_2025_info, data.frame(
        Dataframe = df_navn, Dato = dato,
        Raekker = nrow(df), Kolonner = ncol(df),
        stringsAsFactors = FALSE
      ))
    }
    print(kable(n11_2025_info %>% arrange(Dato), format = "simple",
                align = c("l","l","r","r")))
    cat("\n")
  }
}


# --- Hudl oversigt ---

vis_hudl_oversigt <- function() {
  
  hudl_dfs <- ls(pattern = "^hudl_\\d{8}", envir = .GlobalEnv)
  cat(sprintf("Hudl kamp dataframes: %d\n\n", length(hudl_dfs)))
  
  if (length(hudl_dfs) > 0) {
    hudl_info <- data.frame(
      Dataframe = character(), Dato = character(),
      Raekker = integer(), Kolonner = integer(), Spillere = integer(),
      stringsAsFactors = FALSE
    )
    for (df_navn in hudl_dfs) {
      df         <- get(df_navn, envir = .GlobalEnv)
      dato       <- gsub("hudl_", "", df_navn)
      player_col <- names(df)[grepl("player|spiller", names(df), ignore.case = TRUE)]
      spillere   <- if (length(player_col) > 0) length(unique(df[[player_col[1]]])) else NA
      hudl_info  <- rbind(hudl_info, data.frame(
        Dataframe = df_navn, Dato = dato,
        Raekker = nrow(df), Kolonner = ncol(df), Spillere = spillere,
        stringsAsFactors = FALSE
      ))
    }
    print(kable(hudl_info %>% arrange(Dato), format = "simple",
                align = c("l","l","r","r","r")))
    cat("\n")
  }
  
  if (exists("hudl_kampoversigt", envir = .GlobalEnv)) {
    kampoversigt <- get("hudl_kampoversigt", envir = .GlobalEnv)
    cat(sprintf("hudl_kampoversigt: %d kampe, %d kolonner\n\n",
                nrow(kampoversigt), ncol(kampoversigt)))
  }
}


# --- Fysiologisk profil oversigt ---

vis_fysio_profil_oversigt <- function() {
  
  if (!exists("Rodovre_Komplet_Fysio_Profil", envir = .GlobalEnv)) {
    cat("Rodovre_Komplet_Fysio_Profil ikke fundet.\n")
    return(invisible(NULL))
  }
  
  df <- Rodovre_Komplet_Fysio_Profil
  cat(sprintf("Rodovre_Komplet_Fysio_Profil: %d spillere, %d variable\n\n",
              nrow(df), ncol(df)))
  
  kategorier <- list(
    "Demografisk"    = c("Spiller_Nr","Spiller_Navn","Troje_Nr","Fodselsaar",
                         "Hojde_cm","Vaegt_kg","Alder","Aldersgruppe","BMI"),
    "Styrke 2024"    = grep("2024",           names(df), value = TRUE),
    "Styrke 2025"    = grep("2025",           names(df), value = TRUE),
    "DIU"            = grep("DIU|Asymmetry|Z_Score", names(df), value = TRUE),
    "Hudl"           = grep("Hudl",           names(df), value = TRUE),
    "Afledte"        = grep("Udvikling|Total|Rel_|Injury_Risk|Performance",
                            names(df), value = TRUE)
  )
  
  for (kat in names(kategorier)) {
    cols <- kategorier[[kat]][kategorier[[kat]] %in% names(df)]
    cat(sprintf("%s (%d kolonner):\n", kat, length(cols)))
    if (length(cols) > 0) cat(paste(" ", cols, collapse = "\n"), "\n")
    cat("\n")
  }
  
  if ("Data_Komplethed_Pct" %in% names(df)) {
    cat(sprintf("Datakomplethed: gns. %.1f%%  (min %.1f%%  max %.1f%%)\n\n",
                mean(df$Data_Komplethed_Pct, na.rm = TRUE),
                min(df$Data_Komplethed_Pct,  na.rm = TRUE),
                max(df$Data_Komplethed_Pct,  na.rm = TRUE)))
  }
}


# --- Kør ---

vis_alle_dataframes()
vis_n11_oversigt()
vis_hudl_oversigt()
vis_fysio_profil_oversigt()

inspicer_dataframe("Rodovre_Komplet_Fysio_Profil")

# Kolonnenavne for hudl og rod dataframes
df_navne <- ls(envir = .GlobalEnv)
df_navne <- df_navne[sapply(df_navne, function(x) is.data.frame(get(x)))]
df_navne <- df_navne[grepl("^hudl|^rod", df_navne)]

for (df_navn in df_navne) {
  cat(sprintf("\n%s:\n", df_navn))
  print(names(get(df_navn, envir = .GlobalEnv)))
}

colnames(Rodovre_Komplet_Fysio_Profil)