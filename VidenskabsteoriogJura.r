# Visualisering - videnskabsteori og etik/jura
# Rodovre U20

library(tidyverse)
library(patchwork)


# ================================================================
# DEL 1: VIDENSKABSTEORI
# ================================================================

dataset_data <- data.frame(
  Datasaet = c("N11 GPS\n2024", "N11 GPS\n2025", "Hudl\nKampe", "DIU\nFysio"),
  Antal    = c(4182, 6357, 80, 108),
  Kategori = c("Traening", "Traening", "Kamp", "Profil")
)

triangulering <- data.frame(
  x = c(0, -0.866, 0.866),
  y = c(1, -0.5, -0.5)
)

labels_data <- data.frame(
  x     = c(0, -0.866, 0.866),
  y     = c(1.3, -0.8, -0.8),
  label = c("N11 GPS\n10.539 sessioner", "Hudl\n80 kampe", "DIU Fysio\n108 spillere"),
  color = c("#FF69B4", "#FF8C42", "#DC143C")
)

# Datasaet soejlediagram
p_datasaet <- ggplot(dataset_data, aes(x = Datasaet, y = Antal, fill = Kategori)) +
  geom_col(color = "white", size = 1) +
  geom_text(aes(label = scales::comma(Antal)),
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Traening" = "#FF69B4",
                               "Kamp"     = "#FF8C42",
                               "Profil"   = "#DC143C")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Empirisk datamateriale", x = NULL, y = "Antal") +
  theme_minimal() +
  theme(
    plot.title         = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Triangulering
p_triangulering <- ggplot() +
  geom_polygon(data = data.frame(x = c(0, -0.866, 0.866, 0),
                                 y = c(1, -0.5, -0.5, 1)),
               aes(x = x, y = y),
               fill = alpha("#4682B4", 0.1), color = "#4682B4", size = 2) +
  geom_point(data = triangulering, aes(x = x, y = y),
             size = 25, color = c("#FF69B4", "#FF8C42", "#DC143C")) +
  geom_text(data = labels_data,
            aes(x = x, y = y, label = label),
            size = 4.5, fontface = "bold", color = labels_data$color,
            lineheight = 0.85) +
  annotate("text", x = 0, y = 0.05,
           label = "VALIDITET\ngennem\ntriangulering",
           size = 6, fontface = "bold", color = "#4682B4", lineheight = 0.9) +
  coord_fixed(xlim = c(-1.2, 1.2), ylim = c(-1, 1.5)) +
  labs(title = "Metodisk triangulering") +
  theme_void() +
  theme(
    plot.title  = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.margin = margin(20, 20, 20, 20)
  )

final_videnskabsteori <- (p_datasaet / p_triangulering) +
  plot_annotation(
    title = "Videnskabsteori: Positivistisk-pragmatisk tilgang",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(final_videnskabsteori)
ggsave("videnskabsteori.png", final_videnskabsteori,
       width = 10, height = 12, dpi = 300, bg = "white")


# ================================================================
# DEL 2: ETIK OG JURA
# ================================================================

gdpr_artikler <- data.frame(
  Artikel    = c("Art. 5", "Art. 6(1)(a)", "Art. 9", "Art. 13",
                 "Art. 15-17", "Art. 25", "Art. 32", "Art. 35"),
  Princip    = c("Principper", "Samtykke", "Sundhedsdata", "Oplysningspligt",
                 "Rettigheder", "Privacy by design", "Sikkerhed", "DPIA"),
  Anvendelse = c("Formaalsbegrænsning, Dataminimering",
                 "Informeret samtykke, Foraeldremyndighed",
                 "Eksplicit samtykke, Fysiologiske data",
                 "Transparent info om databehandling",
                 "Indsigt, rettelse, sletning",
                 "Anonymisering som standard",
                 "Kryptering, Adgangskontrol",
                 "Risikovurdering af tracking"),
  Kategori   = c("Grundlag", "Grundlag", "Saerlige data", "Information",
                 "Rettigheder", "Teknisk", "Teknisk", "Procedure"),
  y = 8:1
)

gdpr_flow <- data.frame(
  Fase         = c("Indsamling", "Opbevaring", "Behandling", "Rapportering", "Sletning"),
  Datakilde    = c("N11 GPS\nHudl\nDIU", "Database", "Analyse", "Outputs", "Arkiv"),
  GDPR_Artikel = c("Art. 6+9", "Art. 32", "Art. 5", "Art. 25", "Art. 17"),
  Beskyttelse  = c("Samtykke\nFormaal",
                   "Kryptering\nBackup",
                   "Minimering\nIntegritet",
                   "Anonymisering\nPseudonymisering",
                   "3-aarig\nperiode"),
  x    = 1:5,
  Farve = c("#FF69B4", "#4682B4", "#5B9BD5", "#7CB3E3", "#708090")
)

etiske_principper <- data.frame(
  Princip        = c("Transparens", "Ikke-stigmatisering",
                     "Ret til indsigt", "Datasikkerhed"),
  Beskrivelse    = c("Spillere informeres om\ndataindsamling og anvendelse",
                     "Data bruges konstruktivt\ntil udvikling, ikke eksklusion",
                     "Spillere har ret til\nat se egne data",
                     "Krypteret opbevaring\nmed adgangskontrol"),
  Implementation = c("100%", "100%", "100%", "100%"),
  Farve = c("#7CB3E3", "#9DCBF0", "#5B9BD5", "#4682B4"),
  x = c(1, 2, 3, 4),
  y = c(1, 1, 1, 1)
)

juridisk_compliance <- data.frame(
  Kategori = c("GDPR", "Arbejdsmiljoe", "Etik", "Fortrolighed"),
  Procent  = c(100, 100, 100, 100),
  Farve    = c("#4682B4", "#5B9BD5", "#7CB3E3", "#9DCBF0")
)

# GDPR artikler oversigt
p_gdpr <- ggplot(gdpr_artikler, aes(x = 1, y = y)) +
  geom_tile(aes(x = 0.12, fill = Kategori), width = 0.2, height = 0.9,
            alpha = 0.8, color = "white", size = 1) +
  geom_text(aes(x = 0.35, label = Artikel),
            fontface = "bold", size = 3.8, hjust = 0) +
  geom_text(aes(x = 0.65, label = Princip),
            fontface = "bold", size = 3.3, hjust = 0) +
  geom_text(aes(x = 1.2, label = Anvendelse),
            size = 2.8, hjust = 0, color = "gray30") +
  scale_fill_manual(values = c("Grundlag"     = "#4682B4",
                               "Saerlige data"= "#DC143C",
                               "Information"  = "#5B9BD5",
                               "Rettigheder"  = "#7CB3E3",
                               "Teknisk"      = "#9DCBF0",
                               "Procedure"    = "#B8D4E8")) +
  scale_x_continuous(limits = c(0, 2.5)) +
  scale_y_continuous(limits = c(0.5, 8.5)) +
  labs(title = "GDPR artikler", subtitle = "Konkrete artikelhenvisninger") +
  theme_void() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
    legend.position = "none",
    plot.margin   = margin(10, 10, 10, 10)
  )

# Etiske principper
p_etik <- ggplot(etiske_principper, aes(x = x, y = y)) +
  geom_tile(aes(fill = Farve), width = 0.8, height = 0.5,
            color = "black", size = 2) +
  geom_text(aes(label = Princip, y = 1.35),
            fontface = "bold", size = 4.5, color = etiske_principper$Farve) +
  geom_text(aes(label = Beskrivelse),
            size = 3.2, color = "black", fontface = "bold", lineheight = 0.85) +
  geom_text(aes(label = Implementation), y = 0.6,
            size = 4, fontface = "bold", color = "darkgreen") +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0.3, 4.7)) +
  scale_y_continuous(limits = c(0.5, 1.5)) +
  labs(title = "Etiske principper",
       subtitle = "Fundamentale vaerdier i databehandlingen") +
  theme_void() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
    plot.margin   = margin(10, 10, 10, 10)
  )

# GDPR compliance i dataflow
p_flow <- ggplot(gdpr_flow, aes(x = x, y = 1)) +
  geom_segment(aes(x = x + 0.35, xend = x + 0.65, y = 1, yend = 1),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               size = 1.2, color = "#4682B4",
               data = gdpr_flow[1:4, ]) +
  geom_tile(aes(fill = Farve), width = 0.65, height = 0.55,
            color = "white", size = 2) +
  geom_text(aes(label = Fase),       y = 1.45, fontface = "bold", size = 3.5) +
  geom_text(aes(label = Datakilde),  y = 1,    size = 2.8, fontface = "bold",
            color = "black", lineheight = 0.85) +
  geom_text(aes(label = GDPR_Artikel), y = 0.65, size = 3,
            fontface = "bold", color = "#DC143C") +
  geom_text(aes(label = Beskyttelse), y = 0.35, size = 2.5,
            lineheight = 0.8, color = "gray20") +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0.5, 5.5)) +
  scale_y_continuous(limits = c(0.1, 1.7)) +
  labs(title    = "GDPR compliance i dataflow",
       subtitle = "Artikler markeret med rodt") +
  theme_void() +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
    plot.margin   = margin(10, 10, 10, 10)
  )

# Compliance status
p_compliance <- ggplot(juridisk_compliance,
                       aes(x = Kategori, y = Procent, fill = Farve)) +
  geom_col(color = "white", size = 1.5, width = 0.7) +
  geom_text(aes(label = paste0(Procent, "%")),
            vjust = -0.5, fontface = "bold", size = 5, color = "gray20") +
  geom_text(aes(label = "Opfyldt"), y = 50,
            fontface = "bold", size = 4, color = "black") +
  scale_fill_identity() +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25),
                     expand = c(0, 0)) +
  labs(title    = "Juridisk og etisk compliance",
       subtitle = "Fuld overholdelse",
       x = NULL, y = "Opfyldelse (%)") +
  theme_minimal() +
  theme(
    plot.title         = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 9, hjust = 0.5, color = "gray40"),
    axis.text.x        = element_text(size = 10, face = "bold"),
    axis.text.y        = element_text(size = 10),
    axis.title.y       = element_text(size = 11, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(10, 10, 10, 10)
  )

final_jura <- ((p_gdpr | p_etik) / (p_flow | p_compliance)) +
  plot_annotation(
    title    = "Etik og jura: GDPR-compliance og etiske principper",
    subtitle = "Forordning (EU) 2016/679",
    theme = theme(
      plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.margin   = margin(15, 15, 15, 15)
    )
  )

print(final_jura)
ggsave("etik_jura.png", final_jura, width = 14, height = 12, dpi = 300, bg = "white")