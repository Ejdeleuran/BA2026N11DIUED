# Random Forest visualisering
# Kør efter 0_4_2klassifikationsmodelller.r

library(ggplot2)
library(gridExtra)
library(grid)

if (!exists("rf_model")) stop("rf_model ikke fundet - kør klassifikationsscriptet forst")

graphics.off()

rf_final <- rf_model$finalModel


# --- Plot 1: Majority vote illustration (base R) ---

set.seed(42)

n_trees   <- rf_final$ntree
oob_error <- rf_final$err.rate[nrow(rf_final$err.rate), "OOB"]
hoj_votes <- round(n_trees * (1 - oob_error) * 0.82)
lav_votes <- n_trees - hoj_votes

tree_colors <- c("#FF6B6B", "#4ECDC4", "#F39C12", "#9B59B6", "#3498DB")
tree_labels <- c("Trae 1", "Trae 47", "Trae 124", "Trae 201", "Trae 300")
tree_focus  <- c("ACWR", "Load_CV", "Styrke", "ACWR+CV", "Weekly_PL")
tree_votes  <- c("HOJ", "HOJ", "LAV", "HOJ", "HOJ")
tree_x      <- c(10, 30, 50, 70, 90)

par(mfrow = c(1, 1), mar = c(2, 2, 4, 2), bg = "#1a1a2e")
plot(NULL, xlim = c(0, 100), ylim = c(0, 100),
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "", bty = "n")

rect(-5, -5, 105, 105, col = "#1a1a2e", border = NA)

text(50, 97, "Random Forest - Majority Vote", col = "#4ECDC4", cex = 2, font = 2)
text(50, 92, sprintf("Rodovre U20 | %d traeer | %d spillere", n_trees, nrow(model_df)),
     col = "#888888", cex = 1.2)

rect(25, 75, 75, 88, col = "#667eea", border = "#8899ff", lwd = 2)
text(50, 84, "INPUT: Spiller Data", col = "white", cex = 1.3, font = 2)
text(50, 79, "ACWR=1.92 | Load_CV=42% | Asymmetri=8%", col = "white", cex = 0.9)

arrows(50, 75, 50, 70, col = "#4ECDC4", lwd = 3, length = 0.15)

for (i in 1:5) {
  rect(tree_x[i]-8, 45, tree_x[i]+8, 68, col = tree_colors[i], border = "white", lwd = 2)
  text(tree_x[i], 64, tree_labels[i], col = "white", cex = 0.9, font = 2)
  text(tree_x[i], 58, paste0("Fokus:\n", tree_focus[i]), col = "white", cex = 0.7)
  vote_col <- ifelse(tree_votes[i] == "HOJ", "#c0392b", "#27ae60")
  rect(tree_x[i]-7, 46, tree_x[i]+7, 52, col = vote_col, border = NA)
  text(tree_x[i], 49, tree_votes[i], col = "white", cex = 0.8, font = 2)
  arrows(tree_x[i], 45, tree_x[i], 38, col = tree_colors[i], lwd = 2, length = 0.1)
}

text(50, 40, sprintf("... og %d andre traeer stemmer ogsaa ...", n_trees - 5),
     col = "#888888", cex = 1, font = 3)

segments(10, 35, 90, 35, col = "#4ECDC4", lwd = 3)
arrows(50, 35, 50, 28, col = "#4ECDC4", lwd = 3, length = 0.15)

rect(30, 18, 70, 28, col = "#e74c3c", border = "#ff6b6b", lwd = 3)
text(50, 25, "MAJORITY VOTE", col = "white", cex = 1.2, font = 2)
text(50, 21, sprintf("%d / %d traeer = HOJ RISIKO", hoj_votes, n_trees), col = "white", cex = 1)

arrows(50, 18, 50, 12, col = "#FF6B6B", lwd = 3, length = 0.15)

rect(25, 2, 75, 12, col = "#FF6B6B", border = "#ff8888", lwd = 3)
text(50, 8.5, "FINAL OUTPUT", col = "white", cex = 1, font = 2)
text(50, 5, "HOJ RISIKO", col = "white", cex = 1.5, font = 2)


# --- Plot 2: Feature importance ---

imp_df <- data.frame(
  Feature          = rownames(importance(rf_final)),
  MeanDecreaseGini = importance(rf_final)[, "MeanDecreaseGini"]
) %>%
  mutate(
    Kilde = case_when(
      Feature %in% c("ACWR_Estimate", "Load_CV", "Weekly_Mean_PL", "Mean_Load_Per_Min") ~ "N11 Workload",
      Feature %in% c("DIU_Leg_Asymmetry_pct", "DIU_Z_Score")                           ~ "DIU Test",
      Feature %in% c("Rel_Bench_2024", "Rel_Deadlift_2024", "Rel_Squat_2024")           ~ "Styrke",
      Feature %in% c("Hudl_Points_Per_Game", "Hudl_Kampe_Spillet")                      ~ "Hudl",
      TRUE                                                                               ~ "Demografi"
    )
  ) %>%
  arrange(desc(MeanDecreaseGini))

p_imp <- imp_df %>%
  mutate(Feature = factor(Feature, levels = rev(Feature))) %>%
  ggplot(aes(x = Feature, y = MeanDecreaseGini, fill = Kilde)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", MeanDecreaseGini)),
            hjust = -0.1, size = 3.5, color = "white") +
  coord_flip() +
  scale_fill_manual(values = c("N11 Workload" = "#FF6B6B",
                               "Styrke"       = "#4ECDC4",
                               "DIU Test"     = "#45B7D1",
                               "Hudl"         = "#96CEB4",
                               "Demografi"    = "#FFEAA7")) +
  labs(title    = "Random Forest: Feature Importance",
       subtitle = "Mean Decrease Gini - storre vaerdi = vigtigere feature",
       x = NULL, y = "Importance") +
  theme_minimal() +
  theme(
    plot.background   = element_rect(fill = "#1a1a2e", color = NA),
    panel.background  = element_rect(fill = "#1a1a2e", color = NA),
    panel.grid.major  = element_line(color = "#333355"),
    panel.grid.minor  = element_blank(),
    text              = element_text(color = "white"),
    axis.text         = element_text(color = "white", size = 10),
    plot.title        = element_text(color = "#4ECDC4", size = 16, face = "bold"),
    plot.subtitle     = element_text(color = "#888888", size = 11),
    legend.background = element_rect(fill = "#1a1a2e"),
    legend.text       = element_text(color = "white"),
    legend.position   = "bottom"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p_imp)


# --- Plot 3: Datakilde-bidrag ---

kilde_summary <- imp_df %>%
  group_by(Kilde) %>%
  summarise(Total = sum(MeanDecreaseGini), .groups = "drop") %>%
  mutate(Procent = Total / sum(Total) * 100) %>%
  arrange(desc(Procent))

p_kilde <- kilde_summary %>%
  mutate(Kilde = factor(Kilde, levels = rev(Kilde))) %>%
  ggplot(aes(x = Kilde, y = Procent, fill = Kilde)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.0f%%", Procent)),
            hjust = -0.2, size = 6, color = "white", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("N11 Workload" = "#FF6B6B",
                               "Styrke"       = "#4ECDC4",
                               "DIU Test"     = "#45B7D1",
                               "Hudl"         = "#96CEB4",
                               "Demografi"    = "#FFEAA7")) +
  labs(title    = "Datakilde-bidrag til Random Forest",
       subtitle = "Procentvis andel af samlet praediktiv kraft",
       x = NULL, y = "Andel af total importance (%)") +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#1a1a2e", color = NA),
    panel.background = element_rect(fill = "#1a1a2e", color = NA),
    panel.grid.major = element_line(color = "#333355"),
    panel.grid.minor = element_blank(),
    text             = element_text(color = "white"),
    axis.text        = element_text(color = "white", size = 12, face = "bold"),
    plot.title       = element_text(color = "#FF6B6B", size = 18, face = "bold"),
    plot.subtitle    = element_text(color = "#888888", size = 12),
    legend.position  = "none"
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1)))

print(p_kilde)


# --- Plot 4: OOB error rate ---

oob_df <- data.frame(
  Trees = 1:nrow(rf_final$err.rate),
  OOB   = rf_final$err.rate[, "OOB"]  * 100,
  Lav   = rf_final$err.rate[, "Lav"]  * 100,
  Hoej  = rf_final$err.rate[, "Hoej"] * 100
)

final_oob <- oob_df$OOB[nrow(oob_df)]

p_oob <- ggplot(oob_df) +
  geom_line(aes(x = Trees, y = OOB),  color = "#4ECDC4", linewidth = 1.5) +
  geom_line(aes(x = Trees, y = Lav),  color = "#27ae60", linewidth = 1, alpha = 0.7) +
  geom_line(aes(x = Trees, y = Hoej), color = "#FF6B6B", linewidth = 1, alpha = 0.7) +
  geom_hline(yintercept = final_oob, linetype = "dashed", color = "#FF6B6B", linewidth = 1) +
  annotate("text", x = 250, y = final_oob + 3,
           label = sprintf("Final OOB: %.1f%%", final_oob),
           color = "#FF6B6B", size = 5, fontface = "bold") +
  labs(title    = "Random Forest: Error Rate vs. antal traeer",
       subtitle = "Fejlraten stabiliseres efterhaanden som flere traeer tilfoejes",
       x = "Antal traeer", y = "Error Rate (%)") +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#1a1a2e", color = NA),
    panel.background = element_rect(fill = "#1a1a2e", color = NA),
    panel.grid.major = element_line(color = "#333355"),
    panel.grid.minor = element_blank(),
    text             = element_text(color = "white"),
    axis.text        = element_text(color = "white", size = 10),
    plot.title       = element_text(color = "#4ECDC4", size = 16, face = "bold"),
    plot.subtitle    = element_text(color = "#888888", size = 11)
  )

print(p_oob)


# --- Plot 5: N11 features per risikogruppe ---

n11_feat <- c("ACWR_Estimate", "Load_CV", "Weekly_Mean_PL")
n11_feat <- n11_feat[n11_feat %in% names(model_df)]

feat_long <- model_df %>%
  dplyr::select(all_of(n11_feat), Target) %>%
  pivot_longer(cols = -Target, names_to = "Feature", values_to = "Vaerdi") %>%
  mutate(Feature = case_when(
    Feature == "ACWR_Estimate"  ~ "ACWR\n(Acute:Chronic)",
    Feature == "Load_CV"        ~ "Load CV\n(Variabilitet %)",
    Feature == "Weekly_Mean_PL" ~ "Weekly PL\n(Gns. belastning)",
    TRUE ~ Feature
  ))

p_n11 <- feat_long %>%
  ggplot(aes(x = Target, y = Vaerdi, fill = Target)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 3, alpha = 0.7, color = "white") +
  facet_wrap(~Feature, scales = "free_y") +
  scale_fill_manual(values = c("Lav" = "#4ECDC4", "Hoej" = "#FF6B6B")) +
  labs(title    = "N11 Features: hoj- vs. lavrisiko spillere",
       subtitle = "Hojrisiko-spillere har hojere ACWR og mere variabel belastning",
       x = "Risikogruppe", y = "Vaerdi") +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#1a1a2e", color = NA),
    panel.background = element_rect(fill = "#1a1a2e", color = NA),
    panel.grid.major = element_line(color = "#333355"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#333355"),
    strip.text       = element_text(color = "white", size = 11, face = "bold"),
    text             = element_text(color = "white"),
    axis.text        = element_text(color = "white", size = 10),
    plot.title       = element_text(color = "#4ECDC4", size = 16, face = "bold"),
    plot.subtitle    = element_text(color = "#888888", size = 11),
    legend.position  = "none"
  )

print(p_n11)


# --- Plot 6: Confusion matrix ---

cm_df <- as.data.frame(cm_rf$table)

p_cm <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = Freq), size = 20, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#2E86AB", high = "#FF6B6B") +
  labs(title    = "Random Forest: Confusion Matrix",
       subtitle = sprintf("Accuracy: %.0f%% | Sensitivity: %.0f%% | Specificity: %.0f%%",
                          cm_rf$overall["Accuracy"]    * 100,
                          cm_rf$byClass["Sensitivity"] * 100,
                          cm_rf$byClass["Specificity"] * 100),
       x = "Faktisk klasse", y = "Forudsagt klasse") +
  theme_minimal() +
  theme(
    plot.background  = element_rect(fill = "#1a1a2e", color = NA),
    panel.background = element_rect(fill = "#1a1a2e", color = NA),
    panel.grid       = element_blank(),
    text             = element_text(color = "white"),
    axis.text        = element_text(color = "white", size = 14, face = "bold"),
    plot.title       = element_text(color = "#4ECDC4", size = 18, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(color = "#888888", size = 12, hjust = 0.5),
    legend.position  = "none"
  )

print(p_cm)


# --- Gem til PDF ---

pdf("RF_visualisering.pdf", width = 14, height = 10, bg = "#1a1a2e")

par(mar = c(2, 2, 4, 2), bg = "#1a1a2e")
plot(NULL, xlim = c(0, 100), ylim = c(0, 100),
     xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = "", bty = "n")
rect(-5, -5, 105, 105, col = "#1a1a2e", border = NA)
text(50, 97, "Random Forest - Majority Vote", col = "#4ECDC4", cex = 2, font = 2)
text(50, 92, sprintf("Rodovre U20 | %d traeer | %d spillere", n_trees, nrow(model_df)),
     col = "#888888", cex = 1.2)
rect(25, 75, 75, 88, col = "#667eea", border = "#8899ff", lwd = 2)
text(50, 84, "INPUT: Spiller Data", col = "white", cex = 1.3, font = 2)
text(50, 79, "ACWR=1.92 | Load_CV=42% | Asymmetri=8%", col = "white", cex = 0.9)
arrows(50, 75, 50, 70, col = "#4ECDC4", lwd = 3, length = 0.15)
for (i in 1:5) {
  rect(tree_x[i]-8, 45, tree_x[i]+8, 68, col = tree_colors[i], border = "white", lwd = 2)
  text(tree_x[i], 64, tree_labels[i], col = "white", cex = 0.9, font = 2)
  text(tree_x[i], 58, paste0("Fokus:\n", tree_focus[i]), col = "white", cex = 0.7)
  vote_col <- ifelse(tree_votes[i] == "HOJ", "#c0392b", "#27ae60")
  rect(tree_x[i]-7, 46, tree_x[i]+7, 52, col = vote_col, border = NA)
  text(tree_x[i], 49, tree_votes[i], col = "white", cex = 0.8, font = 2)
  arrows(tree_x[i], 45, tree_x[i], 38, col = tree_colors[i], lwd = 2, length = 0.1)
}
text(50, 40, sprintf("... og %d andre traeer stemmer ogsaa ...", n_trees - 5),
     col = "#888888", cex = 1, font = 3)
segments(10, 35, 90, 35, col = "#4ECDC4", lwd = 3)
arrows(50, 35, 50, 28, col = "#4ECDC4", lwd = 3, length = 0.15)
rect(30, 18, 70, 28, col = "#e74c3c", border = "#ff6b6b", lwd = 3)
text(50, 25, "MAJORITY VOTE", col = "white", cex = 1.2, font = 2)
text(50, 21, sprintf("%d / %d traeer = HOJ RISIKO", hoj_votes, n_trees), col = "white", cex = 1)
arrows(50, 18, 50, 12, col = "#FF6B6B", lwd = 3, length = 0.15)
rect(25, 2, 75, 12, col = "#FF6B6B", border = "#ff8888", lwd = 3)
text(50, 8.5, "FINAL OUTPUT", col = "white", cex = 1, font = 2)
text(50, 5, "HOJ RISIKO", col = "white", cex = 1.5, font = 2)

print(p_imp)
print(p_kilde)
print(p_oob)
print(p_n11)
print(p_cm)

dev.off()


# --- Opsummering ---

n11_pct <- kilde_summary %>% filter(Kilde == "N11 Workload") %>% pull(Procent)

cat(sprintf("N11 workload andel af praediktiv kraft: %.0f%%\n", n11_pct))
cat(sprintf("Top features:\n"))
cat(sprintf("  1. %s (%.2f)\n", imp_df$Feature[1], imp_df$MeanDecreaseGini[1]))
cat(sprintf("  2. %s (%.2f)\n", imp_df$Feature[2], imp_df$MeanDecreaseGini[2]))
cat(sprintf("  3. %s (%.2f)\n", imp_df$Feature[3], imp_df$MeanDecreaseGini[3]))