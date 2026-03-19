# Klassifikationsmodeller - Rodovre U20
# Logistisk regression, Decision Tree og Random Forest

if ("package:MASS" %in% search()) detach("package:MASS", unload = TRUE)

required_pkgs <- c("tidyverse", "caret", "randomForest", "rpart",
                   "rpart.plot", "pROC", "gridExtra", "grid", "ggtext", "scales")

for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

select <- dplyr::select
filter <- dplyr::filter

# Farvepalet
COL_BG      <- "#FAFAFA"
COL_PANEL   <- "#FFFFFF"
COL_GRID    <- "#EBEBEB"
COL_TEXT    <- "#1A1A2E"
COL_SUBTEXT <- "#555577"
COL_ACCENT  <- "#2E6BE6"
COL_HOEJ    <- "#E63946"
COL_LAV     <- "#2A9D8F"
COL_WARN    <- "#F4A261"
COL_N11     <- "#E63946"
COL_DIU     <- "#457B9D"
COL_STYRKE  <- "#2A9D8F"
COL_HUDL    <- "#A8DADC"
COL_DEMO    <- "#F4A261"

theme_rodovre <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background    = element_rect(fill = COL_BG, color = NA),
      panel.background   = element_rect(fill = COL_PANEL, color = NA),
      panel.grid.major   = element_line(color = COL_GRID, linewidth = 0.5),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(color = "#DDDDDD", fill = NA, linewidth = 0.8),
      plot.title         = element_text(color = COL_TEXT, size = base_size + 4,
                                        face = "bold", margin = margin(b = 6)),
      plot.subtitle      = element_text(color = COL_SUBTEXT, size = base_size - 1,
                                        margin = margin(b = 10)),
      plot.caption       = element_text(color = COL_SUBTEXT, size = base_size - 3,
                                        hjust = 0, margin = margin(t = 8)),
      axis.title         = element_text(color = COL_TEXT, size = base_size - 1),
      axis.text          = element_text(color = COL_TEXT, size = base_size - 2),
      strip.background   = element_rect(fill = "#EEF2FF", color = NA),
      strip.text         = element_text(color = COL_TEXT, face = "bold",
                                        size = base_size - 1),
      legend.background  = element_rect(fill = COL_BG, color = NA),
      legend.key         = element_rect(fill = COL_BG, color = NA),
      legend.title       = element_text(color = COL_TEXT, face = "bold"),
      legend.text        = element_text(color = COL_TEXT),
      legend.position    = "bottom",
      plot.margin        = margin(16, 16, 12, 16)
    )
}

theme_set(theme_rodovre())


# --- Data integration ---

n11_navne <- ls(pattern = "^rod(24|25)_", envir = .GlobalEnv)

all_n11 <- bind_rows(lapply(n11_navne, function(navn) {
  df <- get(navn, envir = .GlobalEnv)
  if ("Player Tag" %in% names(df))  df$Spiller_Navn <- df$`Player Tag`
  if ("Spiller_Navn" %in% names(df)) return(df)
  return(NULL)
}))

n11_agg <- all_n11 %>%
  filter(!is.na(Spiller_Navn)) %>%
  group_by(Spiller_Navn) %>%
  summarise(
    N11_Sessions      = n(),
    Weekly_Mean_PL    = mean(`Total Player Load`, na.rm = TRUE),
    Weekly_SD_PL      = sd(`Total Player Load`, na.rm = TRUE),
    Mean_Load_Per_Min = mean(`Load/Min`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Load_CV       = ifelse(Weekly_Mean_PL > 0,
                           Weekly_SD_PL / Weekly_Mean_PL * 100, 25),
    ACWR_Estimate = ifelse(Weekly_Mean_PL > 0 & !is.na(Weekly_SD_PL),
                           (Weekly_Mean_PL + Weekly_SD_PL) / Weekly_Mean_PL, 1.0)
  )

fysio <- get("Rodovre_Komplet_Fysio_Profil", envir = .GlobalEnv)

model_data_raw <- fysio %>%
  inner_join(n11_agg, by = "Spiller_Navn") %>%
  filter(
    !is.na(DIU_Leg_Asymmetry_pct) | !is.na(DIU_Z_Score),
    !is.na(Weekly_Mean_PL),
    !is.na(Hudl_Kampe_Spillet)    | !is.na(Hudl_Total_Points)
  )


# --- Feature engineering og målvariabel ---

model_data <- model_data_raw %>%
  mutate(
    DIU_Leg_Asymmetry_pct = replace_na(DIU_Leg_Asymmetry_pct,
                                       median(DIU_Leg_Asymmetry_pct, na.rm = TRUE)),
    DIU_Z_Score           = replace_na(DIU_Z_Score, 0),
    Load_CV               = replace_na(Load_CV, median(Load_CV, na.rm = TRUE)),
    ACWR_Estimate         = replace_na(ACWR_Estimate, 1.0),
    Pull_ups_Udv          = replace_na(Pull_ups_Udvikling, 0),
    Dips_Udv              = replace_na(Dips_Udvikling, 0),
    Laengdehop_Udv        = replace_na(Længdehop_Udvikling, 0),
    Neg_Udvikling         = (Pull_ups_Udv < 0) + (Dips_Udv < 0) + (Laengdehop_Udv < 0),
    
    Hoejrisiko = case_when(
      ACWR_Estimate         > 1.8   ~ 1L,
      DIU_Leg_Asymmetry_pct > 10    ~ 1L,
      Neg_Udvikling         >= 3    ~ 1L,
      !is.na(Injury_Risk) & Injury_Risk == "Høj" ~ 1L,
      TRUE ~ 0L
    ),
    Target = factor(Hoejrisiko, levels = c(0, 1), labels = c("Lav", "Hoej"))
  )

print(table(model_data$Target))

features <- c("DIU_Leg_Asymmetry_pct", "DIU_Z_Score",
              "Rel_Bench_2024", "Rel_Deadlift_2024", "Rel_Squat_2024",
              "Weekly_Mean_PL", "Load_CV", "Mean_Load_Per_Min", "ACWR_Estimate",
              "Hudl_Points_Per_Game", "Hudl_Kampe_Spillet",
              "Alder_2024", "BMI")
features <- features[features %in% names(model_data)]

model_df <- model_data %>%
  dplyr::select(Spiller_Navn, all_of(features), Hoejrisiko, Target) %>%
  mutate(across(all_of(features), ~replace_na(., median(., na.rm = TRUE))))


# --- Train/test split ---

set.seed(42)
n_lav  <- sum(model_df$Hoejrisiko == 0)
n_hoej <- sum(model_df$Hoejrisiko == 1)

if (min(n_lav, n_hoej) >= 3) {
  train_idx <- createDataPartition(model_df$Target, p = 0.75, list = FALSE)
  train_df  <- model_df[train_idx, ]
  test_df   <- model_df[-train_idx, ]
} else {
  train_df  <- model_df
  test_df   <- model_df
}

X_train <- train_df %>% dplyr::select(all_of(features))
y_train <- train_df$Target
X_test  <- test_df  %>% dplyr::select(all_of(features))
y_test  <- test_df$Target


# --- Tren modeller ---

ctrl <- trainControl(
  method          = "LOOCV",
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

# Logistisk regression
preProc     <- preProcess(X_train, method = c("center", "scale", "nzv"))
X_train_sc  <- predict(preProc, X_train)
X_test_sc   <- predict(preProc, X_test)
train_glm   <- bind_cols(X_train_sc, Target = y_train)
test_glm    <- bind_cols(X_test_sc,  Target = y_test)

glm_model <- tryCatch({
  train(Target ~ ., data = train_glm, method = "glm", family = "binomial",
        trControl = ctrl, metric = "ROC")
}, error = function(e) NULL)

# Decision tree
train_tree <- bind_cols(X_train, Target = y_train)
test_tree  <- bind_cols(X_test,  Target = y_test)

tree_model <- train(
  Target ~ ., data = train_tree, method = "rpart",
  trControl = ctrl,
  tuneGrid  = expand.grid(cp = seq(0.01, 0.5, 0.05)),
  metric    = "ROC"
)

# Random forest
train_rf <- bind_cols(X_train, Target = y_train)
test_rf  <- bind_cols(X_test,  Target = y_test)

rf_model <- train(
  Target ~ ., data = train_rf, method = "rf",
  trControl = ctrl,
  tuneGrid  = expand.grid(mtry = 2:min(6, ncol(X_train))),
  metric    = "ROC", ntree = 300, importance = TRUE
)


# --- Evaluering ---

if (!is.null(glm_model)) {
  glm_pred <- predict(glm_model, test_glm)
  glm_prob <- predict(glm_model, test_glm, type = "prob")
  cm_glm   <- confusionMatrix(glm_pred, y_test, positive = "Hoej")
} else {
  cm_glm <- NULL; glm_prob <- NULL
}

tree_pred <- predict(tree_model, test_tree)
tree_prob <- predict(tree_model, test_tree, type = "prob")
cm_tree   <- confusionMatrix(tree_pred, y_test, positive = "Hoej")

rf_pred <- predict(rf_model, test_rf)
rf_prob <- predict(rf_model, test_rf, type = "prob")
cm_rf   <- confusionMatrix(rf_pred, y_test, positive = "Hoej")

sammenligning <- data.frame(
  Model       = c("Logistisk Regression", "Decision Tree", "Random Forest"),
  Accuracy    = round(c(ifelse(!is.null(cm_glm), cm_glm$overall["Accuracy"], NA),
                        cm_tree$overall["Accuracy"], cm_rf$overall["Accuracy"]), 3),
  Sensitivity = round(c(ifelse(!is.null(cm_glm), cm_glm$byClass["Sensitivity"], NA),
                        cm_tree$byClass["Sensitivity"], cm_rf$byClass["Sensitivity"]), 3),
  Specificity = round(c(ifelse(!is.null(cm_glm), cm_glm$byClass["Specificity"], NA),
                        cm_tree$byClass["Specificity"], cm_rf$byClass["Specificity"]), 3)
)

print(sammenligning, row.names = FALSE)

# ROC / AUC
roc_list <- list()
auc_vals <- c()
if (!is.null(glm_prob)) {
  roc_list$GLM    <- roc(y_test, glm_prob$Hoej,  levels = c("Lav","Hoej"), direction = "<", quiet = TRUE)
  auc_vals["GLM"] <- round(auc(roc_list$GLM), 3)
}
roc_list$Tree    <- roc(y_test, tree_prob$Hoej, levels = c("Lav","Hoej"), direction = "<", quiet = TRUE)
roc_list$RF      <- roc(y_test, rf_prob$Hoej,   levels = c("Lav","Hoej"), direction = "<", quiet = TRUE)
auc_vals["Tree"] <- round(auc(roc_list$Tree), 3)
auc_vals["RF"]   <- round(auc(roc_list$RF), 3)


# --- Baseline (kun fysio-data) ---

baseline_features <- c("DIU_Leg_Asymmetry_pct","DIU_Z_Score",
                       "Rel_Bench_2024","Rel_Deadlift_2024","Rel_Squat_2024",
                       "Alder_2024","BMI")
baseline_features <- baseline_features[baseline_features %in% names(fysio)]

baseline_data <- fysio %>%
  mutate(
    DIU_Leg_Asymmetry_pct = replace_na(DIU_Leg_Asymmetry_pct,
                                       median(DIU_Leg_Asymmetry_pct, na.rm = TRUE)),
    ACWR_Estimate = 1.0,
    Hoejrisiko = case_when(
      DIU_Leg_Asymmetry_pct > 10   ~ 1L,
      DIU_Z_Score           < -1.5 ~ 1L,
      !is.na(Injury_Risk) & Injury_Risk == "Høj" ~ 1L,
      TRUE ~ 0L
    ),
    Target = factor(Hoejrisiko, levels = c(0, 1), labels = c("Lav", "Hoej"))
  ) %>%
  dplyr::select(all_of(baseline_features), Hoejrisiko, Target) %>%
  mutate(across(all_of(baseline_features), ~replace_na(., median(., na.rm = TRUE)))) %>%
  filter(!is.na(Hoejrisiko))

baseline_auc <- 0.5
if (sum(baseline_data$Hoejrisiko == 1) >= 2) {
  baseline_rf <- tryCatch({
    train(Target ~ . - Hoejrisiko, data = baseline_data, method = "rf",
          trControl = trainControl(method = "cv", number = 5, classProbs = TRUE,
                                   summaryFunction = twoClassSummary),
          metric = "ROC", ntree = 300)
  }, error = function(e) NULL)
  if (!is.null(baseline_rf))
    baseline_auc <- max(baseline_rf$results$ROC, na.rm = TRUE)
}

forbedring <- (auc_vals["RF"] - baseline_auc) * 100


# --- Plots ---

rf_final  <- rf_model$finalModel
n_trees   <- rf_final$ntree
oob_error <- rf_final$err.rate[nrow(rf_final$err.rate), "OOB"]
høj_votes <- round(n_trees * (1 - oob_error) * 0.82)
lav_votes <- n_trees - høj_votes

# Plot 1 - ROC kurver
roc_df <- bind_rows(lapply(names(roc_list), function(nm) {
  r <- roc_list[[nm]]
  data.frame(
    FPR    = 1 - r$specificities,
    TPR    = r$sensitivities,
    Model  = paste0(nm, " (AUC=", auc_vals[nm], ")")
  )
}))

model_cols <- setNames(
  c(COL_ACCENT, COL_WARN, COL_HOEJ)[seq_along(names(roc_list))],
  unique(roc_df$Model)
)

p1_roc <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "#AAAAAA", linewidth = 0.8) +
  geom_line(linewidth = 1.4) +
  scale_color_manual(values = model_cols) +
  labs(
    title    = "ROC Kurver - Modelsammenligning",
    subtitle = sprintf("Testsaet: %d spillere - Bedste model: RF AUC = %.3f",
                       nrow(test_df), auc_vals["RF"]),
    x        = "False Positive Rate (1 - Specificitet)",
    y        = "True Positive Rate (Sensitivitet)",
    color    = NULL,
    caption  = "Rodovre U20 - Skaderisiko-klassifikation"
  ) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(p1_roc)

# Plot 2 - Feature importance
imp_df <- data.frame(
  Feature           = rownames(importance(rf_final)),
  MeanDecreaseGini  = importance(rf_final)[, "MeanDecreaseGini"]
) %>%
  mutate(
    Kilde = case_when(
      Feature %in% c("ACWR_Estimate","Load_CV","Weekly_Mean_PL","Mean_Load_Per_Min") ~ "N11 Workload",
      Feature %in% c("DIU_Leg_Asymmetry_pct","DIU_Z_Score")                         ~ "DIU Test",
      Feature %in% c("Rel_Bench_2024","Rel_Deadlift_2024","Rel_Squat_2024")          ~ "Styrke",
      Feature %in% c("Hudl_Points_Per_Game","Hudl_Kampe_Spillet")                    ~ "Hudl",
      TRUE                                                                            ~ "Demografi"
    )
  ) %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(12)

kilde_cols <- c(
  "N11 Workload" = COL_N11,
  "DIU Test"     = COL_DIU,
  "Styrke"       = COL_STYRKE,
  "Hudl"         = COL_HUDL,
  "Demografi"    = COL_DEMO
)

p2_imp <- imp_df %>%
  mutate(Feature = fct_reorder(Feature, MeanDecreaseGini)) %>%
  ggplot(aes(x = Feature, y = MeanDecreaseGini, fill = Kilde)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", MeanDecreaseGini)),
            hjust = -0.15, size = 3.2, color = COL_TEXT) +
  coord_flip() +
  scale_fill_manual(values = kilde_cols) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Random Forest - Feature Importance",
    subtitle = "Mean Decrease Gini - farver angiver datakilde",
    x        = NULL, y = "Importance Score",
    fill     = "Datakilde",
    caption  = "Rodovre U20 - Random Forest (ntree=300)"
  )

print(p2_imp)

# Plot 3 - Datakilde-bidrag
kilde_summary <- imp_df %>%
  group_by(Kilde) %>%
  summarise(Total = sum(MeanDecreaseGini), .groups = "drop") %>%
  mutate(Procent = Total / sum(Total) * 100) %>%
  arrange(desc(Procent))

p3_kilde <- kilde_summary %>%
  mutate(Kilde = fct_reorder(Kilde, Procent)) %>%
  ggplot(aes(x = Kilde, y = Procent, fill = Kilde)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f%%", Procent)),
            hjust = -0.15, size = 5, fontface = "bold", color = COL_TEXT) +
  coord_flip() +
  scale_fill_manual(values = kilde_cols) +
  scale_y_continuous(limits = c(0, 105),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Datakilde-bidrag til Random Forest",
    subtitle = "Procentvis andel af samlet praediktiv kraft",
    x        = NULL, y = "Andel (%)",
    caption  = "Rodovre U20"
  ) +
  theme(legend.position = "none")

print(p3_kilde)

# Plot 4 - OOB error rate
oob_df <- data.frame(
  Trees = 1:nrow(rf_final$err.rate),
  OOB   = rf_final$err.rate[, "OOB"]  * 100,
  Lav   = rf_final$err.rate[, "Lav"]  * 100,
  Hoej  = rf_final$err.rate[, "Hoej"] * 100
) %>% pivot_longer(-Trees, names_to = "Klasse", values_to = "Error")

final_oob <- rf_final$err.rate[nrow(rf_final$err.rate), "OOB"] * 100

oob_cols  <- c("OOB" = COL_TEXT, "Lav" = COL_LAV, "Hoej" = COL_HOEJ)
oob_sizes <- c("OOB" = 1.5, "Lav" = 1.0, "Hoej" = 1.0)

p4_oob <- ggplot(oob_df, aes(x = Trees, y = Error, color = Klasse, linewidth = Klasse)) +
  geom_line(alpha = 0.85) +
  geom_hline(yintercept = final_oob, linetype = "dashed",
             color = COL_ACCENT, linewidth = 0.9) +
  annotate("label", x = n_trees * 0.72, y = final_oob + 4,
           label = sprintf("Final OOB: %.1f%%", final_oob),
           color = COL_ACCENT, fill = COL_BG, size = 3.5,
           label.size = 0.3, fontface = "bold") +
  scale_color_manual(values = oob_cols, labels = c("Hoj risiko","Lav risiko","OOB (samlet)")) +
  scale_linewidth_manual(values = oob_sizes, guide = "none") +
  labs(
    title    = "OOB Error Rate - Antal Traeer",
    subtitle = "Fejlraten stabiliseres efterhaanden som skoven vokser",
    x        = "Antal traeer", y = "Error Rate (%)",
    color    = NULL,
    caption  = "OOB = Out-of-Bag - Random Forest intern cross-validation"
  )

print(p4_oob)

# Plot 5 - N11 features per risikogruppe
n11_feat <- c("ACWR_Estimate","Load_CV","Weekly_Mean_PL","Mean_Load_Per_Min")
n11_feat <- n11_feat[n11_feat %in% names(model_df)]

feat_labels <- c(
  "ACWR_Estimate"     = "ACWR (Acute:Chronic Ratio)",
  "Load_CV"           = "Load CV - Variabilitet (%)",
  "Weekly_Mean_PL"    = "Weekly Player Load (gns.)",
  "Mean_Load_Per_Min" = "Load per Minut"
)

feat_long_n11 <- model_df %>%
  dplyr::select(all_of(n11_feat), Target) %>%
  pivot_longer(-Target, names_to = "Feature", values_to = "Vaerdi") %>%
  mutate(Feature = recode(Feature, !!!feat_labels))

p5_n11 <- ggplot(feat_long_n11, aes(x = Target, y = Vaerdi, fill = Target)) +
  geom_boxplot(alpha = 0.75, outlier.shape = 21, outlier.fill = "white",
               outlier.size = 2, linewidth = 0.6) +
  geom_jitter(width = 0.18, size = 2.2, alpha = 0.6,
              aes(color = Target), show.legend = FALSE) +
  facet_wrap(~Feature, scales = "free_y", ncol = 2) +
  scale_fill_manual(values  = c("Lav" = COL_LAV, "Hoej" = COL_HOEJ),
                    labels  = c("Lav" = "Lav risiko", "Hoej" = "Hoj risiko")) +
  scale_color_manual(values = c("Lav" = COL_LAV, "Hoej" = COL_HOEJ)) +
  scale_x_discrete(labels = c("Lav" = "Lav\nrisiko", "Hoej" = "Hoj\nrisiko")) +
  labs(
    title    = "N11 Workload Features - Risikogrupper",
    subtitle = "Fordelingen af de vigtigste workload-indikatorer",
    x        = NULL, y = NULL, fill = NULL,
    caption  = "Rodovre U20 - Alle fire N11-features sammenlignet"
  )

print(p5_n11)

# Plot 6 - Confusion matrices
make_cm_plot <- function(cm, model_name, acc_col = COL_ACCENT) {
  cm_df <- as.data.frame(cm$table)
  acc   <- cm$overall["Accuracy"]
  sens  <- cm$byClass["Sensitivity"]
  spec  <- cm$byClass["Specificity"]
  
  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white", linewidth = 2.5) +
    geom_text(aes(label = Freq), size = 12, fontface = "bold", color = "white") +
    scale_fill_gradient(low = "#C8DFF2", high = acc_col) +
    labs(
      title    = model_name,
      subtitle = sprintf("Acc %.0f%% - Sens %.0f%% - Spec %.0f%%",
                         acc*100, sens*100, spec*100),
      x = "Faktisk", y = "Forudsagt"
    ) +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5),
      legend.position = "none",
      panel.grid      = element_blank()
    )
}

cm_plots <- list()
if (!is.null(cm_glm))
  cm_plots[["GLM"]]  <- make_cm_plot(cm_glm,  "Logistisk Regression", COL_ACCENT)
cm_plots[["Tree"]]   <- make_cm_plot(cm_tree, "Decision Tree",        COL_WARN)
cm_plots[["RF"]]     <- make_cm_plot(cm_rf,   "Random Forest",        COL_HOEJ)

grid.arrange(
  grobs = cm_plots, ncol = length(cm_plots),
  top   = textGrob("Confusion Matrices - Alle Modeller",
                   gp = gpar(fontsize = 14, fontface = "bold", col = COL_TEXT))
)

# Plot 7 - Performance sammenligning
model_cols2 <- c(
  "Logistisk Regression" = COL_ACCENT,
  "Decision Tree"        = COL_WARN,
  "Random Forest"        = COL_HOEJ
)

perf_long <- sammenligning %>%
  pivot_longer(-Model, names_to = "Metrik", values_to = "Vaerdi") %>%
  filter(!is.na(Vaerdi)) %>%
  mutate(Metrik = factor(Metrik, levels = c("Accuracy","Sensitivity","Specificity")))

p7_perf <- ggplot(perf_long, aes(x = Metrik, y = Vaerdi, fill = Model)) +
  geom_col(position = position_dodge(0.75), width = 0.68,
           color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", Vaerdi)),
            position = position_dodge(0.75),
            vjust = -0.45, size = 3, fontface = "bold", color = COL_TEXT) +
  scale_fill_manual(values = model_cols2) +
  scale_y_continuous(limits = c(0, 1.14), labels = percent_format(),
                     breaks = seq(0, 1, 0.2)) +
  labs(
    title    = "Modelperformance - Sammenligning",
    subtitle = sprintf("Testsaet: %d spillere - RF AUC = %.3f",
                       nrow(test_df), auc_vals["RF"]),
    x        = NULL, y = "Score", fill = NULL,
    caption  = "Rodovre U20 - Klassifikationsmodeller"
  )

print(p7_perf)

# Plot 8 - Decision tree
rpart.plot(
  tree_model$finalModel,
  type        = 4, extra = 104, under = TRUE,
  main        = "Skaderisiko-klassifikation (Decision Tree)",
  box.palette = list(Lav = "#D4EDDA", Hoej = "#F8D7DA"),
  col         = COL_TEXT,
  shadow.col  = "gray80"
)


# --- Gem output ---

pdf("Rodovre_U20_Klassifikation_FINAL.pdf", width = 13, height = 9, bg = COL_BG)

print(p1_roc)
print(p2_imp)
print(p3_kilde)
print(p4_oob)
print(p5_n11)
grid.arrange(
  grobs = cm_plots, ncol = length(cm_plots),
  top   = textGrob("Confusion Matrices - Alle Modeller",
                   gp = gpar(fontsize = 14, fontface = "bold", col = COL_TEXT))
)
print(p7_perf)
rpart.plot(tree_model$finalModel, type = 4, extra = 104, under = TRUE,
           main        = "Skaderisiko-klassifikation (Decision Tree)",
           box.palette = list(Lav = "#D4EDDA", Hoej = "#F8D7DA"))

dev.off()

write.csv(sammenligning, "Rodovre_U20_model_sammenligning.csv", row.names = FALSE)