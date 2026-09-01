# Loading libraries and packages
library("readxl")
library("dplyr")
library("tidyr")
library("effects")
library("stringr")
library("broom")
library("mgcv")
library("ggplot2")
library("randomForest")
library("iml")

# Load metadata
network_metadata <- read.csv("network_metadata.txt", sep="\t")

# Get directory and files
datasets_directory <- "./ElasticNetResults/"
files <- list.files(datasets_directory, pattern = NULL, all.files = FALSE, full.names = FALSE)

# Data cleaning and transformation
cleaned_data_list <- list()

for (file in files) {
  print(paste("Processing file:", file))
  
  # Read data
  data <- read.csv(paste0(datasets_directory, file), sep = ",")
  
  # Add metadata
  data <- merge(data, network_metadata, by = "Author")
  
  # Extract year of birth from birthDate
  data$birthyear <- as.numeric(str_extract(data$birthDate, "\\d{4}"))
  
  # Ternary birth country
  data <- data %>%
    mutate(
      birthCountry = case_when(
        birthCountry == "Nederland" ~ "Nederland",
        birthCountry == "België (Vlaanderen)" ~ "België",
        is.na(birthCountry) ~ "Other",
        TRUE ~ "Other"
      )
    )
  data$birthCountry <- as.factor(data$birthCountry)
  
  # Log scale for network variables
  data$Betweenness_log <- log(data$Betweenness + 0.001)
  data$In.degree_log <- log(data$In.degree + 0.001)
  data$Out.degree_log <- log(data$Out.degree + 0.001)
  data$Closeness_log <- log(data$Closeness + 0.001)
  
  # Compute binary closeness
  closeness_median <- median(data$Closeness, na.rm = TRUE)
  data$Closeness_binary <- ifelse(data$Closeness >= closeness_median, "Central", "Periphery")
  
  # Data cleaning and transformation
  data_clean <- data %>%
    # Remove rows where coefficient is exactly 0
    filter(coefficient != 0) %>%
    
    # Compute percentile of coefficients (0 to 1 scale)
    mutate(coefficient_percentile = percent_rank(coefficient))
  
  # Store the cleaned data
  cleaned_data_list[[file]] <- data_clean
}



# -------------------------
# Storage
# -------------------------
importance_list <- list()
ale_list <- list()
interaction_list <- list()
shap_list <- list()
shap_summary_list <- list()

# -------------------------
# Variables
# -------------------------
vars_used <- c(
  "birthyear",
  "birthCountry",
  "Closeness_log",
  "Out.degree_log",
  "Betweenness_log",
  "In.degree_log"
)

vars_to_plot <- c(
  "Out.degree_log",
  "Betweenness_log",
  "In.degree_log",
  "Closeness_log"
)

network_vars <- c(
  "Betweenness_log",
  "In.degree_log",
  "Out.degree_log",
  "Closeness_log"
)

# =========================
# MAIN LOOP
# =========================
for (file in names(cleaned_data_list)) {
  
  cat("\n====================\n", file, "\n====================\n")
  
  df <- cleaned_data_list[[file]]
  
  df <- df[complete.cases(df[, c("coefficient_percentile", vars_used)]), ]
  
  if (nrow(df) < 20) next
  
  # -------------------------
  # 1. FIT MODEL ONCE
  # -------------------------
  set.seed(123)
  
  rf_model <- randomForest(
    coefficient_percentile ~ .,
    data = df[, c("coefficient_percentile", vars_used)],
    ntree = 500,
    importance = TRUE
  )
  
  print(rf_model)
  
  X <- df[, vars_used]
  
  predictor <- Predictor$new(
    model = rf_model,
    data = X,
    y = df$coefficient_percentile
  )
  
  # =========================================================
  # 2. VARIABLE IMPORTANCE (%IncMSE from randomForest)
  # =========================================================
  importance_df <- as.data.frame(importance(rf_model))
  
  importance_df$feature <- rownames(importance_df)
  
  importance_df <- importance_df %>%
    dplyr::select(feature, `%IncMSE`) %>%
    rename(importance = `%IncMSE`) %>%
    mutate(Dataset = file)
  
  importance_list[[file]] <- importance_df
  
  # =========================================================
  # 3. ALE
  # =========================================================
  for (v in vars_to_plot) {
    
    fe <- FeatureEffect$new(
      predictor,
      feature = v,
      method = "ale",
      grid.size = 50
    )
    
    ale_df <- fe$results
    ale_df$x <- ale_df[[setdiff(names(ale_df), c(".type", ".value"))]]
    
    ale_df$Variable <- v
    ale_df$Dataset <- file
    
    ale_list[[paste(file, v, sep = "_")]] <- ale_df
  }
  
  # =========================================================
  # 4. INTERACTION STRENGTH (H-stat)
  # =========================================================
  inter <- Interaction$new(predictor)
  
  interaction_list[[file]] <- inter$results %>%
    mutate(Dataset = file)
  
  # =========================================================
  # 5. SHAP
  # =========================================================
  
  shap_store <- list()
  
  for (i in seq_len(nrow(X))) {
    
    shap_i <- Shapley$new(
      predictor,
      x.interest = X[i, , drop = FALSE]
    )
    
    df_i <- shap_i$results %>%
      mutate(
        obs = i,
        Dataset = file
      )
    
    shap_store[[length(shap_store) + 1]] <- df_i
  }
  
  shap_df <- bind_rows(shap_store)
  
  # clean structure
  shap_df <- shap_df %>%
    tidyr::separate(
      feature.value,
      into = c("feature", "feature_value"),
      sep = "=",
      extra = "merge",
      fill = "right"
    )
  
  shap_list[[file]] <- shap_df
  
  shap_summary_list[[file]] <- shap_df %>%
    group_by(feature) %>%
    summarise(
      mean_abs_shap = mean(abs(phi), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Dataset = file)
}

importance_all <- bind_rows(importance_list)
ale_all <- bind_rows(ale_list)
interaction_all <- bind_rows(interaction_list)
shap_summary_all <- bind_rows(shap_summary_list)
shap_all <- bind_rows(shap_list)

# Define your mapping from variable names to pretty labels
label_map <- c(
  birthyear = "Year of Birth",
  birthCountry = "Country of Birth",
  Closeness_log = "Closeness",
  Out.degree_log = "Out-Degree",
  Betweenness_log = "Betweenness",
  In.degree_log = "In-Degree"
)

###### SHAP Visualization ####

# add pretty labels (safe mapping)
shap_all$Variable_pretty <- label_map[shap_all$feature]

shap_all$Variable_pretty <- ifelse(
  is.na(shap_all$Variable_pretty),
  shap_all$feature,
  shap_all$Variable_pretty
)

# clean dataset names
shap_all$Dataset <- gsub("_ElasticNetCoefficients\\.csv$", "", shap_all$Dataset)

# SHAP values birthyear
ggplot(
  subset(shap_all, feature == "birthyear"),
  aes(
    x = phi,
    y = Variable_pretty,
    color = as.numeric(feature_value)
  )
) +
  geom_jitter(height = 0.25, alpha = 0.6, size = 1.5) +
  scale_color_viridis_c() +
  facet_wrap(~Dataset) +
  labs(
    x = "SHAP value",
    y = NULL,
    color = "Feature value",
    title = "SHAP Summary Plot — Birth Year"
  ) +
  theme_bw()

# SHAP values network variables
ggplot(
  subset(shap_all, feature != "birthyear" & feature != "birthCountry"),
  aes(
    x = phi,
    y = Variable_pretty,
    color = as.numeric(feature_value)
  )
) +
  geom_jitter(height = 0.25, alpha = 0.6, size = 1.5) +
  #scale_color_viridis_c() +
  scale_color_gradient(
    low = "blue",
    high = "red"
  ) +
  facet_wrap(~Dataset) +
  labs(
    x = "SHAP value",
    y = NULL,
    color = "Feature value",
    title = "SHAP Summary Plot — Other Variables"
  ) +
  theme_bw()

# SHAP dependence plot: out-degree 
ggplot(
  subset(shap_all, feature == "Out.degree_log"),
  aes(
    x = as.numeric(feature_value),
    y = phi
  )
) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Feature value",
    y = "SHAP value",
    title = "SHAP Dependence Plot: Out-degree"
  ) +
  facet_wrap(~Dataset) +
  theme_bw()

# SHAP dependence plot: in-degree 
ggplot(
  subset(shap_all, feature == "In.degree_log"),
  aes(
    x = as.numeric(feature_value),
    y = phi
  )
) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Feature value",
    y = "SHAP value",
    title = "SHAP Dependence Plot: In-degree"
  ) +
  facet_wrap(~Dataset) +
  theme_bw()

# SHAP dependence plot: Betweenness 
ggplot(
  subset(shap_all, feature == "Betweenness_log"),
  aes(
    x = as.numeric(feature_value),
    y = phi
  )
) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Feature value",
    y = "SHAP value",
    title = "SHAP Dependence Plot: Betweenness"
  ) +
  facet_wrap(~Dataset) +
  theme_bw()

# SHAP dependence plot: Closeness 
ggplot(
  subset(shap_all, feature == "Closeness_log"),
  aes(
    x = as.numeric(feature_value),
    y = phi
  )
) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Feature value",
    y = "SHAP value",
    title = "SHAP Dependence Plot: Closeness"
  ) +
  facet_wrap(~Dataset) +
  theme_bw()

# SHAP dependence plot: Year of Birth 
ggplot(
  subset(shap_all, feature == "birthyear"),
  aes(
    x = as.numeric(feature_value),
    y = phi
  )
) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Feature value",
    y = "SHAP value",
    title = "SHAP Dependence Plot: Year of Birth"
  ) +
  facet_wrap(~Dataset) +
  theme_bw()

# SHAP dependence plot: Country of Birth
ggplot(
  subset(shap_all, feature == "birthCountry"),
  aes(
    x = feature_value,
    y = phi
  )
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +
  labs(
    x = "Country of Birth",
    y = "SHAP value",
    title = "SHAP Dependence Plot: Country of Birth"
  ) +
  facet_wrap(~Dataset) +
  theme_bw()

# Overall SHAP Dependence plot
ggplot(
  subset(shap_all,
         feature != "birthyear" &
           feature != "birthCountry"),
  aes(
    x = as.numeric(feature_value),
    y = phi
  )
) +
  geom_point(alpha = 0.4, size = 1) +
  facet_wrap(~ Variable_pretty, scales = "free_x") +
  labs(
    x = "Feature value",
    y = "SHAP value",
    title = "SHAP Dependence Plots: centrality measures across all datasets"
  ) +
  theme_bw()

# Compute correlation
shap_cor <- shap_all %>%
  filter(
    feature != "birthyear",
    feature != "birthCountry"
  ) %>%
  group_by(feature, Variable_pretty, Dataset) %>%
  summarise(
    cor = cor(
      as.numeric(feature_value),
      phi,
      use = "complete.obs"
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(cor)))

shap_cor


# Correlation plot
ggplot(
  shap_cor,
  aes(
    x = cor,
    y = reorder(Variable_pretty, cor),
    fill = cor
  )
) +
  geom_col() +
  facet_wrap(~ Dataset) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    x = "Correlation(feature value, SHAP value)",
    y = NULL
  ) +
  theme_bw()

##### ALE Visualization ####

# clean dataset labels
ale_all$Dataset <- gsub("_ElasticNetCoefficients\\.csv$", "", ale_all$Dataset)

ggplot(ale_all, aes(x = x, y = .value)) +
  geom_line() +
  facet_grid(Variable ~ Dataset, scales = "free_x") +
  labs(
    title = "ALE Effects by Variable and Dataset",
    x = "Predictor value",
    y = "ALE effect"
  ) +
  theme_bw()

#### Variable importance visualization ####

# Clean dataset names
importance_all$Dataset <- gsub("_ElasticNetCoefficients\\.csv$", "", importance_all$Dataset)

# Add pretty labels for features
importance_all$feature_pretty <- label_map[importance_all$feature]

# Fallback to original feature name if not found in label_map
importance_all$feature_pretty <- ifelse(
  is.na(importance_all$feature_pretty),
  importance_all$feature,
  importance_all$feature_pretty
)

ggplot(importance_all,
       aes(x = reorder(feature_pretty, importance),
           y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~Dataset, scales = "free_y") +
  labs(
    title = "Variable importance by dataset",
    x = NULL,
    y = "% increase in MSE"
  ) +
  theme_minimal()

avg_importance <- importance_all %>%
  group_by(feature, feature_pretty) %>%
  summarise(
    mean_imp = mean(importance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_imp))

ggplot(avg_importance,
       aes(x = mean_imp,
           y = reorder(feature_pretty, mean_imp))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average variable importance across all datasets",
    x = "Average % increase in MSE",
    y = NULL
  ) +
  theme_minimal()

#### Interactions visualization ####

# Clean dataset names if needed
interaction_all$Dataset <- gsub("_ElasticNetCoefficients\\.csv$", "", interaction_all$Dataset)

# Add pretty labels
interaction_all$feature_pretty <- label_map[interaction_all$.feature]
interaction_all$feature_pretty <- ifelse(
  is.na(interaction_all$feature_pretty),
  interaction_all$.feature,
  interaction_all$feature_pretty
)

# Plot interaction strength by dataset
ggplot(interaction_all,
       aes(x = reorder(feature_pretty, .interaction),
           y = .interaction)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~Dataset, scales = "free_y") +
  labs(
    title = "Variable interaction strength by dataset",
    x = NULL,
    y = "H-statistic"
  ) +
  theme_minimal()

# Compute average interaction strength across datasets
interaction_avg <- interaction_all %>%
  group_by(.feature, feature_pretty) %>%
  summarise(
    mean_H = mean(.interaction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_H))

# Plot average interaction strength
ggplot(interaction_avg,
       aes(x = mean_H,
           y = reorder(feature_pretty, mean_H))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average variable interaction strength across all datasets",
    x = "Average H-statistic",
    y = NULL
  ) +
  theme_minimal()


#### Pearson correlation test H-statistic and Importance ####
cor.test(
  importance_all %>%
    group_by(feature) %>%
    summarise(mean_importance = mean(importance, na.rm = TRUE)) %>%
    pull(mean_importance),
  
  interaction_all %>%
    group_by(.feature) %>%
    summarise(mean_H = mean(.interaction, na.rm = TRUE)) %>%
    pull(mean_H),
  
  method = "pearson"
)

importance_H <- importance_all %>%
  dplyr::select(Dataset, feature, importance) %>%
  dplyr::inner_join(
    interaction_all %>%
      dplyr::select(Dataset, .feature, .interaction),
    by = c("Dataset", "feature" = ".feature")
  )

cor.test(
  importance_H$importance,
  importance_H$.interaction,
  method = "pearson"
)
