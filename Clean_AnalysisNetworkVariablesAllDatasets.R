# Loading libraries and packages
library("readxl")
library("dplyr")
library("tidyr")
library("effects")
library("stringr")
library("broom")
library("mgcv")
library("ggplot2")

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
  
  # Log scale for network variables
  data$Betweenness_log <- log(data$Betweenness + 0.001)
  data$In.degree_log <- log(data$In.degree + 0.001)
  data$Out.degree_log <- log(data$Out.degree + 0.001)
  
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


#### Random Forest model ####

# Load required packages
library(randomForest)
library(caret)

# List of predictor variables
vars_used <- c(
  "birthyear",
  "Closeness_binary",
  "Out.degree_log",
  "Betweenness_log",
  "In.degree_log"
)

# Define your mapping from variable names to pretty labels
label_map <- c(
  birthyear = "Year of Birth",
  Closeness_binary = "Closeness",
  Out.degree_log = "Out-Degree",
  Betweenness_log = "Betweenness",
  In.degree_log = "In-Degree"
)

# Empty list to store importance data frames from each dataset
importance_list <- list()

# Loop over cleaned datasets
for (file in names(cleaned_data_list)) {
  
  print(file)
  
  # Get the cleaned data
  data_clean <- cleaned_data_list[[file]]
  
  # Filter rows with complete data in the variables of interest
  df_clean <- data_clean[complete.cases(data_clean[, c("coefficient_percentile", vars_used)]), ]
  
  # Fit Random Forest regression model
  set.seed(123)  # For reproducibility
  rf_model <- randomForest(
    formula = as.formula(paste("coefficient_percentile ~", paste(vars_used, collapse = " + "))),
    data = df_clean,
    importance = TRUE,
    ntree = 500
  )
  
  # Model summary
  print(rf_model)
  
  # Variable importance table
  importance_df <- as.data.frame(importance(rf_model)) # importance by MSE
  importance_df$Variable <- rownames(importance_df)
  
  # Add pretty labels for plotting
  importance_df$Variable_pretty <- label_map[importance_df$Variable]
  importance_df$Variable_pretty <- ifelse(
    is.na(importance_df$Variable_pretty),
    importance_df$Variable,
    importance_df$Variable_pretty
  )
  
  # Add a column to track which dataset it came from (for faceting)
  importance_df$Dataset <- file
  
  # Store in the list
  importance_list[[file]] <- importance_df[, c("Variable", "Variable_pretty", "%IncMSE", "Dataset")]
  
}

# Combine all data frames into one for faceted plotting
all_importance <- do.call(rbind, importance_list)

# Clean dataset names for nicer facet titles by removing "_ElasticNetCoefficients.csv"
all_importance$Dataset <- gsub("_ElasticNetCoefficients\\.csv$", "", all_importance$Dataset)

# Faceted plot for all datasets together
ggplot(all_importance, aes(x = `%IncMSE`, y = reorder(Variable_pretty, `%IncMSE`))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ Dataset, scales = "free_y") +
  labs(
    title = "Random Forest Variable Importance by Dataset",
    x = "% Increase in MSE",
    y = "Predictor"
  ) +
  theme_minimal()

# Combine all data frames into one
average_importance <- aggregate(`%IncMSE` ~ Variable, data = all_importance, FUN = mean)

average_importance$Variable_pretty <- label_map[average_importance$Variable]
average_importance$Variable_pretty <- ifelse(
  is.na(average_importance$Variable_pretty),
  average_importance$Variable,
  average_importance$Variable_pretty
)

average_importance <- average_importance[order(average_importance$`%IncMSE`, decreasing = TRUE), ]

ggplot(average_importance, aes(x = `%IncMSE`, y = reorder(Variable_pretty, `%IncMSE`))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Variable Importance Across All Datasets",
    x = "Average % Increase in MSE",
    y = "Predictor"
  ) +
  theme_minimal()


#### Linear regression models ####

for (file in names(cleaned_data_list)) {
  print(paste("Fitting model for file:", file))
  
  # Get the cleaned data for this file
  data_clean <- cleaned_data_list[[file]]
  
  
  # Logistic regression model
  lin_mod <- lm(coefficient_percentile ~ birthyear +
                  In.degree_log +
                  Out.degree_log +
                  Betweenness_log +
                  Closeness_binary, data = data_clean)
  
  # Print summary of the model
  print(summary(lin_mod))
}

#### Aggregated linear model ####
library(purrr)

# Clean dataset names by removing the suffix
clean_names <- sub("_ElasticNetCoefficients\\.csv$", "", names(cleaned_data_list))

# Combine all datasets into one with cleaned names
combined_df <- map2_dfr(cleaned_data_list, clean_names, 
                        ~ mutate(.x, dataset = .y))

aggregated_lin_mod <- lm(coefficient_percentile ~ birthyear +
                In.degree_log +
                Out.degree_log +
                Betweenness_log +
                Closeness_binary, data = combined_df)

# Print summary of the model
print(summary(aggregated_lin_mod))

#### Visualization ####

# Plot with facets
ggplot(combined_df, aes(x = birthyear, y = coefficient_percentile)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ dataset, scales = "free") +  # or scales = "fixed" if appropriate
  theme_minimal() +
  labs(title = "",
       x = "Year of Birth",
       y = "Coefficient Percentiles")

ggplot(combined_df, aes(x = In.degree_log, y = coefficient_percentile)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ dataset, scales = "free") +  # or scales = "fixed" if appropriate
  theme_minimal() +
  labs(title = "",
       x = "In-Degree Centrality",
       y = "Coefficient Percentiles")

ggplot(combined_df, aes(x = Out.degree_log, y = coefficient_percentile)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ dataset, scales = "free") +  # or scales = "fixed" if appropriate
  theme_minimal() +
  labs(title = "",
       x = "Out-Degree Centrality",
       y = "Coefficient Percentiles")

ggplot(combined_df, aes(x = Betweenness_log, y = coefficient_percentile)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ dataset, scales = "free") +  # or scales = "fixed" if appropriate
  theme_minimal() +
  labs(title = "",
       x = "Betweenness Centrality",
       y = "Coefficient Percentiles")

ggplot(combined_df, aes(x = Closeness_binary, y = coefficient_percentile)) +
  geom_boxplot() +
  facet_wrap(~ dataset, scales = "free") +
  theme_minimal() +
  labs(title = "",
       x = "Closeness Centrality",
       y = "Coefficient Percentiles") +
  theme(legend.position = "none")  # Remove redundant legend

# Plot with color for dataset
ggplot(combined_df, aes(x = birthyear, y = coefficient_percentile)) +
  geom_point(aes(color = dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "",
    x = "Year of Birth",
    y = "Coefficient Percentiles",
    color = "Dataset"
  )

ggplot(combined_df, aes(x = In.degree_log, y = coefficient_percentile)) +
  geom_point(aes(color = dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "",
    x = "In-Degree Centrality",
    y = "Coefficient Percentiles",
    color = "Dataset"
  )
ggplot(combined_df, aes(x = Out.degree_log, y = coefficient_percentile)) +
  geom_point(aes(color = dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "",
    x = "Out-Degree Centrality",
    y = "Coefficient Percentiles",
    color = "Dataset"
  )
ggplot(combined_df, aes(x = Betweenness_log, y = coefficient_percentile)) +
  geom_point(aes(color = dataset)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(
    title = "",
    x = "Betweenness Centrality",
    y = "Coefficient Percentiles",
    color = "Dataset"
  )

ggplot(combined_df, aes(x = Closeness_binary, y = coefficient_percentile)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "",
       x = "Closeness Centrality",
       y = "Coefficient Percentiles") +
  theme(legend.position = "none")  # Remove redundant legend

#### Histograms centrality measures ####

for (file in names(cleaned_data_list)) {
  print(paste("Fitting model for file:", file))
  
  # Get the cleaned data for this file
  data_clean <- cleaned_data_list[[file]]
  
  
  # Logistic regression model
  print(hist(data_clean$Betweenness_log,
             main = "Histogram of Data",
             xlab = "Values",
             col = "skyblue",
             border = "white"))
}


