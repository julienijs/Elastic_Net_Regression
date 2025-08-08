library(readxl)
library(ggplot2)
library(dplyr)
library(tools)

# Directories for datasets and ElasticNetResults
data_directory <- "./Datasets"
results_directory <- "./ElasticNetResults"

# Function to list only valid files based on extension
list_files_by_extension <- function(directory, extensions) {
  files <- list.files(directory, full.names = TRUE)
  # Keep only files with specified extensions
  files <- files[grepl(paste0("\\.(", paste(extensions, collapse = "|"), ")$"), files, ignore.case = TRUE)]
  return(files)
}

# List Excel files in Datasets directory
data_files <- list_files_by_extension(data_directory, c("xlsx"))
# List CSV files in ElasticNetResults directory
results_files <- list_files_by_extension(results_directory, c("csv"))

# Initialize lists for classification
big_coeff_files <- c()
small_coeff_files <- c()

# Classify files based on coefficients
for (result_file in results_files) {
  # Read coefficient data from CSV
  result_data <- tryCatch({
    read.csv(result_file)
  }, error = function(e) {
    warning(paste("Error reading file:", result_file, "-", e$message))
    return(NULL)
  })
  
  if (!is.null(result_data) && "coefficient" %in% names(result_data)) {
    # Get the base file name without extension
    base_file <- file_path_sans_ext(basename(result_file))
    
    # Remove the "_ElasticNetCoefficients" suffix if present
    clean_file_name <- sub("_ElasticNetCoefficients$", "", base_file)
    
    # Categorize files based on coefficient values
    if (any(abs(result_data$coefficient) >= 0.01)) {
      big_coeff_files <- c(big_coeff_files, clean_file_name)
    } else {
      small_coeff_files <- c(small_coeff_files, clean_file_name)
    }
  } else {
    warning(paste("File", result_file, "does not contain a 'coefficient' column"))
  }
}

# Function to read and process data files
process_files <- function(files, directory) {
  combined_data <- data.frame()
  for (file in files) {
    # Construct the full file path for the corresponding dataset
    matching_file <- list.files(directory, pattern = paste0("^", file, "\\.xlsx$"), full.names = TRUE)
    if (length(matching_file) == 0) {
      warning(paste("No matching file found for:", file))
      next
    }
    
    # Read the Excel file
    data <- tryCatch({
      read_excel(matching_file[1])  # Use the first match if multiple
    }, error = function(e) {
      warning(paste("Error reading file:", matching_file[1], "-", e$message))
      return(NULL)
    })
    
    if (!is.null(data) && "Decade" %in% names(data) && "Change" %in% names(data)) {
      data <- data %>% select(Decade, Change)
      
      # Convert 'Change' column to factor
      data$Change <- as.factor(data$Change)
      
      # Remove NA rows and convert 'Decade' to numeric
      data <- droplevels(data[!data$Change == "NA", ]) %>% na.omit()
      data$Decade <- as.numeric(data$Decade)
      
      # Add a column to identify the file
      data$Changes <- file_path_sans_ext(basename(matching_file[1]))
      
      # Combine the data
      combined_data <- bind_rows(combined_data, data)
    } else {
      warning(paste("File", file, "does not contain 'Decade' and 'Change' columns"))
    }
  }
  return(combined_data)
}

# Process data files for big and small coefficients
big_coeff_data <- process_files(big_coeff_files, data_directory)
small_coeff_data <- process_files(small_coeff_files, data_directory)

# Create plots for big and small coefficient datasets
big_plot <- ggplot(big_coeff_data, aes(x = as.numeric(Decade), 
                                       y = as.numeric(as.character(Change)), 
                                       color = Changes)) +
  stat_smooth(method = "glm", se = TRUE, 
              method.args = list(family = binomial(link = "logit"))) +
  labs(title = "Big Coefficients (>= 0.01)", x = "Decade", y = "Change") +
  theme_minimal() + 
  theme(legend.position = "right")

small_plot <- ggplot(small_coeff_data, aes(x = as.numeric(Decade), 
                                           y = as.numeric(as.character(Change)), 
                                           color = Changes)) +
  stat_smooth(method = "glm", se = TRUE, 
              method.args = list(family = binomial(link = "logit"))) +
  labs(title = "Small Coefficients (< 0.01)", x = "Decade", y = "Change") +
  theme_minimal() + 
  theme(legend.position = "right")

# Print the plots
print(big_plot)
print(small_plot)
