# Loading libraries and packages
options(warn=1)
# from https://github.com/AntheSevenants/ElasticToolsR :
#Install.packages(c("glmnet", "doMC"))
source("Dataset.R")
source("ElasticNet.R")

library("readxl")
library("dplyr")
library("tidyr")
library("tools")

set.seed(10)


#### Function for performing elastic net regression ####
do_elastic_net <- function(dataset, var1, var2, var3) {
  # Convert dataset
  ds <- dataset(df=dataset,
                response_variable_column=var1,
                to_binary_columns=c(var2),
                other_columns=c(var3))
  
  # Convert the data to a feature matrix
  feature_matrix <- ds$as_matrix()
  
  # retrieve feature list
  feature_list <- ds$as_feature_list()
  
  # Define net object
  net <- elastic_net(ds=ds,
                     nfolds=20,
                     type.measure="class")
  
  # Automatically determine the best alpha parameter
  collection <- net$do_elastic_net_regression_auto_alpha(k=10)
  print(collection$results)
  
  lowest_loss_row <- collection$results[which.min(collection$results$loss),]
  fit <- collection$fits[[lowest_loss_row[["X_id"]]]]
  print(lowest_loss_row)
  
  # Get coefficients
  coefficients_with_labels <- net$attach_coefficients(fit)
  
  # Rename coefficients
  names(coefficients_with_labels)[names(coefficients_with_labels) == "feature"] <- "Author"
  
  return(coefficients_with_labels)
}


#### Get directory and files ####
datasets_directory="./Datasets"

files <- list.files(datasets_directory, pattern=NULL, all.files=FALSE, 
           full.names=FALSE)

#### Elastic net part ####
# For all files in datasets_directory:
for (file in files) {
  # Read data
  print(file)
  data <- read_excel(paste(datasets_directory, file, sep="/"))
  
  # Make subset with necessary columns
  subset <- data[, c("Change", "author", "year")]
  
  # Remove rows with multiple authors (assumes authors are separated by "; ")
  subset <- subset[!grepl(";", subset$author), ]
  
  # Convert author & Change to factor
  subset$Change <- factor(subset$Change, levels = c("1", "0"))
  subset$author <- as.factor(subset$author)
  
  # Clean columns
  subset <- subset[!is.na(subset$Change), ]
  subset <- subset[!is.na(subset$author), ]
  
  # Perform elastic net regression
  res <- do_elastic_net(subset, "Change", "author", "year")
  
  # Save results
  write.csv(res, paste("./ElasticNetResults", paste(file_path_sans_ext(file), "_ElasticNetCoefficients.csv", sep=""), sep="/"), row.names=FALSE)
}


