#####################################################
#                    Data upload                    #
#####################################################
#### to upload the data obtained with 1 garment and 12 garments ####
# Define a function to process a dataset
process_dataset <- function(file_prefix) {
  # Data loading
  data <- read.csv(paste0('./Fibre count Summary/', file_prefix, '_Summary.csv'), sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
  
  # Creating the different dataframes before and after transfer
  data_B <- data %>% filter(grepl('_B.', Slice))
  data_Atr <- data %>% filter(grepl('_Atr.TIF', Slice))
  
  # Removing the ".TIF" in data_B and data_Atr
  data_B$Slice <- gsub(".TIF", "", data_B$Slice)
  data_Atr$Slice <- gsub(".TIF", "", data_Atr$Slice)
  
  # Create table
  dataset_pending <- data.frame(rbind(data_B$Slice, data_B$Count, data_Atr$Count))
  dataset <- as.data.frame(t(dataset_pending))
  names(dataset) <- c("Sample", "Before transfer", "After transfer")
  
  # Change factor to numeric in columns 2 and 3
  dataset[, 2:3] <- apply(dataset[, 2:3], 2, function(x) as.numeric(as.character(x)))
  
  # Return the processed dataset
  return(dataset)
}

# List of prefixes for the datasets
prefixes <- c(paste0("W", sprintf("%03d", c(0, 1, 2, 3, 4, 5, 6, 7, 9, 11, 13, 15)), "_G1"),
              paste0("W", sprintf("%03d", c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)), "_G12A"),
              paste0("W", sprintf("%03d", c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)), "_G12B"),
              paste0("W", sprintf("%03d", c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)), "_G12C"))

# Process and store the datasets in separate data frames
for (prefix in prefixes) {
  dataset_name <- paste0(prefix, "_Dataset")
  assign(dataset_name, process_dataset(prefix))
}

#### to upload the data obtained with 5 garments ####
# Define a function to process a dataset
# Define a function to process a dataset
process_dataset <- function(file_prefix) {
  # Data loading
  data_B <- read.csv(paste0('./Fibre count Summary/', file_prefix, '_B_Summary.csv'), sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
  data_Atr <- read.csv(paste0('./Fibre count Summary/', file_prefix, '_Atr_Summary.csv'), sep = ",", header = TRUE, fileEncoding = 'UTF-8-BOM')
  
  # Removing the "_B.TIF" in data_B
  data_B$Slice <- gsub("_B.TIF", "", data_B$Slice)
  
  # Create table
  dataset_pending <- data.frame(rbind(data_B$Slice, data_B$Count, data_Atr$Count))
  dataset <- as.data.frame(t(dataset_pending))
  names(dataset) <- c("Sample", "Before transfer", "After transfer")
  
  # Change factor to numeric in columns 2 and 3
  dataset[, 2:3] <- apply(dataset[, 2:3], 2, function(x) as.numeric(as.character(x)))
  
  # Return the processed dataset
  return(dataset)
}

# Create individual dataframes for W000 to W051
for (i in 0:51) {
  file_prefix <- sprintf("W%03d", i)
  assign(paste0(file_prefix, "_G5_Dataset"), process_dataset(file_prefix))
}

