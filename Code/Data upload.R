#####################################################
#                    Data upload                    #
#####################################################
###_________________Data loading_________________###
G1 <- read.csv('./Fibre count Summary NEW/G1_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# removing the ".TIF" in G1
G1$Slice<- gsub(".TIF","",G1$Slice)

text_to_remove <- "_negative"
G1 <- G1[!grepl(text_to_remove, G1$Slice), ]
text_to_remove <- "_positive"
G1 <- G1[!grepl(text_to_remove, G1$Slice), ]

# Creating the different dataframe before and after transfer
G1B<- G1 %>% filter(grepl('_B', Slice))
G1Atr <- G1 %>% filter(grepl('_Atr', Slice))

# Create table
G1_Dataset_pending <- data.frame(rbind(G1B$Slice, G1B$Count, G1Atr$Count))
G1_Dataset<-as.data.frame(t(G1_Dataset_pending))

names(G1_Dataset) <- c("Sample", "Before transfer", "After transfer")

# change factor to numeric in the column 2 to 3
G1_Dataset[,2:3] = apply(G1_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# remove unused dataframe
rm(G1,G1B,G1Atr,G1_Dataset_pending)

###_________________Data loading_________________###
G5 <- read.csv('./Fibre count Summary NEW/G5_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# removing the ".TIF" in G5
G5$Slice<- gsub(".TIF","",G5$Slice)

text_to_remove <- "_negative"
G5 <- G5[!grepl(text_to_remove, G5$Slice), ]
text_to_remove <- "_positive"
G5 <- G5[!grepl(text_to_remove, G5$Slice), ]

# Creating the different dataframe before and after transfer
G5B<- G5 %>% filter(grepl('_B', Slice))
G5Atr <- G5 %>% filter(grepl('_Atr', Slice))

# Create table
G5_Dataset_pending <- data.frame(rbind(G5B$Slice, G5B$Count, G5Atr$Count))
G5_Dataset<-as.data.frame(t(G5_Dataset_pending))

names(G5_Dataset) <- c("Sample", "Before transfer", "After transfer")

# change factor to numeric in the column 2 to 3
G5_Dataset[,2:3] = apply(G5_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# remove unused dataframe
rm(G5,G5B,G5Atr,G5_Dataset_pending)

###_________________Data loading_________________###
G12 <- read.csv('./Fibre count Summary NEW/G12_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# removing the ".TIF" in G12
G12$Slice<- gsub(".TIF","",G12$Slice)

text_to_remove <- "_negative"
G12 <- G12[!grepl(text_to_remove, G12$Slice), ]
text_to_remove <- "_positive"
G12 <- G12[!grepl(text_to_remove, G12$Slice), ]

# Creating the different dataframe before and after transfer
G12B<- G12 %>% filter(grepl('_B', Slice))
G12Atr <- G12 %>% filter(grepl('_Atr', Slice))

# # Assuming 'your_data' is your dataframe and 'column_name' is the column to check
# duplicates <- duplicated(G12Atr$Slice)
# # Select rows with duplicates
# duplicate_rows <- G12Atr[duplicates, ]

# Create table
G12_Dataset_pending <- data.frame(rbind(G12B$Slice, G12B$Count, G12Atr$Count))
G12_Dataset<-as.data.frame(t(G12_Dataset_pending))

names(G12_Dataset) <- c("Sample", "Before transfer", "After transfer")

# change factor to numeric in the column 2 to 3
G12_Dataset[,2:3] = apply(G12_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# remove unused dataframe
rm(G12,G12B,G12Atr,G12_Dataset_pending)
