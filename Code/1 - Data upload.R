##########################################################################
#####                           Data upload                          #####
##########################################################################
# This R script is the first step to export all the data needed to generate the figure in the article.
# This includes the data from: 
# 1. Weight of all donor garments
# 2. Wastewater Volume and Fibre release
# 3. Secondary transfer experiments 
# 4. Transfer experiments (with and without washing activities)

# ------------------------------------------------------------------------
# Section 1: Weight of all donor garments
# ------------------------------------------------------------------------

# Read the CSV file containing garment weight data
WeightGarment <- read.csv('./Garment weight.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Create different dataframes for each garment
G1weight<- WeightGarment %>% filter(grepl('^[1]$', Load))
G5weight<- WeightGarment %>% filter(grepl(5, Load))
G12weight<- WeightGarment %>% filter(grepl(12, Load))

# Calculate total weight
G1weightTotal<- sum(G1weight$weight_kg);G1weightTotal
G5weightTotal <- sum(G5weight$weight_kg);G5weightTotal
G12weightTotal <- sum(G12weight$weight_kg);G12weightTotal

# Remove unused dataframes
rm(G1weight,G5weight,G12weight,WeightGarment)

# ------------------------------------------------------------------------
# Section 2: Wastewater Volume and Fibre release
# ------------------------------------------------------------------------

# Read data on wastewater fibre weight and volume for G1 garments
Wastewaterfibres_G1 <- read.csv('./Wastewater/Data_filtration_fibre weight_G1.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres_G1$Date<-as.factor(Wastewaterfibres_G1$Date)
Wastewatervolume_G1 <- read.csv('./Wastewater//Data_filtration_water volume_G1.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Read data on wastewater fibre weight and volume for G5 garments
Wastewaterfibres_G5 <- read.csv('./Wastewater//Data_filtration_fibre weight_G5.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres_G5$Date<-as.factor(Wastewaterfibres_G5$Date)
Wastewatervolume_G5 <- read.csv('./Wastewater//Data_filtration_water volume_G5.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Read data on wastewater fibre weight and volume for G12 garments
Wastewaterfibres_G12 <- read.csv('./Wastewater//Data_filtration_fibre weight_G12.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres_G12$Date<-as.factor(Wastewaterfibres_G12$Date)
Wastewatervolume_G12 <- read.csv('./Wastewater//Data_filtration_water volume_G12.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# ------------------------------------------------------------------------
# Section 3: Transfer experiments
# ------------------------------------------------------------------------

#### Uploading data from the first series involving washing a single donor garment ####
G1 <- read.csv('./Fibre count Summary/G1_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in G1
G1$Slice<- gsub(".TIF","",G1$Slice)

# Remove "_negative" and "_positive" in G1
text_to_remove <- "_negative"
G1 <- G1[!grepl(text_to_remove, G1$Slice), ]
text_to_remove <- "_positive"
G1 <- G1[!grepl(text_to_remove, G1$Slice), ]

# Create different dataframes before and after transfer
G1B<- G1 %>% filter(grepl('_B', Slice))
G1Atr <- G1 %>% filter(grepl('_Atr', Slice))

# Create table
G1_Dataset_pending <- data.frame(rbind(G1B$Slice, G1B$Count, G1Atr$Count))
G1_Dataset<-as.data.frame(t(G1_Dataset_pending))

names(G1_Dataset) <- c("Sample", "Before transfer", "After transfer")

# Convert factor to numeric in columns 2 to 3
G1_Dataset[,2:3] = apply(G1_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(G1,G1B,G1Atr,G1_Dataset_pending)

#### Uploading data from the second series involving washing 5 donor garments ####
G5 <- read.csv('./Fibre count Summary/G5_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in G5
G5$Slice<- gsub(".TIF","",G5$Slice)

# Remove "_negative" and "_positive" in G5
text_to_remove <- "_negative"
G5 <- G5[!grepl(text_to_remove, G5$Slice), ]
text_to_remove <- "_positive"
G5 <- G5[!grepl(text_to_remove, G5$Slice), ]

# Create different dataframes before and after transfer
G5B<- G5 %>% filter(grepl('_B', Slice))
G5Atr <- G5 %>% filter(grepl('_Atr', Slice))

# Create table
G5_Dataset_pending <- data.frame(rbind(G5B$Slice, G5B$Count, G5Atr$Count))
G5_Dataset<-as.data.frame(t(G5_Dataset_pending))

names(G5_Dataset) <- c("Sample", "Before transfer", "After transfer")

# Convert factor to numeric in columns 2 to 3
G5_Dataset[,2:3] = apply(G5_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(G5,G5B,G5Atr,G5_Dataset_pending)

#### Uploading data from the third series involving washing 12 donor garments ####
G12 <- read.csv('./Fibre count Summary/G12_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in G12
G12$Slice<- gsub(".TIF","",G12$Slice)

# Remove "_negative" and "_positive" in G12
text_to_remove <- "_negative"
G12 <- G12[!grepl(text_to_remove, G12$Slice), ]
text_to_remove <- "_positive"
G12 <- G12[!grepl(text_to_remove, G12$Slice), ]

# Create different dataframes before and after transfer
G12B<- G12 %>% filter(grepl('_B', Slice))
G12Atr <- G12 %>% filter(grepl('_Atr', Slice))

# Create table
G12_Dataset_pending <- data.frame(rbind(G12B$Slice, G12B$Count, G12Atr$Count))
G12_Dataset<-as.data.frame(t(G12_Dataset_pending))

names(G12_Dataset) <- c("Sample", "Before transfer", "After transfer")

# Convert factor to numeric in columns 2 to 3
G12_Dataset[,2:3] = apply(G12_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(G12,G12B,G12Atr,G12_Dataset_pending)

#### Uploading data from the repetitive transfer performed with the control garment (never washed) ####
RT <- read.csv('./Fibre count Summary/RT_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in RT
RT$Slice<- gsub(".TIF","",RT$Slice)

# Remove "_negative" and "_positive" in RT
text_to_remove <- "_negative"
RT <- RT[!grepl(text_to_remove, RT$Slice), ]
text_to_remove <- "_positive"
RT <- RT[!grepl(text_to_remove, RT$Slice), ]

# Create different dataframes before and after transfer
RTB<- RT %>% filter(grepl('_B', Slice))
RTAtr <- RT %>% filter(grepl('_Atr', Slice))

# Create table
RT_Dataset_pending <- data.frame(rbind(RTB$Slice, RTB$Count, RTAtr$Count))
RT_Dataset<-as.data.frame(t(RT_Dataset_pending))

names(RT_Dataset) <- c("Sample", "Before transfer", "After transfer")

# Convert factor to numeric in columns 2 to 3
RT_Dataset[,2:3] = apply(RT_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(RT,RTB,RTAtr,RT_Dataset_pending)

#### Uploading data from the repetitive transfer performed with the donor garment washed only once ####
RTwash <- read.csv('./Fibre count Summary/RTwash_Summary.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

# Remove ".TIF" in RTwash
RTwash$Slice<- gsub(".TIF","",RTwash$Slice)

# Remove "_negative" and "_positive" in RTwash
text_to_remove <- "_negative"
RTwash <- RTwash[!grepl(text_to_remove, RTwash$Slice), ]
text_to_remove <- "_positive"
RTwash <- RTwash[!grepl(text_to_remove, RTwash$Slice), ]

# Create different dataframes before and after transfer
RTwashB<- RTwash %>% filter(grepl('_B', Slice))
RTwashAtr <- RTwash %>% filter(grepl('_Atr', Slice))

# Create table
RTwash_Dataset_pending <- data.frame(rbind(RTwashB$Slice, RTwashB$Count, RTwashAtr$Count))
RTwash_Dataset<-as.data.frame(t(RTwash_Dataset_pending))

names(RTwash_Dataset) <- c("Sample", "Before transfer", "After transfer")

# ConveRTwash factor to numeric in columns 2 to 3
RTwash_Dataset[,2:3] = apply(RTwash_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# Remove unused dataframes
rm(RTwash,RTwashB,RTwashAtr,RTwash_Dataset_pending)

# ------------------------------------------------------------------------
# Section 4: Secondary transfer experiments
# ------------------------------------------------------------------------
# Upload data from the secondary transfer experiments
ST <- read.csv('./Fibre count Summary/ST_Summary.csv', sep="," ,header = T,fileEncoding="UTF-8-BOM")

# Remove ".TIF" in ST
ST$Slice<- gsub(".TIF","",ST$Slice)

# Create different dataframes before and after transfer
STB<- ST %>% filter(grepl('_B', Slice))
STAW <- ST %>% filter(grepl('_AW', Slice))

# Create table
ST_Dataset_pending <- data.frame(rbind(STB$Slice, STB$Count, STAW$Count))
ST_Dataset<-as.data.frame(t(ST_Dataset_pending))

names(ST_Dataset) <- c("Sample", "Before wash", "After wash")

# change factor to numeric in the column 2 to 3
ST_Dataset[,2:3] = apply(ST_Dataset[,2:3], 2, function(x) as.numeric(as.character(x)));

# remove unused dataframe
rm(ST,STB,STAW,ST_Dataset_pending)