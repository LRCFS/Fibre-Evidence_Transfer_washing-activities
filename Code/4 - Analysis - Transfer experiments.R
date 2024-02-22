#############################################################
#####                     To read                       #####
#############################################################
# This R script is to generate the figures related to the Transfer experiments:
# 1: Washing series vs control garment
# Repetitive transfer (control garment)
# Repetitive Transfer - washing activities
# Repetitive Transfer vs. Wash 
# the fibres released in the wastewater VS number of transferred fibres (this part of the code needs to be run after Analysis - Transfer.R)

# ------------------------------------------------------------------------
# Section 1: Washing series vs control garment
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Subsection 1-1 : Repetitive Transfer - control garment
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# subtract background
RT_Dataset$value <-  RT_Dataset$`After transfer` -  RT_Dataset$`Before transfer`

# Filter the original dataset RT_Dataset to create subsets based on specific patterns in the Sample column
RTB1_Dataset<- RT_Dataset %>% filter(grepl('G1_1', Sample))
RTB3_Dataset<- RT_Dataset %>% filter(grepl('G1_3', Sample))
RTB6_Dataset<- RT_Dataset %>% filter(grepl('G1_6', Sample))
RTB8_Dataset<- RT_Dataset %>% filter(grepl('G1_8', Sample))

#Assign a Coder to each wash
RTB1_Dataset$Coder <- "RTB1"
RTB3_Dataset$Coder <- "RTB3"
RTB6_Dataset$Coder <- "RTB6"
RTB8_Dataset$Coder <- "RTB8"

# Create a data frame with a sequence of numbers from 1 to 100
numS <- data.frame(seq(1,100, by = 1))
names(numS) <- c("Time")

# Create a data frame with a sequence of numbers from 1 to 100
RTB1_Dataset <- cbind(RTB1_Dataset,numS)
RTB3_Dataset <- cbind(RTB3_Dataset,numS)
RTB6_Dataset <- cbind(RTB6_Dataset,numS)
RTB8_Dataset <- cbind(RTB8_Dataset,numS)

# Selects specific columns (Coder, 'After transfer', Time) from different datasets 
TransferRTB1 <- RTB1_Dataset %>%
  dplyr::select(Coder,`After transfer`,Time)
TransferRTB6 <- RTB6_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)
TransferRTB3 <- RTB3_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)
TransferRTB8 <- RTB8_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)
names(TransferRTB1) <- c("group", "value", "Transfer")
names(TransferRTB6) <- c("group", "value", "Transfer")
names(TransferRTB3) <- c("group", "value", "Transfer")
names(TransferRTB8) <- c("group", "value", "Transfer")

# Combined all results together
TotRT <-rbind(TransferRTB1,TransferRTB3,TransferRTB6,TransferRTB8)

# Combined results by orientation (parallel or perpendicular to the rib of the knit)
TotRTpara <-rbind(TransferRTB1,TransferRTB3)
TotRTperp <-rbind(TransferRTB6,TransferRTB8)

# descriptive statistics
df_means <- ddply(TotRT, "group", summarise, mean_value = mean(value)) ; df_means
df_SD <- ddply(TotRT, "group", summarise, mean_value = sd(value)) ; df_SD
df_meanspara <- ddply(TotRTpara, "group", summarise, mean_value = mean(value)) ; df_meanspara
df_meansperp <- ddply(TotRTperp, "group", summarise, mean_value = mean(value)) ; df_meansperp

#### Intermediate Data Visualisation ####
lm(value~Transfer, data=TransferRTB1)
pRTB1 <-ggplot(data = TransferRTB1, aes(Transfer, value)) +
  geom_point(size=1, colour= "black")+
  labs(x="Repetitive transfer", y="Number of Fibre")+ ylim(0,15)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Parallel contact area 1")+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="solid", size=1)+
  annotate(geom="text", x=82, y=15, label="y = - 0.02x + 3.04",color="black",fontface = "bold")+
  annotate(geom="text", x=82, y=14, label="bar(x) == 1.98",color="black",parse=T)

lm(value~Transfer, data=TransferRTB3)
pRTB3 <-ggplot(data = TransferRTB3, aes(Transfer, value)) +
  geom_point(size=1, colour= "black")+
  labs(x="Repetitive transfer", y="Number of Fibre")+ ylim(0,15)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Parallel contact area 2")+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="solid", size=1)+
  annotate(geom="text", x=83, y=15, label="y = - 0.01x + 3.7",color="black",fontface = "bold")+
  annotate(geom="text", x=82, y=14, label="bar(x) == 3.02",color="black",parse=T)

lm(value~Transfer, data=TransferRTB6)
pRTB6 <-ggplot(data = TransferRTB6, aes(Transfer, value)) +
  geom_point(size=1, colour= "black")+
  labs(x="Repetitive transfer", y="Number of Fibre")+ ylim(0,15)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Perpendicular contact area 1")+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="solid", size=1)+
  annotate(geom="text", x=82, y=15, label="y = - 0.01x + 4.93",color="black",fontface = "bold")+
  annotate(geom="text", x=82, y=14, label="bar(x) == 4.50",color="black",parse=T)

lm(value~Transfer, data=TransferRTB8)
pRTB8 <-ggplot(data = TransferRTB8, aes(Transfer, value)) +
  geom_point(size=1, colour= "black")+
  labs(x="Repetitive transfer", y="Number of Fibre")+ ylim(0,15)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Perpendicular contact area 2")+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="solid", size=1)+
  annotate(geom="text", x=82, y=15, label="y = - 0.04x + 5.16",color="black",fontface = "bold")+
  annotate(geom="text", x=82, y=14, label="bar(x) == 3.27",color="black",parse=T)

#### Final graph - Figure S4 (supplementary information) #### 
pCombinedRT_pending <- ggarrange(pRTB1+ rremove("ylab") + rremove("xlab"),
                                 pRTB3+ rremove("ylab") + rremove("xlab"),
                                 pRTB6+ rremove("ylab") + rremove("xlab"),
                                 pRTB8+ rremove("ylab") + rremove("xlab"),
                                 labels = NULL,
                                 common.legend = TRUE, legend = "bottom",
                                 align = "hv",
                                 ncol = 2, nrow = 2,
                                 font.label = list(size = 8, color = "black", family = NULL, position = "top"))

pCombinedRT <- annotate_figure(pCombinedRT_pending, left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedRT

# to save the graph
ggsave("RT_control garment.png", pCombinedRT, width =8, height = 7, units = "in", dpi=300,path = "Results")

# ------------------------------------------------------------------------
# Subsection 1-2 : Transfer - Washing series vs control garment
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# Split the column based on "_"
Transfer_G1 <- separate(G1_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")
Transfer_G5 <- separate(G5_Dataset, Sample, into = c("wash","garment", "band", "orientation", "condition"), sep = "_")
Transfer_G12 <- separate(G12_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")

# substract background
Transfer_G1$value <-  Transfer_G1$`After transfer` -  Transfer_G1$`Before transfer`
Transfer_G5$value <-  Transfer_G5$`After transfer` -  Transfer_G5$`Before transfer`
Transfer_G12$value <-  Transfer_G12$`After transfer` -  Transfer_G12$`Before transfer`

# Rename garment name in Transfer_G12 for more comprehension
Transfer_G12$garment<- gsub("G4A","G1",Transfer_G12$garment)
Transfer_G12$garment<- gsub("G4B","G2",Transfer_G12$garment)
Transfer_G12$garment<- gsub("G4C","G3",Transfer_G12$garment)

# to export 
Transfer_G1 <- Transfer_G1 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value,band)
Transfer_G5 <- Transfer_G5 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value,band)
Transfer_G12 <- Transfer_G12 %>% dplyr::select(wash, garment,`Before transfer`,`After transfer`,value,band)

# Select important column
Transfer_G1 <-  Transfer_G1 %>% dplyr::select(wash, garment,value,band)
Transfer_G5 <-  Transfer_G5 %>% dplyr::select(wash, garment,value,band)
Transfer_G12 <-  Transfer_G12 %>% dplyr::select(wash, garment,value,band)

# Create a list of dataframes
dataframes <- list(Transfer_G1, Transfer_G5, Transfer_G12)

# Create a function to compute statistics and write to CSV
compute_and_write_stats <- function(df, filename) {
  mean_df <- aggregate(value ~ wash, df, function(x) round(mean(x), digits = 2))
  sd_df <- aggregate(value ~ wash, df, function(x) round(sd(x), digits = 2))
  median_df <- aggregate(value ~ wash, df, function(x) round(median(x), digits = 2))
  
  datatable <- data.frame(
    "Wash number" = mean_df$wash,
    "Average" = mean_df$value,
    "Median" = median_df$value,
    "SD" = sd_df$value
  )
  
  write.table(datatable, file = filename, sep = ",", fileEncoding = "UTF-8")
}

# Loop through the dataframes and compute/write statistics
for (i in 1:length(dataframes)) {
  compute_and_write_stats(dataframes[[i]], paste("Results/Stats-Garment", c(1, 5, 12)[i], ".csv", sep = ""))
}

# Create dataframe for the final plot
# data from the first series involving washing a single donor garment
n=15
numS <- data.frame(setdiff(0:n, c(8,10,12,14)))
Transfer_G1 <- Transfer_G1[!is.na(Transfer_G1$value), ]
Transfer_G1$value <- as.numeric(Transfer_G1$value)
meanAtrG1 <- aggregate(value ~ wash, Transfer_G1, function(x) round(mean(x), digits = 2))
forplotTotG1 <- data.frame(cbind(numS, value =meanAtrG1$value))
names(forplotTotG1) <- c("Transfer", "value")
forplotTotG1$group <- c("1 garment")

# data from the second series involving washing 5 donor garments
n=51
numS <- data.frame(setdiff(0:n, c()))
Transfer_G5 <- Transfer_G5[!is.na(Transfer_G5$value), ]
Transfer_G5$value <- as.numeric(Transfer_G5$value)
meanAtrG5 <- aggregate(value ~ wash, Transfer_G5, function(x) round(mean(x), digits = 2))
forplotTotG5 <- data.frame(cbind(numS, value =meanAtrG5$value))
names(forplotTotG5) <- c("Transfer", "value")
forplotTotG5$group <- c("5 garments")

# data from the third series involving washing 12 donor garments
n=41
numS <- data.frame(setdiff(0:n, c(16,17,24,26,28,30,32,34,36,38,40)))
Transfer_G12 <- Transfer_G12[!is.na(Transfer_G12$value), ]
Transfer_G12$value <- as.numeric(Transfer_G12$value)
meanAtrG12 <- aggregate(value ~ wash, Transfer_G12, function(x) round(mean(x), digits = 2))
forplotTotG12 <- data.frame(cbind(numS, value =meanAtrG12$value))
names(forplotTotG12) <- c("Transfer", "value")
forplotTotG12$group <- c("12 garments")

# data from the repetitive transfer performed on the control garment
n=51
numS <- data.frame(setdiff(0:n, c()))
meanTotRT<- aggregate(value ~  Transfer, TotRT, function(x) {round(mean(x), digits=2)})
meanTotRT <- meanTotRT[1:52,]
forplotTotRT <- data.frame(cbind(numS, value =meanTotRT$value))
names(forplotTotRT) <- c("Transfer", "value")
forplotTotRT$group <- "Repetitive transfer"

# Combined all data 
Toplot <- rbind(forplotTotRT,forplotTotG1,forplotTotG5,forplotTotG12)

# # Convert specific columns to different format
Toplot$Transfer <- as.numeric(Toplot$Transfer)
Toplot$value <- as.numeric(Toplot$value)
Toplot$group <- as.factor(Toplot$group)

# find the best visual fit
# for the first series involving washing a single donor garment
fit10 <- lm(value~poly(Transfer,10,raw=TRUE), data=forplotTotG1)
j <- summary(fit10)$adj.r.squared;j

# for the second series involving washing 5 donor garments
fit9 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG5)
i <- summary(fit9)$adj.r.squared;i

# for the third series involving washing 12 donor garments
fit7 <- lm(value~poly(Transfer,7,raw=TRUE), data=forplotTotG12)

# for the repetitive transfer performed on the control garment
fit1 <- lm(value~Transfer, data=forplotTotRT);fit1

#### Final graph - Figure 9 ####
# create a scatterplot
colors <- c("#469990",
            "darkred",
            "#000000",
            "grey")

plot(Toplot$Transfer, Toplot$value, col=colors[Toplot$group],
     pch=19,
     xlab='\nWash/Repetitive transfer number',
     ylab='Average number of transferred fibres',
     xaxt='n',
     cex.axis = 1.2,
     cex.lab = 1.2)
axis(1, xaxp=c(0, 51, 51), las=1,cex.axis = 1.2) +theme_bw()
# add legend
legend(32, 24, legend=c("Control garment","1 garment", "5 garments", "12 garments"),
       col=c("grey","#469990", "#000000","darkred"), lty=1:1, cex=1.2)

#fit polynomial regression models up to degree 5
fitRT <- lm(value~Transfer, data=forplotTotRT)
fitG1<- lm(value ~ poly(Transfer, 5, raw = TRUE), data = forplotTotG1)
fitG5 <- lm(value ~ poly(Transfer, 9, raw = TRUE), data = forplotTotG5)
fitG12 <- lm(value ~ poly(Transfer, 7, raw = TRUE), data = forplotTotG12)

#define x-axis values
x_axis <- seq(1, 51, length=51)
lines(x_axis, predict(fitRT, data.frame(Transfer=x_axis)), col='grey')

x_range_G1 <- seq(1, 15, length = 51)  # Adjust the range as needed
predictions_G1 <- predict(fitG1, data.frame(Transfer = x_range_G1))
lines(x_range_G1, predictions_G1, col = '#469990')

lines(x_axis, predict(fitG5, data.frame(Transfer=x_axis)), col='#000000')

x_range <- seq(1, 41, length = 51)  # Adjust the range as needed
predictions <- predict(fitG12, data.frame(Transfer = x_range))
lines(x_range, predictions, col = 'darkred')

# ------------------------------------------------------------------------
# Subsection 1-3 : Repetitive Transfer - Washing series vs control garment
# ------------------------------------------------------------------------
#### Data Cleaning and Processing #####
# substract background
RTWash_Dataset$value <-  RTWash_Dataset$`After transfer` -  RTWash_Dataset$`Before transfer`

# Select each contact areas and place it in different dataframes
RTWashB1_Dataset <- RTWash_Dataset %>% filter(grepl("\\_B1_B\\b", Sample))
RTWashB2_Dataset <- RTWash_Dataset %>% filter(grepl("\\_B2_B\\b", Sample))
RTWashB3_Dataset <- RTWash_Dataset %>% filter(grepl("\\_B3_B\\b", Sample))
RTWashB4_Dataset <- RTWash_Dataset %>% filter(grepl("\\_B4_B\\b", Sample))

# Select the 15 repetitive transfers performed after washing 1 garment one time
RToneWashB1_Dataset <- subset(RTWashB1_Dataset, !grepl("_RTWash_W015|_RTWash_W000", Sample))
RToneWashB2_Dataset <- subset(RTWashB2_Dataset, !grepl("_RTWash_W015|_RTWash_W000", Sample))
RToneWashB3_Dataset <- subset(RTWashB3_Dataset, !grepl("_RTWash_W015|_RTWash_W000", Sample))
RToneWashB4_Dataset <- subset(RTWashB4_Dataset, !grepl("_RTWash_W015|_RTWash_W000", Sample))

# Select the 15 repetitive transfers performed after washing 1 garment 15 time
RT15WashB1_Dataset <- subset(RTWashB1_Dataset, !grepl("_RTWash_W001|_RTWash_W000", Sample))
RT15WashB2_Dataset <- subset(RTWashB2_Dataset, !grepl("_RTWash_W001|_RTWash_W000", Sample))
RT15WashB3_Dataset <- subset(RTWashB3_Dataset, !grepl("_RTWash_W001|_RTWash_W000", Sample))
RT15WashB4_Dataset <- subset(RTWashB4_Dataset, !grepl("_RTWash_W001|_RTWash_W000", Sample))

# Add a coder
RToneWashB1_Dataset$Coder <- "After one wash"
RToneWashB2_Dataset$Coder <- "After one wash"
RToneWashB3_Dataset$Coder <- "After one wash"
RToneWashB4_Dataset$Coder <- "After one wash"

RT15WashB1_Dataset$Coder <- "After 15 washes"
RT15WashB2_Dataset$Coder <- "After 15 washes"
RT15WashB3_Dataset$Coder <- "After 15 washes"
RT15WashB4_Dataset$Coder <- "After 15 washes"

# Create a data frame with a sequence of numbers from 1 to 15
numS <- data.frame(seq(1,15, by = 1))
names(numS) <- c("Time")

# add the column "Time" from "numS" to all dataframe
RToneWashB1_Dataset <- cbind(RToneWashB1_Dataset,numS)
RToneWashB2_Dataset <- cbind(RToneWashB2_Dataset,numS)
RToneWashB3_Dataset <- cbind(RToneWashB3_Dataset,numS)
RToneWashB4_Dataset <- cbind(RToneWashB4_Dataset,numS)

RT15WashB1_Dataset <- cbind(RT15WashB1_Dataset,numS)
RT15WashB2_Dataset <- cbind(RT15WashB2_Dataset,numS)
RT15WashB3_Dataset <- cbind(RT15WashB3_Dataset,numS)
RT15WashB4_Dataset <- cbind(RT15WashB4_Dataset,numS)

# Select the useful variables
TransferRToneWashB1 <- RToneWashB1_Dataset %>%
  dplyr::select(Coder,`After transfer`,Time)
TransferRToneWashB3 <- RToneWashB3_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)
TransferRToneWashB2 <- RToneWashB2_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)
TransferRToneWashB4 <- RToneWashB4_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)

TransferRT15WashB1 <- RT15WashB1_Dataset %>%
  dplyr::select(Coder,`After transfer`,Time)
TransferRT15WashB3 <- RT15WashB3_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)
TransferRT15WashB2 <- RT15WashB2_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)
TransferRT15WashB4 <- RT15WashB4_Dataset %>%
  dplyr::select(Coder,`After transfer`, Time)

# Rename the variables
names(TransferRToneWashB1) <- c("group", "value", "Transfer")
names(TransferRToneWashB3) <- c("group", "value", "Transfer")
names(TransferRToneWashB2) <- c("group", "value", "Transfer")
names(TransferRToneWashB4) <- c("group", "value", "Transfer")

names(TransferRT15WashB1) <- c("group", "value", "Transfer")
names(TransferRT15WashB3) <- c("group", "value", "Transfer")
names(TransferRT15WashB2) <- c("group", "value", "Transfer")
names(TransferRT15WashB4) <- c("group", "value", "Transfer")

# Create dataframe with data from all bands per garment
TotRToneWash <-rbind(TransferRToneWashB1,TransferRToneWashB2,TransferRToneWashB3,TransferRToneWashB4)
TotRT15Wash <-rbind(TransferRT15WashB1,TransferRT15WashB2,TransferRT15WashB3,TransferRT15WashB4)

# Create dataframe with data from all bands, split by orientation
TotRToneWashpara <-rbind(TransferRToneWashB1,TransferRToneWashB2)
TotRToneWashperp <-rbind(TransferRToneWashB3,TransferRToneWashB4)
TotRT15Washpara <-rbind(TransferRT15WashB1,TransferRT15WashB2)
TotRT15Washperp <-rbind(TransferRT15WashB3,TransferRT15WashB4)

# descriptive statistics
df_means <- ddply(TotRToneWash, "group", summarise, mean_value = mean(value)) ; df_means
df_SD <- ddply(TotRToneWash, "group", summarise, mean_value = sd(value)) ; df_SD
df_meanspara <- ddply(TotRToneWashpara, "group", summarise, mean_value = mean(value)) ; df_meanspara
df_meansperp <- ddply(TotRToneWashperp, "group", summarise, mean_value = mean(value)) ; df_meansperp
df_means <- ddply(TotRT15Wash, "group", summarise, mean_value = mean(value)) ; df_means
df_SD <- ddply(TotRT15Wash, "group", summarise, mean_value = sd(value)) ; df_SD
df_meanspara <- ddply(TotRT15Washpara, "group", summarise, mean_value = mean(value)) ; df_meanspara
df_meansperp <- ddply(TotRT15Washperp, "group", summarise, mean_value = mean(value)) ; df_meansperp

# Parallel contact areas
  # plot data with both B1 from the three dataset
TransferRTB1_15 <- head(TransferRTB1, 15)
TransferRTB1_15$group<- gsub("RTB1","Unwashed",TransferRTB1_15$group)
TransferRTWashB1_combined <-rbind(TransferRTB1_15,TransferRToneWashB1,TransferRT15WashB1)
meanTransferRTWashB1_combined<- aggregate(value ~  group, TransferRTWashB1_combined, function(x) {round(mean(x), digits=2)})
  # plot data with both B2 from the three dataset
TransferRTB2_15 <- head(TransferRTB3, 15)
TransferRTB2_15$group<- gsub("RTB3","Unwashed",TransferRTB2_15$group)
TransferRTWashB2_combined <-rbind(TransferRTB2_15,TransferRToneWashB2,TransferRT15WashB2)
meanTransferRTWashB2_combined<- aggregate(value ~  group, TransferRTWashB2_combined, function(x) {round(mean(x), digits=2)})

# Perpendicular contact areas
  # plot data with both B3 from the three dataset
TransferRTB3_15 <- head(TransferRTB6, 15)
TransferRTB3_15$group<- gsub("RTB6","Unwashed",TransferRTB3_15$group)
TransferRTWashB3_combined <-rbind(TransferRTB3_15,TransferRToneWashB3,TransferRT15WashB3)
meanTransferRTWashB3_combined<- aggregate(value ~  group, TransferRTWashB3_combined, function(x) {round(mean(x), digits=2)})
  # plot data with both B4 from the three dataset
TransferRTB4_15 <- head(TransferRTB8, 15)
TransferRTB4_15$group<- gsub("RTB8","Unwashed",TransferRTB4_15$group)
TransferRTWashB4_combined <-rbind(TransferRTB4_15,TransferRToneWashB4,TransferRT15WashB4)
meanTransferRTWashB4_combined<- aggregate(value ~  group, TransferRTWashB4_combined, function(x) {round(mean(x), digits=2)})

#### Intermediate Data Visualisation ####
# Set different shapes for points based on the coder variable
coder_shapes <- c(15,16,2)  # You can add more shape codes if needed
# Set different colors for the linear regression lines based on the coder variable
coder_colors <- c("black", "black","black")  # You can add more colors if needed
# Set different line types for the linear regression lines based on the coder variable
coder_linetypes <- c("solid", "dashed", "dotted")  # You can add more linetypes if needed

# Parallel contact areas combined
TransferRTBpara_combined <- rbind(TransferRTWashB1_combined, TransferRTWashB2_combined)
meanTransferRTBpara_combined <- round(mean(TransferRTBpara_combined$value), digits = 3);meanTransferRTBpara_combined
pRTWashBpara_combined <- ggplot(data = TransferRTBpara_combined, aes(x = Transfer, y = value, color = group, shape = group, linetype = group)) +
  geom_point(size = 1.5) +
  ggtitle("Parallel contact areas")+
  labs(x = "Repetitive experiments", y = "Number of transferred fibres") +
  guides(color = guide_legend(title = NULL, override.aes = list(shape = coder_shapes, linetype = coder_linetypes))) +
  scale_y_continuous(breaks = seq(-2, 28, by = 5), limits = c(-1, 28), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 16, by = 2), limits = c(1, 16), expand = c(0.01, 0)) +
  scale_color_manual(values = coder_colors, guide = FALSE) +
  scale_shape_manual(values = coder_shapes, guide = FALSE) +  
  scale_linetype_manual(values = coder_linetypes, guide = FALSE) +  # Set different line types for the regression lines
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "grey95", size = 1, linetype = "solid", colour = "grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 12)) +
  geom_smooth(aes(linetype = as.factor(group)), method = lm, se = FALSE, size = 0.7)+
  annotate(geom="text", x=15, y=26.5, label="bar(x) == 4.12",color="black",parse=T)

# Perpendicular contact areas combined
TransferRTBperp_combined <- rbind(TransferRTWashB3_combined, TransferRTWashB4_combined)
meanVTransferRTBperp_combined <- round(mean(TransferRTBperp_combined$value), digits = 3);meanVTransferRTBperp_combined
pRTWashBperp_combined <- ggplot(data = TransferRTBperp_combined, aes(x = Transfer, y = value, color = group, shape = group, linetype = group)) +
  geom_point(size = 1.5) +
  ggtitle("Parallel contact areas")+
  labs(x = "Repetitive experiments", y = "Number of transferred fibres") +
  guides(color = guide_legend(title = NULL, override.aes = list(shape = coder_shapes, linetype = coder_linetypes))) +
  scale_y_continuous(breaks = seq(-2, 28, by = 5), limits = c(-1, 28), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 16, by = 2), limits = c(1, 16), expand = c(0.01, 0)) +
  scale_color_manual(values = coder_colors, guide = FALSE) +
  scale_shape_manual(values = coder_shapes, guide = FALSE) +  
  scale_linetype_manual(values = coder_linetypes, guide = FALSE) +  # Set different line types for the regression lines
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "grey95", size = 1, linetype = "solid", colour = "grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(size = 12)) +
  geom_smooth(aes(linetype = as.factor(group)), method = lm, se = FALSE, size = 0.7)+
  annotate(geom="text", x=15, y=26.5, label="bar(x) == 7.8",color="black",parse=T)

#### Final graph - Figure 10 ####
pCombinedRTWash_pending <- ggarrange(pRTWashBpara_combined+ rremove("ylab") + rremove("xlab"),
                                     pRTWashBperp_combined+ rremove("ylab") + rremove("xlab"),
                                     labels = NULL,
                                     common.legend = TRUE, legend = "bottom",
                                     align = "hv",
                                     ncol = 1, nrow = 2,
                                     font.label = list(size = 8, color = "black", family = NULL, position = "top"))

pCombinedRTWash <- annotate_figure(pCombinedRTWash_pending, left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                   bottom = textGrob("\nRepetitive transfer number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedRTWash

#to save the graph
ggsave("Repetitive transfer with wash.png", pCombinedRTWash, width =6, height = 8, units = "in", dpi=300,path = "Results")

# ------------------------------------------------------------------------
# Section 2: Variability between and within garments
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Subsection 2-1 : inter-garment variability
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# data from the second series involving washing 5 donor garments
# Split Transfer_G5 per garment
Garment1_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G1', garment))
Garment2_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G2', garment))
Garment3_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G3', garment))
Garment4_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G4', garment))
Garment5_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G5', garment))

# Create a table with means per garment
meanGarment1_Transfer_G5 <- aggregate(value ~ wash, Garment1_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G5 <- aggregate(value ~ wash, Garment2_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G5 <- aggregate(value ~ wash, Garment3_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment4_Transfer_G5 <- aggregate(value ~ wash, Garment4_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment5_Transfer_G5 <- aggregate(value ~ wash, Garment5_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG5_forCorrPlot <- cbind(G1=meanGarment1_Transfer_G5$value,G2=meanGarment2_Transfer_G5$value,
                               G3=meanGarment3_Transfer_G5$value,G4=meanGarment4_Transfer_G5$value,
                               G5=meanGarment5_Transfer_G5$value)

# Split each garment by parallel and perpendicular contact areas
Garment1_Transfer_G5_para <- Garment1_Transfer_G5 %>% filter(grepl('^[1-5]$', band))
Garment2_Transfer_G5_para <- Garment2_Transfer_G5 %>% filter(grepl('^[1-5]$', band))
Garment3_Transfer_G5_para <- Garment3_Transfer_G5 %>% filter(grepl('^[1-5]$', band))
Garment4_Transfer_G5_para <- Garment4_Transfer_G5 %>% filter(grepl('^[1-5]$', band))
Garment5_Transfer_G5_para <- Garment5_Transfer_G5 %>% filter(grepl('^[1-5]$', band))

Garment1_Transfer_G5_perp <- Garment1_Transfer_G5 %>% filter(grepl('^[6-9]|10$', band))
Garment2_Transfer_G5_perp <- Garment2_Transfer_G5 %>% filter(grepl('^[6-9]|10$', band))
Garment3_Transfer_G5_perp <- Garment3_Transfer_G5 %>% filter(grepl('^[6-9]|10$', band))
Garment4_Transfer_G5_perp <- Garment4_Transfer_G5 %>% filter(grepl('^[6-9]|10$', band))
Garment5_Transfer_G5_perp <- Garment5_Transfer_G5 %>% filter(grepl('^[6-9]|10$', band))

# Create a table with means per garment
meanGarment1_Transfer_G5_para <- aggregate(value ~ wash + garment, Garment1_Transfer_G5_para, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G5_para <- aggregate(value ~ wash + garment, Garment2_Transfer_G5_para, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G5_para <- aggregate(value ~ wash + garment, Garment3_Transfer_G5_para, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment4_Transfer_G5_para <- aggregate(value ~ wash + garment, Garment4_Transfer_G5_para, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment5_Transfer_G5_para <- aggregate(value ~ wash + garment, Garment5_Transfer_G5_para, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG5_para <- rbind(meanGarment1_Transfer_G5_para,meanGarment2_Transfer_G5_para,meanGarment3_Transfer_G5_para,meanGarment4_Transfer_G5_para,meanGarment5_Transfer_G5_para)

meanGarment1_Transfer_G5_perp <- aggregate(value ~ wash + garment, Garment1_Transfer_G5_perp, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G5_perp <- aggregate(value ~ wash + garment, Garment2_Transfer_G5_perp, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G5_perp <- aggregate(value ~ wash + garment, Garment3_Transfer_G5_perp, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment4_Transfer_G5_perp <- aggregate(value ~ wash + garment, Garment4_Transfer_G5_perp, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment5_Transfer_G5_perp <- aggregate(value ~ wash + garment, Garment5_Transfer_G5_perp, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG5_perp <- rbind(meanGarment1_Transfer_G5_perp,meanGarment2_Transfer_G5_perp,meanGarment3_Transfer_G5_perp,meanGarment4_Transfer_G5_perp,meanGarment5_Transfer_G5_perp)


# data from the second series involving washing 5 donor garments
# Split Transfer_G12 per garment
Garment1_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G1', garment))
Garment2_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G2', garment))
Garment3_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G3', garment))

# convert every column into numeric values
Garment1_Transfer_G12$value <- as.numeric(Garment1_Transfer_G12$value)
Garment2_Transfer_G12$value <- as.numeric(Garment2_Transfer_G12$value)
Garment3_Transfer_G12$value <- as.numeric(Garment3_Transfer_G12$value)

# Create a table with means per garment
meanGarment1_Transfer_G12 <- aggregate(value ~ wash, Garment1_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G12 <- aggregate(value ~ wash, Garment2_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G12 <- aggregate(value ~ wash, Garment3_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG12_forCorrPlot <- cbind(G1=meanGarment1_Transfer_G12$value,G2=meanGarment2_Transfer_G12$value,
                                G3=meanGarment3_Transfer_G12$value)

# Split each garment by parallel and perpendicular contact areas
Garment1_Transfer_G12_para <- Garment1_Transfer_G12 %>% filter(grepl('^[1-5]$', band))
Garment2_Transfer_G12_para <- Garment2_Transfer_G12 %>% filter(grepl('^[1-5]$', band))
Garment3_Transfer_G12_para <- Garment3_Transfer_G12 %>% filter(grepl('^[1-5]$', band))

Garment1_Transfer_G12_perp <- Garment1_Transfer_G12 %>% filter(grepl('^[6-9]|10$', band))
Garment2_Transfer_G12_perp <- Garment2_Transfer_G12 %>% filter(grepl('^[6-9]|10$', band))
Garment3_Transfer_G12_perp <- Garment3_Transfer_G12 %>% filter(grepl('^[6-9]|10$', band))

# Create a table with means per garment
meanGarment1_Transfer_G12_para <- aggregate(value ~ wash + garment, Garment1_Transfer_G12_para, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G12_para <- aggregate(value ~ wash + garment, Garment2_Transfer_G12_para, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G12_para <- aggregate(value ~ wash + garment, Garment3_Transfer_G12_para, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG12_para <- rbind(meanGarment1_Transfer_G12_para,meanGarment2_Transfer_G12_para,meanGarment3_Transfer_G12_para)

meanGarment1_Transfer_G12_perp <- aggregate(value ~ wash + garment, Garment1_Transfer_G12_perp, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G12_perp <- aggregate(value ~ wash + garment, Garment2_Transfer_G12_perp, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G12_perp <- aggregate(value ~ wash + garment, Garment3_Transfer_G12_perp, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG12_perp <- rbind(meanGarment1_Transfer_G12_perp,meanGarment2_Transfer_G12_perp,meanGarment3_Transfer_G12_perp)


#### Intermediate Data Visualisation ####
pGarment_G5_para <-ggplot(data = MeanTotG5_para, aes(x =wash, y = value, color=garment, group=garment)) +
  geom_line(aes(linetype=garment), size=0.3)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b"))+
  scale_linetype_manual(values=c("solid","solid", "solid","solid","solid"))+
  ylim(-2,35)+
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

pGarment_G5_perp <-ggplot(data = MeanTotG5_perp, aes(x =wash, y = value, color=garment, group=garment)) +
  geom_line(aes(linetype=garment), size=0.3)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b"))+
  scale_linetype_manual(values=c("solid","solid", "solid","solid","solid"))+
  ylim(-2,35)+
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

pG5combined_pending <- ggarrange(pGarment_G5_para+rremove("ylab")+rremove("xlab"),
                                 pGarment_G5_perp+rremove("ylab")+rremove("xlab"),
                                 labels = c("A",""),
                                 common.legend = T, legend = "right",
                                 align = "h", widths = c(1,1),
                                 font.label = list(size = 12, color = "black", family = "Arial", position = "top"),
                                 hjust=-0.5, vjust=1)

pGarment_G12_para <-ggplot(data = MeanTotG12_para, aes(x =wash, y = value, color=garment, group=garment)) +
  geom_line(aes(linetype=garment), size=0.3)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b"))+
  scale_linetype_manual(values=c("solid","solid", "solid","solid","solid"))+
  ylim(-2,35)+
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

pGarment_G12_perp <-ggplot(data = MeanTotG12_perp, aes(x =wash, y = value, color=garment, group=garment)) +
  geom_line(aes(linetype=garment), size=0.3)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b"))+
  scale_linetype_manual(values=c("solid","solid", "solid","solid","solid"))+
  ylim(-2,35)+
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

pG12combined_pending <- ggarrange(pGarment_G12_para+rremove("ylab")+rremove("xlab"),
                                 pGarment_G12_perp+rremove("ylab")+rremove("xlab"),
                                 labels = c("B",""),
                                 common.legend = T, legend = "right",
                                 align = "h", widths = c(1,1),
                                 ncol = 2, nrow = 1,
                                 font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                 hjust=-0.8, vjust=1)
pG12combined_pending

#### Final graph - Figure 11 ####
pIntercombined_pending <- ggarrange(pG5combined_pending,
                                    pG12combined_pending,
                                   common.legend = T, legend = "right",
                                   align = "h", widths = c(1,1),
                                   ncol = 1, nrow = 2,
                                   font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                   hjust=-0.3, vjust=2)
pIntercombined_pending

pIntercombined <- annotate_figure(pIntercombined_pending,
                                  left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                  bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)),
                                  right = text_grob(bquote("Garments"), rot = 90),
                                  top = textGrob("Parallel contact areas                     Perpendicular contact areas", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1.05)))
pIntercombined

# to save the graph
ggsave("Intervariability.png", pIntercombined, width =7, height = 5, units = "in", dpi=300,path = "Results")

# ------------------------------------------------------------------------
# Subsection 2-1 : intra-garment variability
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# data from the first series involving washing a single donor garment
# Combined Dataset for parallel contact area
G1_DatasetB1 <- Transfer_G1 %>% filter(grepl("\\b1\\b", band)) #The \\b in the regular expression ensures that "1" is matched as a whole word, and it won't select rows with "10" or "100."
G1_DatasetB2 <- Transfer_G1 %>% filter(grepl("\\b2\\b", band))
G1_DatasetB3 <- Transfer_G1 %>% filter(grepl("\\b3\\b", band))
G1_DatasetB4 <- Transfer_G1 %>% filter(grepl("\\b4\\b", band))
G1_DatasetB5 <- Transfer_G1 %>% filter(grepl("\\b5\\b", band))

# Combined Dataset for perpendicular contact area
G1_DatasetB6 <- Transfer_G1 %>% filter(grepl("\\b6\\b", band))
G1_DatasetB7 <- Transfer_G1 %>% filter(grepl("\\b7\\b", band))
G1_DatasetB8 <- Transfer_G1 %>% filter(grepl("\\b8\\b", band))
G1_DatasetB9 <- Transfer_G1 %>% filter(grepl("\\b9\\b", band))
G1_DatasetB10 <- Transfer_G1 %>% filter(grepl("\\b10\\b", band))

# transform value into numeric
G1_DatasetB1$value <- as.numeric(G1_DatasetB1$value)
G1_DatasetB2$value <- as.numeric(G1_DatasetB2$value)
G1_DatasetB3$value <- as.numeric(G1_DatasetB3$value)
G1_DatasetB4$value <- as.numeric(G1_DatasetB4$value)
G1_DatasetB5$value <- as.numeric(G1_DatasetB5$value)
G1_DatasetB6$value <- as.numeric(G1_DatasetB6$value)
G1_DatasetB7$value <- as.numeric(G1_DatasetB7$value)
G1_DatasetB8$value <- as.numeric(G1_DatasetB8$value)
G1_DatasetB9$value <- as.numeric(G1_DatasetB9$value)
G1_DatasetB10$value <- as.numeric(G1_DatasetB10$value)

# Create a table with means per contact area
G1_meanB1 <- aggregate(value ~  wash, G1_DatasetB1, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB1$`Contact area` <- 1
G1_meanB2 <- aggregate(value ~  wash, G1_DatasetB2, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB2$`Contact area` <- 2
G1_meanB3 <- aggregate(value ~  wash, G1_DatasetB3, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB3$`Contact area` <- 3
G1_meanB4 <- aggregate(value ~  wash, G1_DatasetB4, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB4$`Contact area` <- 4
G1_meanB5 <- aggregate(value ~  wash, G1_DatasetB5, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB5$`Contact area` <- 5
G1_meanB6 <- aggregate(value ~  wash, G1_DatasetB6, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB6$`Contact area` <- 6
G1_meanB7 <- aggregate(value ~  wash, G1_DatasetB7, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB7$`Contact area` <- 7
G1_meanB8 <- aggregate(value ~  wash, G1_DatasetB8, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB8$`Contact area` <- 8
G1_meanB9 <- aggregate(value ~  wash, G1_DatasetB9, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB9$`Contact area` <- 9
G1_meanB10 <- aggregate(value ~  wash, G1_DatasetB10, FUN = function(x) {round(mean(x), digits = 2)})
G1_meanB10$`Contact area` <- 10
G1_MeanTotContactArea <- rbind(G1_meanB1,G1_meanB2,G1_meanB3,G1_meanB4,G1_meanB5,G1_meanB6,G1_meanB7,G1_meanB8,G1_meanB9,G1_meanB10)

# parallel contact area
G1_MeanTotpara <- rbind(G1_meanB1,G1_meanB2,G1_meanB3,G1_meanB4,G1_meanB5)
# perpendicular contact area
G1_MeanTotperp <- rbind(G1_meanB6,G1_meanB7,G1_meanB8,G1_meanB9,G1_meanB10)

# average number of fibres retrieved on parallel contact area, per wash
G1_meanPara <- aggregate(value ~  wash, G1_MeanTotpara, FUN = function(x) {round(mean(x), digits = 2)})
# average number of fibres retrieved on perpendicular contact area, per wash
G1_meanPerp <- aggregate(value ~  wash, G1_MeanTotperp, FUN = function(x) {round(mean(x), digits = 2)})

# average number of fibres retrieved on parallel contact area, all washes combined
mean(G1_meanPara$value)
# average number of fibres retrieved on perpendicular contact area, all washes combined
mean(G1_meanPerp$value)

# data from the second series involving washing 5 donor garments
# Combined Dataset for parallel contact area
G5_DatasetB1 <- Transfer_G5 %>% filter(grepl("\\b1\\b", band)) #The \\b in the regular expression ensures that "1" is matched as a whole word, and it won't select rows with "10" or "100."
G5_DatasetB2 <- Transfer_G5 %>% filter(grepl("\\b2\\b", band))
G5_DatasetB3 <- Transfer_G5 %>% filter(grepl("\\b3\\b", band))
G5_DatasetB4 <- Transfer_G5 %>% filter(grepl("\\b4\\b", band))
G5_DatasetB5 <- Transfer_G5 %>% filter(grepl("\\b5\\b", band))

# Combined Dataset for perpendicular contact area
G5_DatasetB6 <- Transfer_G5 %>% filter(grepl("\\b6\\b", band))
G5_DatasetB7 <- Transfer_G5 %>% filter(grepl("\\b7\\b", band))
G5_DatasetB8 <- Transfer_G5 %>% filter(grepl("\\b8\\b", band))
G5_DatasetB9 <- Transfer_G5 %>% filter(grepl("\\b9\\b", band))
G5_DatasetB10 <- Transfer_G5 %>% filter(grepl("\\b10\\b", band))

# Create a table with means per contact area
G5_meanB1 <- aggregate(value ~  wash, G5_DatasetB1, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB1$`Contact area` <-1
G5_meanB2 <- aggregate(value ~  wash, G5_DatasetB2, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB2$`Contact area` <- 2
G5_meanB3 <- aggregate(value ~  wash, G5_DatasetB3, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB3$`Contact area` <- 3
G5_meanB4 <- aggregate(value ~  wash, G5_DatasetB4, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB4$`Contact area` <- 4
G5_meanB5 <- aggregate(value ~  wash, G5_DatasetB5, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB5$`Contact area` <- 5
G5_meanB6 <- aggregate(value ~  wash, G5_DatasetB6, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB6$`Contact area` <- 6
G5_meanB7 <- aggregate(value ~  wash, G5_DatasetB7, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB7$`Contact area` <- 7
G5_meanB8 <- aggregate(value ~  wash, G5_DatasetB8, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB8$`Contact area` <- 8
G5_meanB9 <- aggregate(value ~  wash, G5_DatasetB9, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB9$`Contact area` <- 9
G5_meanB10 <- aggregate(value ~  wash, G5_DatasetB10, FUN = function(x) {round(mean(x), digits = 2)})
G5_meanB10$`Contact area` <- 10
G5_MeanTotContactArea <- rbind(G5_meanB1,G5_meanB2,G5_meanB3,G5_meanB4,G5_meanB5,G5_meanB6,G5_meanB7,G5_meanB8,G5_meanB9,G5_meanB10)

# parallel contact area
G5_MeanTotpara <- rbind(G5_meanB1,G5_meanB2,G5_meanB3,G5_meanB4,G5_meanB5)
# perpendicular contact area
G5_MeanTotperp <- rbind(G5_meanB6,G5_meanB7,G5_meanB8,G5_meanB9,G5_meanB10)

# average number of fibres retrieved on parallel contact area, per wash
G5_meanPara <- aggregate(value ~  wash, G5_MeanTotpara, FUN = function(x) {round(mean(x), digits = 2)})
# average number of fibres retrieved on perpendicular contact area, per wash
G5_meanPerp <- aggregate(value ~  wash, G5_MeanTotperp, FUN = function(x) {round(mean(x), digits = 2)})

# average number of fibres retrieved on parallel contact area, all washes combined
mean(G5_meanPara$value)
# average number of fibres retrieved on perpendicular contact area, all washes combined
mean(G5_meanPerp$value)

# data from the third series involving washing 12 donor garments
# Combined Dataset for parallel contact area
G12_DatasetB1 <- Transfer_G12 %>% filter(grepl("\\b1\\b", band)) #The \\b in the regular expression ensures that "1" is matched as a whole word, and it won't select rows with "10" or "100."
G12_DatasetB2 <- Transfer_G12 %>% filter(grepl("\\b2\\b", band))
G12_DatasetB3 <- Transfer_G12 %>% filter(grepl("\\b3\\b", band))
G12_DatasetB4 <- Transfer_G12 %>% filter(grepl("\\b4\\b", band))
G12_DatasetB5 <- Transfer_G12 %>% filter(grepl("\\b5\\b", band))

# Combined Dataset for perpendicular contact area
G12_DatasetB6 <- Transfer_G12 %>% filter(grepl("\\b6\\b", band))
G12_DatasetB7 <- Transfer_G12 %>% filter(grepl("\\b7\\b", band))
G12_DatasetB8 <- Transfer_G12 %>% filter(grepl("\\b8\\b", band))
G12_DatasetB9 <- Transfer_G12 %>% filter(grepl("\\b9\\b", band))
G12_DatasetB10 <- Transfer_G12 %>% filter(grepl("\\b10\\b", band))

# transform value into numeric
G12_DatasetB1$value <- as.numeric(G12_DatasetB1$value)
G12_DatasetB2$value <- as.numeric(G12_DatasetB2$value)
G12_DatasetB3$value <- as.numeric(G12_DatasetB3$value)
G12_DatasetB4$value <- as.numeric(G12_DatasetB4$value)
G12_DatasetB5$value <- as.numeric(G12_DatasetB5$value)
G12_DatasetB6$value <- as.numeric(G12_DatasetB6$value)
G12_DatasetB7$value <- as.numeric(G12_DatasetB7$value)
G12_DatasetB8$value <- as.numeric(G12_DatasetB8$value)
G12_DatasetB9$value <- as.numeric(G12_DatasetB9$value)
G12_DatasetB10$value <- as.numeric(G12_DatasetB10$value)

# Create a table with means per contact area
G12_meanB1 <- aggregate(value ~  wash, G12_DatasetB1, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB1$`Contact area` <-1
G12_meanB2 <- aggregate(value ~  wash, G12_DatasetB2, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB2$`Contact area` <- 2
G12_meanB3 <- aggregate(value ~  wash, G12_DatasetB3, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB3$`Contact area` <- 3
G12_meanB4 <- aggregate(value ~  wash, G12_DatasetB4, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB4$`Contact area` <- 4
G12_meanB5 <- aggregate(value ~  wash, G12_DatasetB5, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB5$`Contact area` <- 5
G12_meanB6 <- aggregate(value ~  wash, G12_DatasetB6, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB6$`Contact area` <- 6
G12_meanB7 <- aggregate(value ~  wash, G12_DatasetB7, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB7$`Contact area` <- 7
G12_meanB8 <- aggregate(value ~  wash, G12_DatasetB8, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB8$`Contact area` <- 8
G12_meanB9 <- aggregate(value ~  wash, G12_DatasetB9, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB9$`Contact area` <- 9
G12_meanB10 <- aggregate(value ~  wash, G12_DatasetB10, FUN = function(x) {round(mean(x), digits = 2)})
G12_meanB10$`Contact area` <- 10
G12_MeanTotContactArea <- rbind(G12_meanB1,G12_meanB2,G12_meanB3,G12_meanB4,G12_meanB5,G12_meanB6,G12_meanB7,G12_meanB8,G12_meanB9,G12_meanB10)
G12_MeanTotContactArea_export <- cbind(G12_meanB1$wash,G12_meanB1$value,G12_meanB2$value,G12_meanB3$value,G12_meanB4$value,G12_meanB5$value,G12_meanB6$value,G12_meanB7$value,G12_meanB8$value,G12_meanB9$value,G12_meanB10$value)

# parallel contact area
G12_MeanTotpara <- rbind(G12_meanB1,G12_meanB2,G12_meanB3,G12_meanB4,G12_meanB5)
# perpendicular contact area
G12_MeanTotperp <- rbind(G12_meanB6,G12_meanB7,G12_meanB8,G12_meanB9,G12_meanB10)

# average number of fibres retrieved on parallel contact area, per wash
G12_meanPara <- aggregate(value ~  wash, G12_MeanTotpara, FUN = function(x) {round(mean(x), digits = 2)})
# average number of fibres retrieved on perpendicular contact area, per wash
G12_meanPerp <- aggregate(value ~  wash, G12_MeanTotperp, FUN = function(x) {round(mean(x), digits = 2)})

# average number of fibres retrieved on parallel contact area, all washes combined
mean(G12_meanPara$value)
# average number of fibres retrieved on perpendicular contact area, all washes combined
mean(G12_meanPerp$value)

#### Intermediate Data Visualisation ####
G1_MeanTotpara[61,] <- c("W008", NA, NA)
G1_MeanTotpara[62,] <- c("W010", NA, NA)
G1_MeanTotpara[63,] <- c("W012", NA, NA)
G1_MeanTotpara[64,] <- c("W014", NA, NA)
pG1_bandspara <-ggplot(data = G1_MeanTotpara, aes(x = as.factor(wash), y = as.numeric(value), color = `Contact area`, group = `Contact area`)) +
  geom_line(aes(linetype=`Contact area`, color=`Contact area`), size=0.5)+
  labs(x = "\nWash number", y = "\nNumber of Fibre\n") +
  scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-2,35)+
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

G1_MeanTotperp[61,] <- c("W008", NA, NA)
G1_MeanTotperp[62,] <- c("W010", NA, NA)
G1_MeanTotperp[63,] <- c("W012", NA, NA)
G1_MeanTotperp[64,] <- c("W014", NA, NA)
pG1_bandsperp <- ggplot(data = G1_MeanTotperp, aes(x = as.factor(wash), y = as.numeric(value), color = `Contact area`, group = `Contact area`)) +
  geom_line(aes(linetype = `Contact area`, color = `Contact area`)) +
  labs(x = "\nWash number", y = "\nNumber of Fibre\n") +
  scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-2,35)+
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))

pbandscombinedG1_pending <- ggarrange(pG1_bandspara+ rremove("ylab") + rremove("xlab"),
                                    pG1_bandsperp+ rremove("ylab") + rremove("xlab"),
                                    common.legend = T, legend = "right",
                                    labels = c("A"),
                                    align = "h", widths = c(1,1),
                                    ncol = 2, nrow = 1,
                                    font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                    hjust=-1, vjust=1)
pbandscombinedG1_pending

pG5_bandspara <-ggplot(data = G5_MeanTotpara, aes(x =wash, y = value, color=as.factor(`Contact area`), group=as.factor(`Contact area`))) +
  geom_line(aes(linetype=as.factor(`Contact area`), color=as.factor(`Contact area`)))+
  labs(x="\nWash number", y="\nNumber of Fibre\n")+
  ylim(-2,35)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

pG5_bandsperp <-ggplot(data = G5_MeanTotperp, aes(x =wash, y = value, color=as.factor(`Contact area`), group=as.factor(`Contact area`))) +
  geom_line(aes(linetype=as.factor(`Contact area`), color=as.factor(`Contact area`)))+
  labs(x="\nWash number", y="\nNumber of Fibre\n")+
  ylim(-2,35)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

pbandscombinedG5_pending <- ggarrange(pG5_bandspara+ rremove("ylab") + rremove("xlab"),
                                    pG5_bandsperp+ rremove("ylab") + rremove("xlab"),
                                    common.legend = T, legend = "right",
                                    labels = c("B"),
                                    align = "h", widths = c(1,1),
                                    ncol = 2, nrow = 1,
                                    font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                    hjust=-1, vjust=1)
pbandscombinedG5_pending

G12_MeanTotpara[156,] <- c("W016", NA, NA)
G12_MeanTotpara[157,] <- c("W017", NA, NA)
G12_MeanTotpara[158,] <- c("W024", NA, NA)
G12_MeanTotpara[159,] <- c("W026", NA, NA)
G12_MeanTotpara[160,] <- c("W028", NA, NA)
G12_MeanTotpara[161,] <- c("W030", NA, NA)
G12_MeanTotpara[162,] <- c("W032", NA, NA)
G12_MeanTotpara[163,] <- c("W034", NA, NA)
G12_MeanTotpara[164,] <- c("W036", NA, NA)
G12_MeanTotpara[165,] <- c("W038", NA, NA)
G12_MeanTotpara[166,] <- c("W040", NA, NA)
pG12_bandspara <-ggplot(data = G12_MeanTotpara, aes(x =as.factor(wash), y = as.numeric(value), color=`Contact area`, group=`Contact area`)) +
  geom_line(aes(linetype=`Contact area`, color=`Contact area`))+
  labs(x="\nWash number", y="\nNumber of Fibre\n")+
  ylim(-2,35)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

G12_MeanTotperp[156,] <- c("W016", NA, NA)
G12_MeanTotperp[157,] <- c("W017", NA, NA)
G12_MeanTotperp[158,] <- c("W024", NA, NA)
G12_MeanTotperp[159,] <- c("W026", NA, NA)
G12_MeanTotperp[160,] <- c("W028", NA, NA)
G12_MeanTotperp[161,] <- c("W030", NA, NA)
G12_MeanTotperp[162,] <- c("W032", NA, NA)
G12_MeanTotperp[163,] <- c("W034", NA, NA)
G12_MeanTotperp[164,] <- c("W036", NA, NA)
G12_MeanTotperp[165,] <- c("W038", NA, NA)
G12_MeanTotperp[166,] <- c("W040", NA, NA)
# Modify the levels of the `Contact area` variable
G12_MeanTotperp$`Contact area` <- factor(G12_MeanTotperp$`Contact area`, levels = c(6, 7, 8, 9, 10))
pG12_bandsperp <-ggplot(data = G12_MeanTotperp, aes(x =as.factor(wash), y = as.numeric(value), color=`Contact area`, group=`Contact area`)) +
  geom_line(aes(linetype=`Contact area`, color=`Contact area`))+
  labs(x="\nWash number", y="\nNumber of Fibre\n")+
  ylim(-2,35)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

pbandscombinedG12_pending <- ggarrange(pG12_bandspara+ rremove("ylab") + rremove("xlab"),
                                    pG12_bandsperp+ rremove("ylab") + rremove("xlab"),
                                    common.legend = T, legend = "right",
                                    labels = c("C"),
                                    align = "h", widths = c(1,1),
                                    ncol = 2, nrow = 1,
                                    font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                    hjust=-1, vjust=1)
pbandscombinedG12_pending

#### Final graph - Figure 12 ####
pIntracombined_pending <- ggarrange(pbandscombinedG1_pending,
                                    pbandscombinedG5_pending,
                                    pbandscombinedG12_pending,
                                    common.legend = T, legend = "right",
                                    align = "h", widths = c(1,1,1),
                                    ncol = 1, nrow = 3,
                                    font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                    hjust=-0.3, vjust=2)
pIntracombined_pending

pIntracombined <- annotate_figure(pIntracombined_pending,
                                  left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                  bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)),
                                  right = text_grob(bquote("\nContact areas"), rot = 90),
                                  top = textGrob("Parallel contact areas                        Perpendicular contact areas", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1.05)))
pIntracombined

# to save the graph
ggsave("Intravariability.png", pIntracombined, width =7, height = 8, units = "in", dpi=300,path = "Results")

#### Final graph - Figure S5 to S7 (supplementary information) ####
MeanTotBG5_forCorrPlot <-data.frame(cbind(G1para=meanGarment1_Transfer_G5_para$value, G2para=meanGarment2_Transfer_G5_para$value,
                                         G3para=meanGarment3_Transfer_G5_para$value,G4para=meanGarment4_Transfer_G5_para$value,
                                         G5para=meanGarment5_Transfer_G5_para$value,G1perp=meanGarment1_Transfer_G5_perp$value, G2perp=meanGarment2_Transfer_G5_perp$value,
                                         G3perp=meanGarment3_Transfer_G5_perp$value,G4perp=meanGarment4_Transfer_G5_perp$value,
                                         G5perp=meanGarment5_Transfer_G5_perp$value))
MeanTotBG12_forCorrPlot <-data.frame(cbind(G1para=meanGarment1_Transfer_G12_para$value, G2para=meanGarment2_Transfer_G12_para$value,
                                         G3para=meanGarment3_Transfer_G12_para$value,G1perp=meanGarment1_Transfer_G12_perp$value, G2perp=meanGarment2_Transfer_G12_perp$value,
                                         G3perp=meanGarment3_Transfer_G12_perp$value))

Garment1_Transfer_G1_para <- Transfer_G1 %>% filter(grepl('^[1-5]$', band))
Garment1_Transfer_G1_perp <- Transfer_G1 %>% filter(grepl('^[6-9]|10$', band))
MeanTotBG1_forCorrPlot <-data.frame(cbind(G1para=as.numeric(Garment1_Transfer_G1_para$value), G1perp=as.numeric(Garment1_Transfer_G1_perp$value)))

# data from the first series involving washing a single donor garment - Figure S5
cor_matrix <- cor(MeanTotBG1_forCorrPlot,use = "complete.obs")
head(cor_matrix)
pairs.panels(MeanTotBG1_forCorrPlot[,1:2],
             stars = TRUE, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)

# data from the second series involving washing 5 donor garments - Figure S6
cor_matrix <- cor(MeanTotBG5_forCorrPlot,use = "complete.obs")
head(cor_matrix)
pairs.panels(MeanTotBG5_forCorrPlot[,1:10],
             stars = TRUE, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)

# data from the third series involving washing 12 donor garments - Figure S7
cor_matrix <- cor(MeanTotBG12_forCorrPlot,use = "complete.obs")
head(cor_matrix)
pairs.panels(MeanTotBG12_forCorrPlot[,1:6],
             stars = TRUE, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)

# ------------------------------------------------------------------------
# Section 3: Fibres in wastewater vs fibres transferred
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
# create a new variable for the fibres released in the wastewater, normalised to the volume of water
Garment1_mgvol <- data.frame(cbind(Wastewatervolume_G1$Washnumber, Wastewatervolume_G1$Total, Wastewaterfibres_G1p4$Diff.FN))
Garment1_mgvol$mgvol <- round(Garment1_mgvol$X2/Garment1_mgvol$X1, digits =2)

Garment5_mgvol <- data.frame(cbind(Wastewatervolume_G5$Washnumber,Wastewatervolume_G5$Total, Wastewaterfibres_G5p4$Diff.FN))
Garment5_mgvol$mgvol <- round(Garment5_mgvol$X2/Garment5_mgvol$X1, digits =2)

Garment12_mgvol <- data.frame(cbind(Wastewatervolume_G12$Washnumber,Wastewatervolume_G12$Total, Wastewaterfibres_G12p4$Diff.FN))
Garment12_mgvol$mgvol <- round(Garment12_mgvol$X2/Garment12_mgvol$X1, digits =2)

# add in each dataframe the average number of transferred fibres (all CA and garment combined)
specific_values <- c("1","2","3","4","5","6","7","9","11","13","15")
Garment1_mgvol <- subset(Garment1_mgvol, X1 %in% specific_values)
text_to_remove <- "W000"
meanAtrG1 <- meanAtrG1[!grepl(text_to_remove, meanAtrG1$wash), ]
Garment1_mgvol <- data.frame(cbind(Garment1_mgvol, fibre=meanAtrG1$value))

text_to_remove2 <-"W034"
meanAtrG5 <- meanAtrG5[!grepl(text_to_remove, meanAtrG5$wash), ]
meanAtrG5 <- meanAtrG5[!grepl(text_to_remove2, meanAtrG5$wash), ]
Garment5_mgvol <- data.frame(cbind(Garment5_mgvol, fibre=meanAtrG5$value))

meanAtrG12 <- meanAtrG12[!grepl(text_to_remove, meanAtrG12$wash), ]
text_to_remove <- c(16,17,24,26,28,30,32,34,36,38,40)
Garment12_mgvol <- Garment12_mgvol[!as.character(Garment12_mgvol$X1) %in% as.character(text_to_remove), ]
Garment12_mgvol <- data.frame(cbind(Garment12_mgvol, fibre=meanAtrG12$value))

#### Intermediate Data Visualisation ####
PearsonpmgvolG1 <- ggscatter(Garment1_mgvol, x = "fibre", y = "mgvol",
                             add = "reg.line",
                             xlab = "Number of fibres transferred", ylab = "fibre released per volume of wastewater(mg/L)",
                             ylim = c(0,30),
                             xlim = c(0,10))+
  annotate(geom="text", x=1, y=29, label="R = 0.96\np < 0.0001",color="black")

PearsonpmgvolG5 <- ggscatter(Garment5_mgvol, x = "fibre", y = "mgvol",
                             add = "reg.line",
                             xlab = "Number of fibres transferred", ylab = "Fibre released per volume of wastewater(mg/L)",
                             ylim = c(0,12),
                             xlim = c(0,30))+
  annotate(geom="text", x=2, y=11.5, label="R = 0.77\np < 0.0001",color="black")

PearsonpmgvolG12 <- ggscatter(Garment12_mgvol, x = "fibre", y = "mgvol",
                              add = "reg.line",
                              xlab = "Number of fibres transferred", ylab = "fibre released per volume of wastewater(mg/L)",
                              ylim = c(0,12),
                              xlim = c(0,20))+
  annotate(geom="text", x=1.5, y=11.5, label="R = 0.63\np < 0.0001",color="black")

#### Final graph - Figure 13 #### 
pPearson_combined_pending <- ggarrange(PearsonpmgvolG1+ rremove("ylab") + rremove("xlab"),
                                       PearsonpmgvolG5+ rremove("ylab") + rremove("xlab"),
                                       PearsonpmgvolG12+ rremove("ylab") + rremove("xlab"),
                                       nrow = 3, labels = c("A", "B", "C"),
                                       vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)

pPearson_combined <- annotate_figure(pPearson_combined_pending, left = textGrob("Mass of fibre released per litre of wastewater(mg/L)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                     bottom = textGrob("\nAverage Number of fibres transferred", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pVolume_combined
pPearson_combined

# to save the graph
ggsave("pPearson_combined_waterVStransfer.png", pPearson_combined, width = 6, height = 9, units = "in", dpi=300, path = "Results")