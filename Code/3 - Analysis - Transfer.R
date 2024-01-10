#############################################################
#####                     To read                       #####
#############################################################
# This R script is to generate the figures related to:
# the transfer of fibres following washing
# Repetitive transfer (control garment)

#----------------------------------------------------------------------------------#
####                   Transfer of fibres following washing                    #####
#----------------------------------------------------------------------------------#
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
# write.table(Transfer_G1, file = "Results/Transfer_G1.csv", quote = F, sep = ",", row.names = F)
# write.table(Transfer_G5, file = "Results/Transfer_G5.csv", quote = F, sep = ",", row.names = F)
# write.table(Transfer_G12, file = "Results/Transfer_G12.csv", quote = F, sep = ",", row.names = F)

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
  compute_and_write_stats(dataframes[[i]], paste("Results/Stats-Atr-G", c(1, 5, 12)[i], ".csv", sep = ""))
}

##############################################################################################
#####    Garment inter-variability, split by parallel and perpendicular contact areas    #####
##############################################################################################
### 5 garments washed together
#### Split Transfer_G5 per garment ####
Garment1_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G1', garment))
Garment2_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G2', garment))
Garment3_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G3', garment))
Garment4_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G4', garment))
Garment5_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G5', garment))

#### Create a table with means per garment ####
meanGarment1_Transfer_G5 <- aggregate(value ~ wash, Garment1_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G5 <- aggregate(value ~ wash, Garment2_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G5 <- aggregate(value ~ wash, Garment3_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment4_Transfer_G5 <- aggregate(value ~ wash, Garment4_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment5_Transfer_G5 <- aggregate(value ~ wash, Garment5_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG5_forCorrPlot <- cbind(G1=meanGarment1_Transfer_G5$value,G2=meanGarment2_Transfer_G5$value,
                               G3=meanGarment3_Transfer_G5$value,G4=meanGarment4_Transfer_G5$value,
                               G5=meanGarment5_Transfer_G5$value)

#### Split each garment by parallel and perpendicular strips ####
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

#### Create a table with means per garment ####
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

#### GRAPH  ####
pGarment_G5_para <-ggplot(data = MeanTotG5_para, aes(x =wash, y = value, color=garment, group=garment)) +
  geom_line(aes(linetype=garment), size=0.3)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b"))+
  scale_linetype_manual(values=c("solid","solid", "solid","solid","solid"))+
  ylim(-2,30)+
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
#pGarment_G5_para
ggsave("Fibre Count per Garment_G5_para.png", pGarment_G5_para, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pGarment_G5_perp <-ggplot(data = MeanTotG5_perp, aes(x =wash, y = value, color=garment, group=garment)) +
  geom_line(aes(linetype=garment), size=0.3)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b"))+
  scale_linetype_manual(values=c("solid","solid", "solid","solid","solid"))+
  ylim(-2,30)+
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
#pGarment_G5_perp
ggsave("Fibre Count per Garment_G5_perp.png", pGarment_G5_perp, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pG5combined_pending <- ggarrange(pGarment_G5_para+rremove("ylab")+rremove("xlab"),
                                 pGarment_G5_perp+rremove("ylab")+rremove("xlab"),
                                 labels = c("A",""),
                                 common.legend = T, legend = "right",
                                 align = "h", widths = c(1,1),
                                 font.label = list(size = 12, color = "black", family = "Arial", position = "top"),
                                 hjust=-0.5, vjust=1)
pG5combined_pending

### 12 garments washed together
# Split Transfer_G12 per garment
Garment1_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G1', garment))
Garment2_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G2', garment))
Garment3_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G3', garment))

# convert every column into numeric values
Garment1_Transfer_G12$value <- as.numeric(Garment1_Transfer_G12$value)
Garment2_Transfer_G12$value <- as.numeric(Garment2_Transfer_G12$value)
Garment3_Transfer_G12$value <- as.numeric(Garment3_Transfer_G12$value)

#### Create a table with means per garment ####
meanGarment1_Transfer_G12 <- aggregate(value ~ wash, Garment1_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G12 <- aggregate(value ~ wash, Garment2_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G12 <- aggregate(value ~ wash, Garment3_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG12_forCorrPlot <- cbind(G1=meanGarment1_Transfer_G12$value,G2=meanGarment2_Transfer_G12$value,
                               G3=meanGarment3_Transfer_G12$value)
#### Split each garment by parallel and perpendicular strips ####
Garment1_Transfer_G12_para <- Garment1_Transfer_G12 %>% filter(grepl('^[1-5]$', band))
Garment2_Transfer_G12_para <- Garment2_Transfer_G12 %>% filter(grepl('^[1-5]$', band))
Garment3_Transfer_G12_para <- Garment3_Transfer_G12 %>% filter(grepl('^[1-5]$', band))

Garment1_Transfer_G12_perp <- Garment1_Transfer_G12 %>% filter(grepl('^[6-9]|10$', band))
Garment2_Transfer_G12_perp <- Garment2_Transfer_G12 %>% filter(grepl('^[6-9]|10$', band))
Garment3_Transfer_G12_perp <- Garment3_Transfer_G12 %>% filter(grepl('^[6-9]|10$', band))

#### Create a table with means per garment ####
meanGarment1_Transfer_G12_para <- aggregate(value ~ wash + garment, Garment1_Transfer_G12_para, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G12_para <- aggregate(value ~ wash + garment, Garment2_Transfer_G12_para, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G12_para <- aggregate(value ~ wash + garment, Garment3_Transfer_G12_para, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG12_para <- rbind(meanGarment1_Transfer_G12_para,meanGarment2_Transfer_G12_para,meanGarment3_Transfer_G12_para)

meanGarment1_Transfer_G12_perp <- aggregate(value ~ wash + garment, Garment1_Transfer_G12_perp, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G12_perp <- aggregate(value ~ wash + garment, Garment2_Transfer_G12_perp, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G12_perp <- aggregate(value ~ wash + garment, Garment3_Transfer_G12_perp, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG12_perp <- rbind(meanGarment1_Transfer_G12_perp,meanGarment2_Transfer_G12_perp,meanGarment3_Transfer_G12_perp)

#### GRAPH ####
pGarment_G12_para <-ggplot(data = MeanTotG12_para, aes(x =wash, y = value, color=garment, group=garment)) +
  geom_line(aes(linetype=garment), size=0.3)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b"))+
  scale_linetype_manual(values=c("solid","solid", "solid","solid","solid"))+
  ylim(-2,30)+
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
#pGarment_G12_para
ggsave("Fibre Count per Garment_G12_para.png", pGarment_G12_para, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pGarment_G12_perp <-ggplot(data = MeanTotG12_perp, aes(x =wash, y = value, color=garment, group=garment)) +
  geom_line(aes(linetype=garment), size=0.3)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b"))+
  scale_linetype_manual(values=c("solid","solid", "solid","solid","solid"))+
  ylim(-2,30)+
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
#pGarment_G12_perp
ggsave("Fibre Count per Garment_G12_perp.png", pGarment_G12_perp, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pG12combined_pending <- ggarrange(pGarment_G12_para+rremove("ylab")+rremove("xlab"),
                                 pGarment_G12_perp+rremove("ylab")+rremove("xlab"),
                                 labels = c("B",""),
                                 common.legend = T, legend = "right",
                                 align = "h", widths = c(1,1),
                                 ncol = 2, nrow = 1,
                                 font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                 hjust=-0.8, vjust=1)
pG12combined_pending

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
                                  right = text_grob(bquote("Contact areas"), rot = 90),
                                  top = textGrob("Parallel contact areas                     Perpendicular contact areas", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1.05)))
pIntercombined
ggsave("Intervariability.png", pIntercombined, width =7, height = 5, units = "in", dpi=300,path = "Results")

#### Corr plot for inter-variability ####
# the corplot has to be saved manually
cor_matrix <- cor(MeanTotG5_forCorrPlot,use = "complete.obs")
head(cor_matrix)
write.matrix(cor_matrix,file="GvsG_G5.csv", sep = ",")
pairs.panels(MeanTotG5_forCorrPlot[,1:5],
             stars = TRUE, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)

# the corplot has to be saved manually
cor_matrix <- cor(MeanTotG12_forCorrPlot,use = "complete.obs")
head(cor_matrix)
write.matrix(cor_matrix,file="GvsG_G12.csv", sep = ",")
pairs.panels(MeanTotG12_forCorrPlot[,1:3],
             stars = TRUE, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)

#####################################################################
#####   NUMBER OF FIBRES FOLLOWING TRANSFER PER CONTACT AREAS   #####
#####################################################################
# 1 garment
#### Combined Dataset for parallel contact area ####
G1_DatasetB1 <- Transfer_G1 %>% filter(grepl("\\b1\\b", band)) #The \\b in the regular expression ensures that "1" is matched as a whole word, and it won't select rows with "10" or "100."
G1_DatasetB2 <- Transfer_G1 %>% filter(grepl("\\b2\\b", band))
G1_DatasetB3 <- Transfer_G1 %>% filter(grepl("\\b3\\b", band))
G1_DatasetB4 <- Transfer_G1 %>% filter(grepl("\\b4\\b", band))
G1_DatasetB5 <- Transfer_G1 %>% filter(grepl("\\b5\\b", band))

#### Combined Dataset for perpendicular contact area ####
G1_DatasetB6 <- Transfer_G1 %>% filter(grepl("\\b6\\b", band))
G1_DatasetB7 <- Transfer_G1 %>% filter(grepl("\\b7\\b", band))
G1_DatasetB8 <- Transfer_G1 %>% filter(grepl("\\b8\\b", band))
G1_DatasetB9 <- Transfer_G1 %>% filter(grepl("\\b9\\b", band))
G1_DatasetB10 <- Transfer_G1 %>% filter(grepl("\\b10\\b", band))

#### Create a table with means per contact area ####
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

G1_MeanTotContactArea_export <- cbind(G1_meanB1$wash,G1_meanB1$value,G1_meanB2$value,G1_meanB3$value,G1_meanB4$value,G1_meanB5$value,G1_meanB6$value,G1_meanB7$value,G1_meanB8$value,G1_meanB9$value,G1_meanB10$value)
write.table(G1_MeanTotContactArea_export, file = "G1_Fibre per strip_export.csv", quote = F, sep = ",", row.names = F)

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

#### Graph ####
G1_MeanTotpara[61,] <- c("W008", NA, NA)
G1_MeanTotpara[62,] <- c("W010", NA, NA)
G1_MeanTotpara[63,] <- c("W012", NA, NA)
G1_MeanTotpara[64,] <- c("W014", NA, NA)

pG1_bandspara <-ggplot(data = G1_MeanTotpara, aes(x = as.factor(wash), y = as.numeric(value), color = `Contact area`, group = `Contact area`)) +
  geom_line(aes(linetype=`Contact area`, color=`Contact area`), size=0.5)+
  labs(x = "\nWash number", y = "\nNumber of Fibre\n") +
  scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-2,30)+
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
  #annotate(geom = "text", x = 15, y = 38, label = "1 garment", color = "#469990")
#pG1_bandspara

G1_MeanTotperp[61,] <- c("W008", NA, NA)
G1_MeanTotperp[62,] <- c("W010", NA, NA)
G1_MeanTotperp[63,] <- c("W012", NA, NA)
G1_MeanTotperp[64,] <- c("W014", NA, NA)

# Create the ggplot with the modified data
pG1_bandsperp <- ggplot(data = G1_MeanTotperp, aes(x = as.factor(wash), y = as.numeric(value), color = `Contact area`, group = `Contact area`)) +
  geom_line(aes(linetype = `Contact area`, color = `Contact area`)) +
  labs(x = "\nWash number", y = "\nNumber of Fibre\n") +
  scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(-2,30)+
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
  #annotate(geom = "text", x = 15, y = 38, label = "1 garment", color = "#469990")
#pG1_bandsperp

#### GRAPH - Figure 4-11 ####
pbandscombinedG1_pending <- ggarrange(pG1_bandspara+ rremove("ylab") + rremove("xlab"),
                                    pG1_bandsperp+ rremove("ylab") + rremove("xlab"),
                                    common.legend = T, legend = "right",
                                    labels = c("A"),
                                    align = "h", widths = c(1,1),
                                    ncol = 2, nrow = 1,
                                    font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                    hjust=-1, vjust=1)
pbandscombinedG1_pending

# 5 garments
#### Combined Dataset for parallel contact area ####
G5_DatasetB1 <- Transfer_G5 %>% filter(grepl("\\b1\\b", band)) #The \\b in the regular expression ensures that "1" is matched as a whole word, and it won't select rows with "10" or "100."
G5_DatasetB2 <- Transfer_G5 %>% filter(grepl("\\b2\\b", band))
G5_DatasetB3 <- Transfer_G5 %>% filter(grepl("\\b3\\b", band))
G5_DatasetB4 <- Transfer_G5 %>% filter(grepl("\\b4\\b", band))
G5_DatasetB5 <- Transfer_G5 %>% filter(grepl("\\b5\\b", band))

#### Combined Dataset for perpendicular contact area ####
G5_DatasetB6 <- Transfer_G5 %>% filter(grepl("\\b6\\b", band))
G5_DatasetB7 <- Transfer_G5 %>% filter(grepl("\\b7\\b", band))
G5_DatasetB8 <- Transfer_G5 %>% filter(grepl("\\b8\\b", band))
G5_DatasetB9 <- Transfer_G5 %>% filter(grepl("\\b9\\b", band))
G5_DatasetB10 <- Transfer_G5 %>% filter(grepl("\\b10\\b", band))

#### Create a table with means per contact area ####
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
write.table(G5_MeanTotContactArea, file = "G5_Fibre per strip_export.csv", quote = F, sep = ",", row.names = F)

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

#### graph #####
pG5_bandspara <-ggplot(data = G5_MeanTotpara, aes(x =wash, y = value, color=as.factor(`Contact area`), group=as.factor(`Contact area`))) +
  geom_line(aes(linetype=as.factor(`Contact area`), color=as.factor(`Contact area`)))+
  labs(x="\nWash number", y="\nNumber of Fibre\n")+
  ylim(-2,30)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  #annotate(geom = "text", x = 47.5, y = 38, label = "5 garments", color = "#000000")
pG5_bandspara
#ggsave("Fibre Count_bandspara.png", pG5_bandspara, width = 6, height = 4, units = "in", dpi=200, path = "Results")

pG5_bandsperp <-ggplot(data = G5_MeanTotperp, aes(x =wash, y = value, color=as.factor(`Contact area`), group=as.factor(`Contact area`))) +
  geom_line(aes(linetype=as.factor(`Contact area`), color=as.factor(`Contact area`)))+
  labs(x="\nWash number", y="\nNumber of Fibre\n")+
  ylim(-2,30)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  #annotate(geom = "text", x = 47.5, y = 38, label = "5 garments", color = "#000000")
pG5_bandsperp
#ggsave("Fibre Count_bandserp.png", pG5_bandsperp, width = 8, height = 6, units = "in", dpi=150, path = "Results")

#### GRAPH - Figure 4-11 ####
pbandscombinedG5_pending <- ggarrange(pG5_bandspara+ rremove("ylab") + rremove("xlab"),
                                    pG5_bandsperp+ rremove("ylab") + rremove("xlab"),
                                    common.legend = T, legend = "right",
                                    labels = c("B"),
                                    align = "h", widths = c(1,1),
                                    ncol = 2, nrow = 1,
                                    font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                    hjust=-1, vjust=1)
pbandscombinedG5_pending

# 12 garments
#### Combined Dataset for parallel contact area ####
G12_DatasetB1 <- Transfer_G12 %>% filter(grepl("\\b1\\b", band)) #The \\b in the regular expression ensures that "1" is matched as a whole word, and it won't select rows with "10" or "100."
G12_DatasetB2 <- Transfer_G12 %>% filter(grepl("\\b2\\b", band))
G12_DatasetB3 <- Transfer_G12 %>% filter(grepl("\\b3\\b", band))
G12_DatasetB4 <- Transfer_G12 %>% filter(grepl("\\b4\\b", band))
G12_DatasetB5 <- Transfer_G12 %>% filter(grepl("\\b5\\b", band))

#### Combined Dataset for perpendicular contact area ####
G12_DatasetB6 <- Transfer_G12 %>% filter(grepl("\\b6\\b", band))
G12_DatasetB7 <- Transfer_G12 %>% filter(grepl("\\b7\\b", band))
G12_DatasetB8 <- Transfer_G12 %>% filter(grepl("\\b8\\b", band))
G12_DatasetB9 <- Transfer_G12 %>% filter(grepl("\\b9\\b", band))
G12_DatasetB10 <- Transfer_G12 %>% filter(grepl("\\b10\\b", band))

#### Create a table with means per contact area ####
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
write.table(G12_MeanTotContactArea_export, file = "G12_Fibre per strip_export.csv", quote = F, sep = ",", row.names = F)

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

#### Graph ####
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
  ylim(-2,30)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  #annotate(geom = "text", x = 30.5, y = 38, label = "12 garments", color = "darkred")
pG12_bandspara
#ggsave("Fibre Count_bandspara.png", pG12_bandspara, width = 6, height = 4, units = "in", dpi=200, path = "Results")

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
  ylim(-2,30)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid")) +
  scale_colour_manual(values = c("#000000", "#a9a9a9", "#4363d8", "#42d4f4", "#3cb44b")) +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
  #annotate(geom = "text", x = 30.5, y = 38, label = "12 garments", color = "darkred")
pG12_bandsperp
#ggsave("Fibre Count_bandserp.png", pG12_bandsperp, width = 8, height = 6, units = "in", dpi=150, path = "Results")

#### GRAPH ####
pbandscombinedG12_pending <- ggarrange(pG12_bandspara+ rremove("ylab") + rremove("xlab"),
                                    pG12_bandsperp+ rremove("ylab") + rremove("xlab"),
                                    common.legend = T, legend = "right",
                                    labels = c("C"),
                                    align = "h", widths = c(1,1),
                                    ncol = 2, nrow = 1,
                                    font.label = list(size = 12, color = "black", family = "Arial", position = "top"), 
                                    hjust=-1, vjust=1)
pbandscombinedG12_pending

#### FINAL GRAPH ####
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
ggsave("Intravariability.png", pIntracombined, width =7, height = 8, units = "in", dpi=300,path = "Results")

#### Corr plot for intra-variability ####
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

# B vs B - Garment 5
cor_matrix <- cor(MeanTotBG5_forCorrPlot,use = "complete.obs")
head(cor_matrix)
write.matrix(cor_matrix,file="BvsB_G5.csv", sep = ",")
pairs.panels(MeanTotBG5_forCorrPlot[,1:10],
             stars = TRUE, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)

# B vs B - Garment 12
cor_matrix <- cor(MeanTotBG12_forCorrPlot,use = "complete.obs")
head(cor_matrix)
write.matrix(cor_matrix,file="BvsB_G12.csv", sep = ",")
pairs.panels(MeanTotBG12_forCorrPlot[,1:6],
             stars = TRUE, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)
# G  VS G  - Garment 1
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

# # G  VS G  - Garment 1 OFFSET
# MeanTotG1_forCorrPlot$G1paraoffset <- MeanTotG1_forCorrPlot$G1para+10
# MeanTotG1_forCorrPlot$G1perpoffset <- MeanTotG1_forCorrPlot$G1perp+10
# cor_matrix <- cor(MeanTotG1_forCorrPlot)
# pairs.panels(MeanTotG1_forCorrPlot[,1:4],
#              stars = TRUE, # If TRUE, adds significance level with stars
#              pch=20, # points shape
#              lm=T, # Plot the linear fit rather than the LOESS smoothed fits
#              method = "pearson", # correlation method
#              hist.col = "#6BAED6",
#              density = TRUE,  # show density plots
#              ellipses = F # show correlation ellipses
# )

# Comparison between load
mean_Transfer_G5 <- aggregate(value ~ wash, Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
specific_values <- c("W000", "W001","W002","W003","W004","W005","W006","W007","W009","W011","W013","W015")
MeanTotG5_subset <- subset(mean_Transfer_G5, wash %in% specific_values)

Transfer_G12$value <- as.numeric(Transfer_G12$value)
mean_Transfer_G12 <- aggregate(value ~ wash, Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
MeanTotG12_subset <- subset(mean_Transfer_G12, wash %in% specific_values)

Transfer_G1$value <- as.numeric(Transfer_G1$value)
mean_Transfer_G1 <- aggregate(value ~ wash, Transfer_G1, FUN = function(x) {round(mean(x), digits = 2)})

LoadComparison_forCorrPlot <- cbind(G1= mean_Transfer_G1$value,G5=MeanTotG5_subset$value, G12= MeanTotG12_subset$value)
cor_matrix <- cor(LoadComparison_forCorrPlot)
pairs.panels(LoadComparison_forCorrPlot[,1:3],
             stars = TRUE, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)

#----------------------------------------------------------------------------------#
####                  Repetitive Transfer - control garment                    #####
#----------------------------------------------------------------------------------#
# substract background
RT_Dataset$value <-  RT_Dataset$`After transfer` -  RT_Dataset$`Before transfer`

# to export
write.table(RT_Dataset, file = "Results/Transfer_control.csv", quote = F, sep = ",", row.names = F)

#Assign a Coder to each wash
RTB1_Dataset<- RT_Dataset %>% filter(grepl('G1_1', Sample))
RTB3_Dataset<- RT_Dataset %>% filter(grepl('G1_3', Sample))
RTB6_Dataset<- RT_Dataset %>% filter(grepl('G1_6', Sample))
RTB8_Dataset<- RT_Dataset %>% filter(grepl('G1_8', Sample))
write.table(RTB1_Dataset, file = "Results/RT_B1.csv", quote = F, sep = ",", row.names = F)
write.table(RTB3_Dataset, file = "Results/RT_B2.csv", quote = F, sep = ",", row.names = F)
write.table(RTB6_Dataset, file = "Results/RT_B3.csv", quote = F, sep = ",", row.names = F)
write.table(RTB8_Dataset, file = "Results/RT_B4.csv", quote = F, sep = ",", row.names = F)

RTB1_Dataset$Coder <- "RTB1"
RTB3_Dataset$Coder <- "RTB3"
RTB6_Dataset$Coder <- "RTB6"
RTB8_Dataset$Coder <- "RTB8"

# Create a new column in all dataframe
numS <- data.frame(seq(1,100, by = 1))
names(numS) <- c("Time")

# add the column "Time" from "numS" to all dataframe
RTB1_Dataset <- cbind(RTB1_Dataset,numS)
RTB3_Dataset <- cbind(RTB3_Dataset,numS)
RTB6_Dataset <- cbind(RTB6_Dataset,numS)
RTB8_Dataset <- cbind(RTB8_Dataset,numS)

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

Tot <-rbind(TransferRTB1,TransferRTB3,TransferRTB6,TransferRTB8)

Totpara <-rbind(TransferRTB1,TransferRTB3)
Totperp <-rbind(TransferRTB6,TransferRTB8)

df_means <- ddply(Tot, "group", summarise, mean_value = mean(value)) ; df_means
df_SD <- ddply(Tot, "group", summarise, mean_value = sd(value)) ; df_SD
df_meanspara <- ddply(Totpara, "group", summarise, mean_value = mean(value)) ; df_meanspara
df_meansperp <- ddply(Totperp, "group", summarise, mean_value = mean(value)) ; df_meansperp

#### GRAPH ####
pRTB1 <-ggplot(data = TransferRTB1, aes(Transfer, value)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Repetitive transfer", y="Number of Fibre")+ ylim(0,15)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Parallel strip 1")+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
mean <- round(mean(TransferRTB1$value),digits = 2)
pRTB1 <-pRTB1 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=1.5)
pRTB1

pRTB3 <-ggplot(data = TransferRTB3, aes(Transfer, value)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Repetitive transfer", y="Number of Fibre")+ ylim(0,15)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Parallel strip 2")+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
mean <- round(mean(TransferRTB3$value),digits = 2)
pRTB3 <-pRTB3 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=1.5)
pRTB3

pRTB6 <-ggplot(data = TransferRTB6, aes(Transfer, value)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Repetitive transfer", y="Number of Fibre")+ ylim(0,15)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Perpendicular strip 1")+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
mean <- round(mean(TransferRTB6$value),digits = 2)
pRTB6 <-pRTB6 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=1.5)
pRTB6

pRTB8 <-ggplot(data = TransferRTB8, aes(Transfer, value)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Repetitive transfer", y="Number of Fibre")+ ylim(0,15)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Perpendicular strip 2")+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
mean <- round(mean(TransferRTB8$value),digits = 2)
pRTB8 <-pRTB8 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=1.5)
pRTB8

#### Combined grap ####
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

#----------------------------------------------------------------------------------#
####                  Repetitive Transfer vs. Wash                    #####
#----------------------------------------------------------------------------------#
# combined graph
n=51
numS <- data.frame(setdiff(0:n, c()))
meanTotRT<- aggregate(value ~  Transfer, Tot, function(x) {round(mean(x), digits=2)})
meanTotRT <- meanTotRT[1:52,]
forplotTotRT <- data.frame(cbind(numS, value =meanTotRT$value))
names(forplotTotRT) <- c("Transfer", "value")
forplotTotRT$group <- "Repetitive transfer"

n=15
numS <- data.frame(setdiff(0:n, c(8,10,12,14)))
Transfer_G1 <- Transfer_G1[!is.na(Transfer_G1$value), ]
Transfer_G1$value <- as.numeric(Transfer_G1$value)
meanAtrG1 <- aggregate(value ~ wash, Transfer_G1, function(x) round(mean(x), digits = 2))
forplotTotG1 <- data.frame(cbind(numS, value =meanAtrG1$value))
names(forplotTotG1) <- c("Transfer", "value")
forplotTotG1$group <- c("1 garment")

n=51
numS <- data.frame(setdiff(0:n, c()))
Transfer_G5 <- Transfer_G5[!is.na(Transfer_G5$value), ]
Transfer_G5$value <- as.numeric(Transfer_G5$value)
meanAtrG5 <- aggregate(value ~ wash, Transfer_G5, function(x) round(mean(x), digits = 2))
forplotTotG5 <- data.frame(cbind(numS, value =meanAtrG5$value))
names(forplotTotG5) <- c("Transfer", "value")
forplotTotG5$group <- c("5 garments")

n=41
numS <- data.frame(setdiff(0:n, c(16,17,24,26,28,30,32,34,36,38,40)))
Transfer_G12 <- Transfer_G12[!is.na(Transfer_G12$value), ]
Transfer_G12$value <- as.numeric(Transfer_G12$value)
meanAtrG12 <- aggregate(value ~ wash, Transfer_G12, function(x) round(mean(x), digits = 2))
forplotTotG12 <- data.frame(cbind(numS, value =meanAtrG12$value))
names(forplotTotG12) <- c("Transfer", "value")
forplotTotG12$group <- c("12 garments")

Toplot <- rbind(forplotTotRT,forplotTotG1,forplotTotG5,forplotTotG12)
Toplot$Transfer <- as.numeric(Toplot$Transfer)
Toplot$value <- as.numeric(Toplot$value)
Toplot$group <- as.factor(Toplot$group)

# find the best fit
#fit polynomial regression models up to degree 5
fit1 <- lm(value~Transfer, data=forplotTotRT);fit1

# 1 garment
# fit1 <- lm(value~poly(Transfer,1,raw=TRUE), data=forplotTotG1)
# fit2 <- lm(value~poly(Transfer,2,raw=TRUE), data=forplotTotG1)
# fit3 <- lm(value~poly(Transfer,3,raw=TRUE), data=forplotTotG1)
# fit4 <- lm(value~poly(Transfer,4,raw=TRUE), data=forplotTotG1)
# fit5 <- lm(value~poly(Transfer,5,raw=TRUE), data=forplotTotG1)
# fit6 <- lm(value~poly(Transfer,6,raw=TRUE), data=forplotTotG1)
# fit7 <- lm(value~poly(Transfer,7,raw=TRUE), data=forplotTotG1)
# fit8 <- lm(value~poly(Transfer,8,raw=TRUE), data=forplotTotG1)
# fit9 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG1)
fit10 <- lm(value~poly(Transfer,10,raw=TRUE), data=forplotTotG1)

# a <- summary(fit1)$adj.r.squared;a
# b <- summary(fit2)$adj.r.squared;b
# c <- summary(fit3)$adj.r.squared;c
# d <- summary(fit4)$adj.r.squared;d
# e <- summary(fit5)$adj.r.squared;e
# f <- summary(fit6)$adj.r.squared;f
# g <- summary(fit7)$adj.r.squared;g
# h <- summary(fit8)$adj.r.squared;h
# i <- summary(fit9)$adj.r.squared;i
j <- summary(fit10)$adj.r.squared;j

# 5 garments
# fit1 <- lm(value~poly(Transfer,1,raw=TRUE), data=forplotTotG5)
# fit2 <- lm(value~poly(Transfer,2,raw=TRUE), data=forplotTotG5)
# fit3 <- lm(value~poly(Transfer,3,raw=TRUE), data=forplotTotG5)
# fit4 <- lm(value~poly(Transfer,4,raw=TRUE), data=forplotTotG5)
# fit5 <- lm(value~poly(Transfer,5,raw=TRUE), data=forplotTotG5)
# fit6 <- lm(value~poly(Transfer,6,raw=TRUE), data=forplotTotG5)
# fit7 <- lm(value~poly(Transfer,7,raw=TRUE), data=forplotTotG5)
# fit8 <- lm(value~poly(Transfer,8,raw=TRUE), data=forplotTotG5)
fit9 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG5)
# fit10 <- lm(value~poly(Transfer,10,raw=TRUE), data=forplotTotG5)

# a <- summary(fit1)$adj.r.squared;a
# b <- summary(fit2)$adj.r.squared;b
# c <- summary(fit3)$adj.r.squared;c
# d <- summary(fit4)$adj.r.squared;d
# e <- summary(fit5)$adj.r.squared;e
# f <- summary(fit6)$adj.r.squared;f
# g <- summary(fit7)$adj.r.squared;g
# h <- summary(fit8)$adj.r.squared;h
i <- summary(fit9)$adj.r.squared;i
# j <- summary(fit10)$adj.r.squared;j

#12 garments
# fit1 <- lm(value~poly(Transfer,1,raw=TRUE), data=forplotTotG12)
# fit2 <- lm(value~poly(Transfer,2,raw=TRUE), data=forplotTotG12)
# fit3 <- lm(value~poly(Transfer,3,raw=TRUE), data=forplotTotG12)
# fit4 <- lm(value~poly(Transfer,4,raw=TRUE), data=forplotTotG12)
# fit5 <- lm(value~poly(Transfer,5,raw=TRUE), data=forplotTotG12)
# fit6 <- lm(value~poly(Transfer,6,raw=TRUE), data=forplotTotG12)
fit7 <- lm(value~poly(Transfer,7,raw=TRUE), data=forplotTotG12)
# fit8 <- lm(value~poly(Transfer,8,raw=TRUE), data=forplotTotG12)
# fit9 <- lm(value~poly(Transfer,9,raw=TRUE), data=forplotTotG12)
# fit10 <- lm(value~poly(Transfer,10,raw=TRUE), data=forplotTotG12)

# a <- summary(fit1)$adj.r.squared;a
# b <- summary(fit2)$adj.r.squared;b
# c <- summary(fit3)$adj.r.squared;c
# d <- summary(fit4)$adj.r.squared;d
# e <- summary(fit5)$adj.r.squared;e
# f <- summary(fit6)$adj.r.squared;f
g <- summary(fit7)$adj.r.squared;g
# h <- summary(fit8)$adj.r.squared;h
# i <- summary(fit9)$adj.r.squared;i
# j <- summary(fit10)$adj.r.squared;j

# create a scatterplot
colors <- c("#469990",
            "darkred",
            "#000000",
            "grey")

plot(Toplot$Transfer, Toplot$value, col=colors[Toplot$group],
     pch=19,
     xlab='\nWash number',
     ylab='Average number of transferred fibres',
     xaxt='n',
     cex.axis = 1.2,
     cex.lab = 1.2)
axis(1, xaxp=c(0, 51, 51), las=1,cex.axis = 1.2) +theme_bw()
# add legend
legend(35, 24, legend=c("Control garment","1 garment", "5 garments", "12 garments"),
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

