#################################################################################################
#####                                FIBRE ANALYSIS GARMENT 1                               #####
#################################################################################################
# Assign a Coder to each wash number
W000_G1_Dataset$Coder <- "W000"
W001_G1_Dataset$Coder <- "W001"
W002_G1_Dataset$Coder <- "W002"
W003_G1_Dataset$Coder <- "W003"
W004_G1_Dataset$Coder <- "W004"
W005_G1_Dataset$Coder <- "W005"
W006_G1_Dataset$Coder <- "W006"
W007_G1_Dataset$Coder <- "W007"
W009_G1_Dataset$Coder <- "W009"
W011_G1_Dataset$Coder <- "W011"

W000_G5_Dataset$Coder <- "W000"
W001_G5_Dataset$Coder <- "W001"
W002_G5_Dataset$Coder <- "W002"
W003_G5_Dataset$Coder <- "W003"
W004_G5_Dataset$Coder <- "W004"
W005_G5_Dataset$Coder <- "W005"
W006_G5_Dataset$Coder <- "W006"
W007_G5_Dataset$Coder <- "W007"
W008_G5_Dataset$Coder <- "W008"
W009_G5_Dataset$Coder <- "W009"
W010_G5_Dataset$Coder <- "W010"
W011_G5_Dataset$Coder <- "W011"
W012_G5_Dataset$Coder <- "W012"
W013_G5_Dataset$Coder <- "W013"
W014_G5_Dataset$Coder <- "W014"
W015_G5_Dataset$Coder <- "W015"
W016_G5_Dataset$Coder <- "W016"
W017_G5_Dataset$Coder <- "W017"
W018_G5_Dataset$Coder <- "W018"
W019_G5_Dataset$Coder <- "W019"
W020_G5_Dataset$Coder <- "W020"
W021_G5_Dataset$Coder <- "W021"
W022_G5_Dataset$Coder <- "W022"
W023_G5_Dataset$Coder <- "W023"
W024_G5_Dataset$Coder <- "W024"
W025_G5_Dataset$Coder <- "W025"
W026_G5_Dataset$Coder <- "W026"
W027_G5_Dataset$Coder <- "W027"
W028_G5_Dataset$Coder <- "W028"
W029_G5_Dataset$Coder <- "W029"
W030_G5_Dataset$Coder <- "W030"
W031_G5_Dataset$Coder <- "W031"
W032_G5_Dataset$Coder <- "W032"
W033_G5_Dataset$Coder <- "W033"
W034_G5_Dataset$Coder <- "W034"
W035_G5_Dataset$Coder <- "W035"
W036_G5_Dataset$Coder <- "W036"
W037_G5_Dataset$Coder <- "W037"
W038_G5_Dataset$Coder <- "W038"
W039_G5_Dataset$Coder <- "W039"
W040_G5_Dataset$Coder <- "W040"
W041_G5_Dataset$Coder <- "W041"
W042_G5_Dataset$Coder <- "W042"
W043_G5_Dataset$Coder <- "W043"
W044_G5_Dataset$Coder <- "W044"
W045_G5_Dataset$Coder <- "W045"
W046_G5_Dataset$Coder <- "W046"
W047_G5_Dataset$Coder <- "W047"
W048_G5_Dataset$Coder <- "W048"
W049_G5_Dataset$Coder <- "W049"
W050_G5_Dataset$Coder <- "W050"
W051_G5_Dataset$Coder <- "W051"

W000_G12A_Dataset$Coder <- "W000"
W001_G12A_Dataset$Coder <- "W001"
W002_G12A_Dataset$Coder <- "W002"
W003_G12A_Dataset$Coder <- "W003"
W004_G12A_Dataset$Coder <- "W004"
W005_G12A_Dataset$Coder <- "W005"

W000_G12B_Dataset$Coder <- "W000"
W001_G12B_Dataset$Coder <- "W001"
W002_G12B_Dataset$Coder <- "W002"
W003_G12B_Dataset$Coder <- "W003"
W004_G12B_Dataset$Coder <- "W004"
W005_G12B_Dataset$Coder <- "W005"

W000_G12C_Dataset$Coder <- "W000"
W001_G12C_Dataset$Coder <- "W001"
W002_G12C_Dataset$Coder <- "W002"
W003_G12C_Dataset$Coder <- "W003"
W004_G12C_Dataset$Coder <- "W004"
W005_G12C_Dataset$Coder <- "W005"

#########################################################
#####                     Controls                  #####
#########################################################
# Negative controls
W000_G1_negative <- W000_G1_Dataset %>% filter(grepl('negative', Sample))
W001_G1_negative <- W001_G1_Dataset %>% filter(grepl('negative', Sample))
W002_G1_negative <- W002_G1_Dataset %>% filter(grepl('negative', Sample))
W003_G1_negative <- W003_G1_Dataset %>% filter(grepl('negative', Sample))
W004_G1_negative <- W004_G1_Dataset %>% filter(grepl('negative', Sample))
W005_G1_negative <- W005_G1_Dataset %>% filter(grepl('negative', Sample))
W006_G1_negative <- W006_G1_Dataset %>% filter(grepl('negative', Sample))
W007_G1_negative <- W007_G1_Dataset %>% filter(grepl('negative', Sample))
W009_G1_negative <- W009_G1_Dataset %>% filter(grepl('negative', Sample))
W011_G1_negative <- W011_G1_Dataset %>% filter(grepl('negative', Sample))

W000_G5_negative <- W000_G5_Dataset %>% filter(grepl('negative', Sample))
W001_G5_negative <- W001_G5_Dataset %>% filter(grepl('negative', Sample))
W002_G5_negative <- W002_G5_Dataset %>% filter(grepl('negative', Sample))
W003_G5_negative <- W003_G5_Dataset %>% filter(grepl('negative', Sample))
W004_G5_negative <- W004_G5_Dataset %>% filter(grepl('negative', Sample))
W005_G5_negative <- W005_G5_Dataset %>% filter(grepl('negative', Sample))
W006_G5_negative <- W006_G5_Dataset %>% filter(grepl('negative', Sample))
W007_G5_negative <- W007_G5_Dataset %>% filter(grepl('negative', Sample))
W008_G5_negative <- W008_G5_Dataset %>% filter(grepl('negative', Sample))
W009_G5_negative <- W009_G5_Dataset %>% filter(grepl('negative', Sample))
W010_G5_negative <- W010_G5_Dataset %>% filter(grepl('negative', Sample))
W011_G5_negative <- W011_G5_Dataset %>% filter(grepl('negative', Sample))
W012_G5_negative <- W012_G5_Dataset %>% filter(grepl('negative', Sample))
W013_G5_negative <- W013_G5_Dataset %>% filter(grepl('negative', Sample))
W014_G5_negative <- W014_G5_Dataset %>% filter(grepl('negative', Sample))
W015_G5_negative <- W015_G5_Dataset %>% filter(grepl('negative', Sample))
W016_G5_negative <- W016_G5_Dataset %>% filter(grepl('negative', Sample))
W017_G5_negative <- W017_G5_Dataset %>% filter(grepl('negative', Sample))
W018_G5_negative <- W018_G5_Dataset %>% filter(grepl('negative', Sample))
W019_G5_negative <- W019_G5_Dataset %>% filter(grepl('negative', Sample))
W020_G5_negative <- W020_G5_Dataset %>% filter(grepl('negative', Sample))
W021_G5_negative <- W021_G5_Dataset %>% filter(grepl('negative', Sample))
W022_G5_negative <- W022_G5_Dataset %>% filter(grepl('negative', Sample))
W023_G5_negative <- W023_G5_Dataset %>% filter(grepl('negative', Sample))
W024_G5_negative <- W024_G5_Dataset %>% filter(grepl('negative', Sample))
W025_G5_negative <- W025_G5_Dataset %>% filter(grepl('negative', Sample))
W026_G5_negative <- W026_G5_Dataset %>% filter(grepl('negative', Sample))
W027_G5_negative <- W027_G5_Dataset %>% filter(grepl('negative', Sample))
W028_G5_negative <- W028_G5_Dataset %>% filter(grepl('negative', Sample))
W029_G5_negative <- W029_G5_Dataset %>% filter(grepl('negative', Sample))
W030_G5_negative <- W030_G5_Dataset %>% filter(grepl('negative', Sample))
W031_G5_negative <- W031_G5_Dataset %>% filter(grepl('negative', Sample))
W032_G5_negative <- W032_G5_Dataset %>% filter(grepl('negative', Sample))
W033_G5_negative <- W033_G5_Dataset %>% filter(grepl('negative', Sample))
W034_G5_negative <- W034_G5_Dataset %>% filter(grepl('negative', Sample))
W035_G5_negative <- W035_G5_Dataset %>% filter(grepl('negative', Sample))
W036_G5_negative <- W036_G5_Dataset %>% filter(grepl('negative', Sample))
W037_G5_negative <- W037_G5_Dataset %>% filter(grepl('negative', Sample))
W038_G5_negative <- W038_G5_Dataset %>% filter(grepl('negative', Sample))
W039_G5_negative <- W039_G5_Dataset %>% filter(grepl('negative', Sample))
W040_G5_negative <- W040_G5_Dataset %>% filter(grepl('negative', Sample))
W041_G5_negative <- W041_G5_Dataset %>% filter(grepl('negative', Sample))
W042_G5_negative <- W042_G5_Dataset %>% filter(grepl('negative', Sample))
W043_G5_negative <- W043_G5_Dataset %>% filter(grepl('negative', Sample))
W044_G5_negative <- W044_G5_Dataset %>% filter(grepl('negative', Sample))
W045_G5_negative <- W045_G5_Dataset %>% filter(grepl('negative', Sample))
W046_G5_negative <- W046_G5_Dataset %>% filter(grepl('negative', Sample))
W047_G5_Dataset <- W047_G5_Dataset %>% filter(grepl('negative', Sample))
W048_G5_Dataset <- W048_G5_Dataset %>% filter(grepl('negative', Sample))
W049_G5_Dataset <- W049_G5_Dataset %>% filter(grepl('negative', Sample))
W050_G5_Dataset <- W050_G5_Dataset %>% filter(grepl('negative', Sample))
W051_G5_Dataset <- W051_G5_Dataset %>% filter(grepl('negative', Sample))

W000_G12A_negative <- W000_G12A_Dataset %>% filter(grepl('negative', Sample))
W001_G12A_negative <- W001_G12A_Dataset %>% filter(grepl('negative', Sample))
W002_G12A_negative <- W002_G12A_Dataset %>% filter(grepl('negative', Sample))
W003_G12A_negative <- W003_G12A_Dataset %>% filter(grepl('negative', Sample))
W004_G12A_negative <- W004_G12A_Dataset %>% filter(grepl('negative', Sample))
W005_G12A_negative <- W005_G12A_Dataset %>% filter(grepl('negative', Sample))
W006_G12A_negative <- W006_G12A_Dataset %>% filter(grepl('negative', Sample))

W000_G12B_negative <- W000_G12B_Dataset %>% filter(grepl('negative', Sample))
W001_G12B_negative <- W001_G12B_Dataset %>% filter(grepl('negative', Sample))
W002_G12B_negative <- W002_G12B_Dataset %>% filter(grepl('negative', Sample))
W003_G12B_negative <- W003_G12B_Dataset %>% filter(grepl('negative', Sample))
W004_G12B_negative <- W004_G12B_Dataset %>% filter(grepl('negative', Sample))
W005_G12B_negative <- W005_G12B_Dataset %>% filter(grepl('negative', Sample))
W006_G12B_negative <- W006_G12B_Dataset %>% filter(grepl('negative', Sample))

W000_G12C_negative <- W000_G12C_Dataset %>% filter(grepl('negative', Sample))
W001_G12C_negative <- W001_G12C_Dataset %>% filter(grepl('negative', Sample))
W002_G12C_negative <- W002_G12C_Dataset %>% filter(grepl('negative', Sample))
W003_G12C_negative <- W003_G12C_Dataset %>% filter(grepl('negative', Sample))
W004_G12C_negative <- W004_G12C_Dataset %>% filter(grepl('negative', Sample))
W005_G12C_negative <- W005_G12C_Dataset %>% filter(grepl('negative', Sample))
W006_G12C_negative <- W006_G12C_Dataset %>% filter(grepl('negative', Sample))

# Create a data frame "Negativecontrol" with all the negative controls 
Negativecontrol <- rbind(W000negative,W001negative, W002negative,W003negative,W004negative,W005negative,W006negative,W007negative,W009negative,W011negative)

# Calculate the number of background fibres
Negativecontrol$Diff <- Negativecontrol$`After transfer` - Negativecontrol$`Before transfer`
# Negativecontrol$Diff :  if value = 0, no difference. if value > 0, fibre there before transfer and not after.
# if value < 0, fibre there after transfer but not before (contamination)
Negativecontrol2 <- aggregate(Negativecontrol$Diff,list(Negativecontrol$Diff), FUN=length)
names(Negativecontrol2) <- c("value","appearance")
Negativecontrol2$percentnodiff <- Negativecontrol2$appearance/(sum(Negativecontrol2$appearance))*100
Negativecontrol2$percentnodiff<-round(Negativecontrol2$percent, digit =2)
Negativecontrol$Sample<- gsub("_negative","",Negativecontrol$Sample)

# plot the negative controls
pNegativecontrol <- ggplot(Negativecontrol, aes(x = Sample, y = Diff)) + 
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) + 
  geom_hline(yintercept=0,linetype="dashed", color = "black")+
  labs(y= "Difference between Before transfer and after transfer\n", x="\nWash")+
  ylim(-3,3)+
  scale_x_discrete(labels = every_n_labeler(0), breaks =every_n_labeler(5)) +
  theme_bw(base_size = 12)+
  theme(panel.grid.major.y = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.background = element_rect(color="grey", size=2),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
show(pNegativecontrol)

# Positive controls
W000positive <- W000_G1_Dataset %>% filter(grepl('positive', Sample))
W001positive <- W001_G1_Dataset %>% filter(grepl('positive', Sample))
W002positive <- W002_G1_Dataset %>% filter(grepl('positive', Sample))
W003positive <- W003_G1_Dataset %>% filter(grepl('positive', Sample))
W004positive <- W004_G1_Dataset %>% filter(grepl('positive', Sample))
W005positive <- W005_G1_Dataset %>% filter(grepl('positive', Sample))
W006positive <- W006_G1_Dataset %>% filter(grepl('positive', Sample))
W007positive <- W007_G1_Dataset %>% filter(grepl('positive', Sample))
W009positive <- W009_G1_Dataset %>% filter(grepl('positive', Sample))
W011positive <- W011_G1_Dataset %>% filter(grepl('positive', Sample))

Positivecontrol <- rbind(W000positive,W001positive, W002positive, W003positive, W004positive, W005positive,W006positive,W007positive,W009positive,W011positive)
Positivecontrol$Diff <- Positivecontrol$`After transfer` - Positivecontrol$`Before transfer`

# if value = 0, no difference. if value > 0, fibre there before and not after. if value < 0, fibre there not before but after
# Count to number of time the same year is repeated in the "AuthorListIFSMSExtended$Year" and save in a data.frame "Year" 
Positivecontrol2 <- aggregate(Positivecontrol$Diff,list(Positivecontrol$Diff), FUN=length)
names(Positivecontrol2) <- c("value","appearance")
Positivecontrol2$percentnodiff <- Positivecontrol2$appearance/(sum(Positivecontrol2$appearance))*100
Positivecontrol2$percentnodiff<-round(Positivecontrol2$percent, digit =2)

Positivecontrol$Sample<- gsub("_positive","",Positivecontrol$Sample)

ppositivecontrol <- ggplot(Positivecontrol, aes(x = Sample, y = Diff)) + 
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) + 
  geom_hline(yintercept=0,linetype="dashed", color = "black")+
  labs(y= "Difference between Before transfer and after transfer\n", x="\nWash")+
  ylim(-3,3)+
  scale_x_discrete(labels = every_n_labeler(0), breaks =every_n_labeler(5)) +
  theme_bw(base_size = 12)+
  theme(panel.grid.major.y = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.background = element_rect(color="grey", size=2),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
show(ppositivecontrol)

### GRAPH ###
# Combined results from positive and negative controls
pControls_pending <- ggarrange(ppositivecontrol+ rremove("ylab") + rremove("xlab"),
                               pNegativecontrol+ rremove("ylab") + rremove("xlab"),
                               labels = c("Positive control","Negative control"),
                               common.legend = TRUE, legend = "bottom",
                               align = "hv",
                               ncol = 1, nrow = 2,
                               font.label = list(size = 12, color = "black", family = "Arial", position = "top"),
                               hjust=-0.3,vjust=2)

pControls <- annotate_figure(pControls_pending, left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pControls
# ggsave("Controls.png", pControls, width = 6, height = 6, units = "in", dpi=150, path = "Results")

#### Creating dataset with only column Coder and After transfer after removing the positive and negative control ####
forFibreCount0<- W000_G1_Dataset[!(W000_G1_Dataset$Sample=="MP_W000_G1_positive_B" | W000_G1_Dataset$Sample=="MP_W000_G1_negative_B"),]
forFibreCount1<- W001_G1_Dataset[!(W001_G1_Dataset$Sample=="MP_W001_G1_positive_B" | W001_G1_Dataset$Sample=="MP_W001_G1_negative_B"),]
forFibreCount2<- W002_G1_Dataset[!(W002_G1_Dataset$Sample=="MP_W002_G1_positive_B" | W002_G1_Dataset$Sample=="MP_W002_G1_negative_B"),]
forFibreCount3<- W003_G1_Dataset[!(W003_G1_Dataset$Sample=="MP_W003_G1_positive_B" | W003_G1_Dataset$Sample=="MP_W003_G1_negative_B"),]
forFibreCount4<- W004_G1_Dataset[!(W004_G1_Dataset$Sample=="MP_W004_G1_positive_B" | W004_G1_Dataset$Sample=="MP_W004_G1_negative_B"),]
forFibreCount5<- W005_G1_Dataset[!(W005_G1_Dataset$Sample=="MP_W005_G1_positive_B" | W005_G1_Dataset$Sample=="MP_W005_G1_negative_B"),]
forFibreCount6<- W006_G1_Dataset[!(W006_G1_Dataset$Sample=="MP_W006_G1_positive_B" | W006_G1_Dataset$Sample=="MP_W006_G1_negative_B"),]
forFibreCount7<- W007_G1_Dataset[!(W007_G1_Dataset$Sample=="MP_W007_G1_positive_B" | W007_G1_Dataset$Sample=="MP_W007_G1_negative_B"),]
forFibreCount9<- W009_G1_Dataset[!(W009_G1_Dataset$Sample=="MP_W009_G1_positive_B" | W009_G1_Dataset$Sample=="MP_W009_G1_negative_B"),]
forFibreCount11<- W011_G1_Dataset[!(W011_G1_Dataset$Sample=="MP_W011_G1_positive_B" | W011_G1_Dataset$Sample=="MP_W011_G1_negative_B"),]

#########################################################
#####           ANALYSE OF THE BACKGROUND           #####
#########################################################
#### dplyr::select the column Before only in each dataframe ####
BackgroundW000 <- forFibreCount0 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW001 <- forFibreCount1 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW002 <- forFibreCount2 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW003 <- forFibreCount3 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW004 <- forFibreCount4 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW005 <- forFibreCount5 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW006 <- forFibreCount6 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW007 <- forFibreCount7 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW009 <- forFibreCount9 %>%
  dplyr::select(Coder,`Before transfer`)
BackgroundW011 <- forFibreCount11 %>%
  dplyr::select(Coder,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000, BackgroundW001, BackgroundW002,BackgroundW003,BackgroundW004,
                              BackgroundW005,BackgroundW006,BackgroundW007,BackgroundW009,BackgroundW011)
names(BackgroundFibreCount) <- c("group", "value")
# write.table(TransferFibreCount_G1, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

# Count the number of fibres found on the background images
BackgroundFibreCountfibres <- aggregate(BackgroundFibreCount$group,list(BackgroundFibreCount$value), FUN=length) # W011: NA

#########################################################
#####      NUMBER OF FIBRES FOLLOWING TRANSFER      #####
#########################################################
#### select the column Transfer only in each dataframe ####
TransferW000 <- forFibreCount0 %>%
  dplyr::select(Coder,`After transfer`)
TransferW001 <- forFibreCount1 %>%
  dplyr::select(Coder,`After transfer`)
TransferW002 <- forFibreCount2 %>%
  dplyr::select(Coder,`After transfer`)
TransferW003 <- forFibreCount3 %>%
  dplyr::select(Coder,`After transfer`)
TransferW004 <- forFibreCount4 %>%
  dplyr::select(Coder,`After transfer`)
TransferW005 <- forFibreCount5 %>%
  dplyr::select(Coder,`After transfer`)
TransferW006 <- forFibreCount6 %>%
  dplyr::select(Coder,`After transfer`)
TransferW007 <- forFibreCount7 %>%
  dplyr::select(Coder,`After transfer`)
TransferW009 <- forFibreCount9 %>%
  dplyr::select(Coder,`After transfer`)
TransferW011 <- forFibreCount11 %>%
  dplyr::select(Coder,`After transfer`)

#### Combine all the dataframe ####
TransferFibreCount_G1 <- rbind(TransferW000, TransferW001, TransferW002,TransferW003,TransferW004,TransferW005,
                            TransferW006,TransferW007,TransferW009,TransferW011)
names(TransferFibreCount_G1) <- c("group", "value")
# write.table(TransferFibreCount_G1, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

#### Bar plots of data by group - distribution ####
histogram(~ value | group,data=TransferFibreCount_G1,layout=c(3,3),
          xlab="Number of fibres")

#### Create a table with descriptive statistics ####
meanAtr_G1 <- aggregate(value ~  group, TransferFibreCount_G1, function(x) {round(mean(x), digits=2)})
SDAtr_G1 <- aggregate(value ~  group, TransferFibreCount_G1, function(x) {round(SD(x), digits=2)})
SD2Atr_G1 <- round(sqrt((SDAtr_G1$value^2)+(0.95^2)),digits=2)
medianAtr_G1 <- aggregate(value ~  group, TransferFibreCount_G1, median)
datatableAtr_G1 <- cbind(meanAtr_G1, medianAtr_G1$value, SDAtr_G1$value, SD2Atr_G1)
names(datatableAtr_G1) <- c("Wash number", "Average", "median", "SD", "SD2")
datatableAtr_G1$Forthesis <- paste(datatableAtr_G1$Average, datatableAtr_G1$SD, sep=" ± ")
#write.table(datatableAtr_G, file = "Stats_Atr red.csv", quote = F, sep = ",", row.names = F)

write.table(TransferFibreCount_G1, file = "Transfer_Fibre_Count.csv", quote = F, sep = ",", row.names = F)

#### GRAPH - FIGURE 4-8 ####
pAtr_G1 <- ggplot(TransferFibreCount_G1, aes(x=group, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,22)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G1)
ggsave("Fibre Count boxplot_ATr_G1.png", pAtr_G1, width = 6, height = 7, units = "in", dpi=150, path = "Results")
