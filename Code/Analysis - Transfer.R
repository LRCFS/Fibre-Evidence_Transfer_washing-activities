#########################################################
#####      NUMBER OF FIBRES FOLLOWING TRANSFER      #####
#########################################################
# select the column Transfer only in each dataframe
# Dataset with 1 garments
# create a list called data_list that contains all the individual data frames 
data_list_G1 <- list(forFibreCount0_G1,forFibreCount1_G1,forFibreCount2_G1,forFibreCount3_G1,forFibreCount4_G1,
                  forFibreCount5_G1,forFibreCount6_G1,forFibreCount7_G1,forFibreCount9_G1,forFibreCount11_G1)
# apply the select function to each data frame to extract the columns:
#"Coder, Coder2, and After transfer"
# The result is a new list called TransferW_G1 that contains the modified data frames
TransferW_G1 <- lapply(data_list_G1, function(data) {
  data %>%
    dplyr::select(Coder, Coder2, `After transfer`)
})
# convert the list TransferW_G1 back to a single data frame
combined_data <- bind_rows(TransferW_G1)

# Dataset with 5 garments
# Create a vector of numbers from 0 to 51
count <- 0:51
# Use lapply to iterate over the count vector and generate the TransferW_G5 list
TransferW_G5 <- lapply(count, function(i) {
  df <- get(paste0("forFibreCount", i, "_G5"))
  df %>% dplyr::select(Coder, Coder2, `After transfer`)
})

# Assign each element of the TransferW_G5 list to separate data frames
for (i in count) {
  assign(paste0("TransferW", sprintf("%03d", i), "_G5"), TransferW_G5[[i+1]], envir = .GlobalEnv)
}


TransferW000_G5 <- forFibreCount0_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW001_G5 <- forFibreCount1_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW002_G5 <- forFibreCount2_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW003_G5 <- forFibreCount3_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW004_G5 <- forFibreCount4_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW005_G5 <- forFibreCount5_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW006_G5 <- forFibreCount6_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW007_G5 <- forFibreCount7_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW008_G5 <- forFibreCount8_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW009_G5 <- forFibreCount9_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW010_G5 <- forFibreCount10_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW011_G5 <- forFibreCount11_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW012_G5 <- forFibreCount12_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW013_G5 <- forFibreCount13_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW014_G5 <- forFibreCount14_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW015_G5 <- forFibreCount15_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW016_G5 <- forFibreCount16_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW017_G5 <- forFibreCount17_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW018_G5 <- forFibreCount18_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW019_G5 <- forFibreCount19_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW020_G5 <- forFibreCount20_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW021_G5 <- forFibreCount21_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW022_G5 <- forFibreCount22_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW023_G5 <- forFibreCount23_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW024_G5 <- forFibreCount24_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW025_G5 <- forFibreCount25_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW026_G5 <- forFibreCount26_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW027_G5 <- forFibreCount27_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW028_G5 <- forFibreCount28_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW029_G5 <- forFibreCount29_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW030_G5 <- forFibreCount30_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW031_G5 <- forFibreCount31_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW032_G5 <- forFibreCount32_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW033_G5 <- forFibreCount33_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW034_G5 <- forFibreCount34_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW035_G5 <- forFibreCount35_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW036_G5 <- forFibreCount36_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW037_G5 <- forFibreCount37_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW038_G5 <- forFibreCount38_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW039_G5 <- forFibreCount39_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW040_G5 <- forFibreCount40_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW041_G5 <- forFibreCount41_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW042_G5 <- forFibreCount42_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW043_G5 <- forFibreCount43_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW044_G5 <- forFibreCount44_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW045_G5 <- forFibreCount45_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW046_G5 <- forFibreCount46_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW047_G5 <- forFibreCount47_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW048_G5 <- forFibreCount48_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW049_G5 <- forFibreCount49_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW050_G5 <- forFibreCount50_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW051_G5 <- forFibreCount51_G5 %>%
  dplyr::select(Coder,Coder2,`After transfer`)

# Dataset with 12 garments
TransferW000_G12A <- forFibreCount0_G12A %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW001_G12A <- forFibreCount1_G12A %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW002_G12A <- forFibreCount2_G12A %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW003_G12A <- forFibreCount3_G12A %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW004_G12A <- forFibreCount4_G12A %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW005_G12A <- forFibreCount5_G12A %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW000_G12B <- forFibreCount0_G12B %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW001_G12B <- forFibreCount1_G12B %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW002_G12B <- forFibreCount2_G12B %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW003_G12B <- forFibreCount3_G12B %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW004_G12B <- forFibreCount4_G12B %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW005_G12B <- forFibreCount5_G12B %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW000_G12C <- forFibreCount0_G12C %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW001_G12C <- forFibreCount1_G12C %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW002_G12C <- forFibreCount2_G12C %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW003_G12C <- forFibreCount3_G12C %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW004_G12C <- forFibreCount4_G12C %>%
  dplyr::select(Coder,Coder2,`After transfer`)
TransferW005_G12C <- forFibreCount5_G12C %>%
  dplyr::select(Coder,Coder2,`After transfer`)

# Combine all the dataframe
TransferFibreCount_G1 <- rbind(TransferW000_G1, TransferW001_G1, TransferW002_G1,TransferW003_G1,TransferW004_G1,
                             TransferW005_G1,TransferW006_G1,TransferW007_G1,TransferW009_G1,TransferW011_G1)
                             
TransferFibreCount_G5 <- rbind(TransferW000_G5, TransferW001_G5, TransferW002_G5, TransferW003_G5, TransferW004_G5, TransferW005_G5, TransferW006_G5,
                             TransferW007_G5, TransferW008_G5, TransferW009_G5, TransferW010_G5, TransferW011_G5, TransferW012_G5,TransferW013_G5,
                             TransferW014_G5, TransferW015_G5, TransferW016_G5,TransferW017_G5,TransferW018_G5,TransferW019_G5,TransferW020_G5,
                             TransferW021_G5,TransferW022_G5,TransferW023_G5,TransferW024_G5,TransferW025_G5, TransferW026_G5, TransferW027_G5,
                             TransferW028_G5, TransferW029_G5, TransferW030_G5,TransferW031_G5,TransferW032_G5,TransferW033_G5,TransferW034_G5,
                             TransferW035_G5,TransferW036_G5,TransferW037_G5,TransferW038_G5,TransferW039_G5,TransferW040_G5,
                             TransferW041_G5,TransferW042_G5,TransferW043_G5,TransferW044_G5,TransferW045_G5,TransferW046_G5,TransferW047_G5,
                             TransferW048_G5,TransferW049_G5,TransferW050_G5,TransferW051_G5)

TransferFibreCount_G12 <- rbind(TransferW000_G12A, TransferW001_G12A, TransferW002_G12A,TransferW003_G12A,TransferW004_G12A,
                             TransferW005_G12A,
                             TransferW000_G12B, TransferW001_G12B, TransferW002_G12B,TransferW003_G12B,TransferW004_G12B,
                             TransferW005_G12B,
                             TransferW000_G12C, TransferW001_G12C, TransferW002_G12C,TransferW003_G12C,TransferW004_G12C,
                             TransferW005_G12C)

names(TransferFibreCount_G1) <- c("wash", "garment", "value")
names(TransferFibreCount_G5) <- c("wash", "garment", "value")
names(TransferFibreCount_G12) <- c("wash", "garment", "value")
# write.table(TransferFibreCount_G1, file = "Fibre Count-Transfer-G1.csv", quote = F, sep = ",", row.names = F)
# write.table(TransferFibreCount_G5, file = "Fibre Count-Transfer-G5.csv", quote = F, sep = ",", row.names = F)
# write.table(TransferFibreCount_G12, file = "Fibre Count-Transfer-G12.csv", quote = F, sep = ",", row.names = F)

# Bar plots of data by group - distribution
histogram(~ value | wash,data=TransferFibreCount_G1,layout=c(5,4),
          xlab="Number of fibres")
histogram(~ value | wash,data=TransferFibreCount_G5,layout=c(5,11),
          xlab="Number of fibres")
histogram(~ value | wash,data=TransferFibreCount_G12,layout=c(5,3),
          xlab="Number of fibres")

# Create a table with descriptive statistics
meanAtr_G1 <- aggregate(value ~  wash, TransferFibreCount_G1, function(x) {round(mean(x), digits=2)})
SDAtr_G1 <- aggregate(value ~  wash, TransferFibreCount_G1, function(x) {round(SD(x), digits=2)})
medianAtr_G1 <- aggregate(value ~  wash, TransferFibreCount_G1, median)
datatableAtr_G1 <- cbind(meanAtr_G1, medianAtr_G1$value, SDAtr_G1$value)
names(datatableAtr_G1) <- c("Wash number", "Average", "median", "SD")
datatableAtr_G1$Forthesis <- paste(datatableAtr_G1$Average, datatableAtr_G1$SD, sep=" ± ")
write.table(datatableAtr_G1, file = "Results/Stats-Atr-G1.csv", sep = ",")

meanAtr_G5 <- aggregate(value ~  wash, TransferFibreCount_G5, function(x) {round(mean(x), digits=2)})
SDAtr_G5 <- aggregate(value ~  wash, TransferFibreCount_G5, function(x) {round(SD(x), digits=2)})
medianAtr_G5 <- aggregate(value ~  wash, TransferFibreCount_G5, median)
datatableAtr_G5 <- cbind(meanAtr_G5, medianAtr_G5$value, SDAtr_G5$value)
names(datatableAtr_G5) <- c("Wash number", "Average", "median", "SD")
datatableAtr_G5$Forthesis <- paste(datatableAtr_G5$Average, datatableAtr_G5$SD, sep=" ± ")
write.table(datatableAtr_G5, file = "Results/Stats-Atr-G5.csv", sep = ",")

meanAtr_G12 <- aggregate(value ~  wash, TransferFibreCount_G12, function(x) {round(mean(x), digits=2)})
SDAtr_G12 <- aggregate(value ~  wash, TransferFibreCount_G12, function(x) {round(SD(x), digits=2)})
medianAtr_G12 <- aggregate(value ~  wash, TransferFibreCount_G12, median)
datatableAtr_G12 <- cbind(meanAtr_G12, medianAtr_G12$value, SDAtr_G12$value)
names(datatableAtr_G12) <- c("Wash number", "Average", "median", "SD")
datatableAtr_G12$Forthesis <- paste(datatableAtr_G12$Average, datatableAtr_G12$SD, sep=" ± ")
write.table(datatableAtr_G12, file = "Results/Stats-Atr-G12.csv", sep = ",")

#### GRAPH - Figure XXX ####
pAtr_G1 <- ggplot(TransferFibreCount_G1, aes(x=wash, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G1)
ggsave("Fibre Count boxplot_ATr_G1.png", pAtr_G1, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pAtr_G5 <- ggplot(TransferFibreCount_G5, aes(x=wash, y=value)) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(5))+
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G5)
ggsave("Fibre Count boxplot_ATr_G1.png", pAtr_G5, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pAtr_G12 <- ggplot(TransferFibreCount_G12, aes(x=wash, y=value)) +
  geom_boxplot() +
  stat_summary(fun = mean, colour="darkred",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="darkred", aes(group=1),
               geom="line", lwd=1, lty=1) +
  ylim(0,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_G12)
ggsave("Fibre Count boxplot_ATr_G1.png", pAtr_G12, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#### Combined Graph ####
TransferFibreCount_G1$Coder <-"1 garment"
TransferFibreCount_G5 <- TransferFibreCount_G5[is.element(TransferFibreCount_G5$wash, c('W000','W001','W002','W003','W004','W005','W006','W007','W008','W009','W010')),]
TransferFibreCount_G5$Coder <-"5 garments"
TransferFibreCount_G12$Coder <-"12 garments"
TransferFibreCount_Total <- rbind(TransferFibreCount_G1, TransferFibreCount_G5, TransferFibreCount_G12)

pAtr_Total<- ggplot(TransferFibreCount_Total, aes(x=wash, y=value,fill=Coder)) +
  geom_boxplot() +
  facet_wrap(~Coder)+
  stat_summary(fun = mean, colour="black",
               geom="point",position=position_dodge(width=0.75)) +
  stat_summary(fun = mean, colour="black", aes(group=1),
               geom="line", lwd=1, lty=1) +
  labs(x="\nWash number", y="Number of Fibre\n") +
  scale_fill_brewer(palette = "Reds")+
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
show(pAtr_Total)
ggsave("Fibre Count boxplot_ATr_Totald.png", pAtr_Total, width = 6, height = 7, units = "in", dpi=150, path = "Results")

# Combined results
pAtr_combined_pending <- ggarrange(pAtr_G5+ rremove("ylab") + rremove("xlab"), vjust = 0.8, hjust = 0.8,                                                # First row with scatter plot
                                   ggarrange(pAtr_G1+ rremove("ylab") + rremove("xlab"),
                                             pAtr_G12+ rremove("ylab") + rremove("xlab"),
                                             ncol = 2, labels = c("B", "C"),vjust = 0.8, hjust = 0.8), # Second row with box and dot plots
                                   nrow = 2,
                                   labels = "A")# Labels of the scatter plot
                                   
ppAtr_combined <- annotate_figure(pAtr_combined_pending, left = textGrob("Number of fibres\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                  bottom = textGrob("\nWash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));ppAtr_combined

ggsave("ppAtr_combined.png", ppAtr_combined, width = 6, height = 6, units = "in", dpi=300, path = "Results")