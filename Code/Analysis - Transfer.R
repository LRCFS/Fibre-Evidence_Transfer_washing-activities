#########################################################
#####      NUMBER OF FIBRES FOLLOWING TRANSFER      #####
#########################################################
# Split the column based on "_"
Transfer_G1 <- separate(G1_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")
Transfer_G5 <- separate(G5_Dataset, Sample, into = c("wash","garment", "band", "orientation", "condition"), sep = "_")
Transfer_G12 <- separate(G12_Dataset, Sample, into = c("date", "exp", "wash","garment", "band", "sample"), sep = "_")

# substract background
Transfer_G1$value <-  Transfer_G1$`After transfer` -  Transfer_G1$`Before transfer`
Transfer_G5$value <-  Transfer_G5$`After transfer` -  Transfer_G5$`Before transfer`
Transfer_G12$value <-  Transfer_G12$`After transfer` -  Transfer_G12$`Before transfer`

# Select important column
Transfer_G1 <-  Transfer_G1 %>% dplyr::select(wash, garment,value)
Transfer_G5 <-  Transfer_G5 %>% dplyr::select(wash, garment,value)
Transfer_G12 <-  Transfer_G12 %>% dplyr::select(wash, garment,value)

# Create a list of dataframes
dataframes <- list(Transfer_G1, Transfer_G5, Transfer_G12)

# Create a function to compute statistics and write to CSV
compute_and_write_stats <- function(df, filename) {
  mean_df <- aggregate(value ~ wash, df, function(x) round(mean(x), digits = 2))
  sd_df <- aggregate(value ~ wash, df, function(x) round(sd(x), digits = 2))
  median_df <- aggregate(value ~ wash, df, median)
  
  datatable <- data.frame(
    "Wash number" = mean_df$wash,
    "Average" = mean_df$value,
    "Median" = median_df$value,
    "SD" = sd_df$value
  )
  
  datatable$Forthesis <- paste(datatable$Average, datatable$SD, sep = " ± ")
  
  write.table(datatable, file = filename, sep = ",", fileEncoding = "UTF-8")
}

# Loop through the dataframes and compute/write statistics
for (i in 1:length(dataframes)) {
  compute_and_write_stats(dataframes[[i]], paste("Results/Stats-Atr-G", c(1, 5, 12)[i], ".csv", sep = ","))
}

#### GRAPH - Figure XXX ####
Transfer_G1[1,] <- c("W008", "GA4", NA)
Transfer_G1[2,] <- c("W010", "GA4", NA)
Transfer_G1[3,] <- c("W012", "GA4", NA)
Transfer_G1[4,] <- c("W014", "GA4", NA)

pAtr_G1 <- ggplot(Transfer_G1, aes(x=as.factor(wash), y=as.numeric(value))) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(2))+
  ylim(-2,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G1)
ggsave("Fibre Count boxplot_ATr_G1.png", pAtr_G1, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pAtr_G5 <- ggplot(Transfer_G5, aes(x=wash, y=value)) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(2))+
  ylim(-2,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G5)
ggsave("Fibre Count boxplot_ATr_G5.png", pAtr_G5, width = 6, height = 7, units = "in", dpi=150, path = "Results")

Transfer_G12[1,] <- c("W016", "GA4", NA)
Transfer_G12[2,] <- c("W017", "GA4", NA)
Transfer_G12[3,] <- c("W024", "GA4", NA)
Transfer_G12[4,] <- c("W026", "GA4", NA)
Transfer_G12[5,] <- c("W028", "GA4", NA)
Transfer_G12[6,] <- c("W030", "GA4", NA)

pAtr_G12 <- ggplot(Transfer_G12, aes(x=as.factor(wash), y=as.numeric(value))) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(2))+
  ylim(-2,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G12)
ggsave("Fibre Count boxplot_ATr_G12.png", pAtr_G12, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#### Combined Graph ####

pAtr_combined_pending <- ggarrange(pAtr_G1+ rremove("ylab") + rremove("xlab"),
                                      pAtr_G5+ rremove("ylab") + rremove("xlab"),
                                      pAtr_G12+ rremove("ylab") + rremove("xlab"),
                                      nrow = 3, labels = c("A", "B", "C"),
                                      vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)
                                   
ppAtr_combined <- annotate_figure(pAtr_combined_pending, left = textGrob("Number of fibres\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                  bottom = textGrob("\nWash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));ppAtr_combined

ggsave("ppAtr_combined.png", ppAtr_combined, width = 7, height = 7, units = "in", dpi=300, path = "Results")

#####################################################################
#####      NUMBER OF FIBRES FOLLOWING TRANSFER PER GARMENT      #####
####################################################################
### 5 garments washed together
#### Split Transfer_G5 per garment ####
Garment1_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G1', garment))
Garment2_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G2', garment))
Garment3_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G3', garment))
Garment4_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G4', garment))
Garment5_Transfer_G5 <- Transfer_G5 %>% filter(grepl('G5', garment))

#### Create a table with means per garment ####
meanGarment1_Transfer_G5 <- aggregate(value~  wash, Garment1_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment1_Transfer_G5$Coder2 <- "1"
meanGarment2_Transfer_G5 <- aggregate(value~  wash, Garment2_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G5$Coder2 <- "2"
meanGarment3_Transfer_G5 <- aggregate(value~  wash, Garment3_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G5$Coder2 <- "3"
meanGarment4_Transfer_G5 <- aggregate(value~  wash, Garment4_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment4_Transfer_G5$Coder2 <- "4"
meanGarment5_Transfer_G5 <- aggregate(value~  wash, Garment5_Transfer_G5, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment5_Transfer_G5$Coder2 <- "5"
MeanTotG5 <- rbind(meanGarment1_Transfer_G5,meanGarment2_Transfer_G5,meanGarment3_Transfer_G5,meanGarment4_Transfer_G5,meanGarment5_Transfer_G5)

#### GRAPH - FIGURE XXX ####
pGarment_G5 <-ggplot(data = MeanTotG5, aes(x =wash, y = value, color=Coder2, group=Coder2)) +
  geom_line(aes(linetype=Coder2), size=0.7)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000","#000000","#3182BD","#3182BD","#3cb44b"))+
  scale_linetype_manual(values=c("solid","dashed", "solid","dashed","solid"))+
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
pGarment_G5
ggsave("Fibre Count_Garment_G5.png", pGarment_G5, width = 6, height = 7, units = "in", dpi=150, path = "Results")

### 12 garments washed together
#### Split Transfer_G12 per garment ####
Garment1_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G4A', garment))
Garment2_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G4B', garment))
Garment3_Transfer_G12 <- Transfer_G12 %>% filter(grepl('G4C', garment))

#### Create a table with means per garment ####
# remove NA value
Garment1_Transfer_G12 <- Garment1_Transfer_G12[!is.na(Garment1_Transfer_G12$value), ]
Garment2_Transfer_G12 <- Garment2_Transfer_G12[!is.na(Garment2_Transfer_G12$value), ]
Garment3_Transfer_G12 <- Garment3_Transfer_G12[!is.na(Garment3_Transfer_G12$value), ]
# transform value into numeric
Garment1_Transfer_G12$value <- as.numeric(Garment1_Transfer_G12$value)
Garment2_Transfer_G12$value <- as.numeric(Garment2_Transfer_G12$value)
Garment3_Transfer_G12$value <- as.numeric(Garment3_Transfer_G12$value)

meanGarment1_Transfer_G12 <- aggregate(value ~ wash, Garment1_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment1_Transfer_G12$Coder2 <- "1"
meanGarment2_Transfer_G12 <- aggregate(value~  wash, Garment2_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment2_Transfer_G12$Coder2 <- "2"
meanGarment3_Transfer_G12 <- aggregate(value~  wash, Garment3_Transfer_G12, FUN = function(x) {round(mean(x), digits = 2)})
meanGarment3_Transfer_G12$Coder2 <- "3"
MeanTotG12 <- rbind(meanGarment1_Transfer_G12,meanGarment2_Transfer_G12,meanGarment3_Transfer_G12)

#### GRAPH - FIGURE XXX ####
pGarment_G12 <-ggplot(data = MeanTotG12, aes(x =wash, y = value, color=Coder2, group=Coder2)) +
  geom_line(aes(linetype=Coder2), size=0.7)+
  scale_x_discrete(labels = every_n_labeler(5)) +
  ylim(0,30)+
  labs(x="\nWash number", y="Number of Fibres\n")+
  scale_colour_manual(values=c("#000000","#e6194B","#dcbeff"))+
  scale_linetype_manual(values=c("solid","solid","solid"))+
  theme_bw(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
pGarment_G12
ggsave("Fibre Count_Garment_G12.png", pGarment_G12, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#### Combined Graph ####
pGarment_combined_pending <- ggarrange(pGarment_G5+ rremove("ylab") + rremove("xlab"),
                                       pGarment_G12+ rremove("ylab") + rremove("xlab"),
                                       nrow = 2, labels = c("A", "B"),
                                       common.legend = F, legend = "right",
                                       vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)

pGarment_combined <- annotate_figure(pGarment_combined_pending,
                                  left = textGrob("Number of fibres\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                  bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)),
                                  right = text_grob(bquote("Garment number"), rot = 90))
pGarment_combined
ggsave("pGarment_combined.png", pGarment_combined, width = 7, height = 7, units = "in", dpi=300, path = "Results")

