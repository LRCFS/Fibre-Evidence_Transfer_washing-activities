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
  ylim(-5,40)+
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
  ylim(-5,40)+
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

pAtr_G12 <- ggplot(Transfer_G12, aes(x=as.factor(wash), y=as.numeric(value))) +
  geom_boxplot() +
  scale_x_discrete(labels = every_n_labeler(2))+
  ylim(-5,40)+
  labs(x="\nWash number", y="Number of Fibre\n") +
  theme_classic(base_family = "Arial", base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
show(pAtr_G12)
ggsave("Fibre Count boxplot_ATr_G12.png", pAtr_G12, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#### Combined Graph ####
pAtr_combined_pending <- ggarrange(pAtr_G5+ rremove("ylab") + rremove("xlab"), vjust = 0.8, hjust = 0.8,                                                # First row with scatter plot
                                   ggarrange(pAtr_G1+ rremove("ylab") + rremove("xlab"),
                                             pAtr_G12+ rremove("ylab") + rremove("xlab"),
                                             ncol = 2, labels = c("B", "C"),vjust = 0.8, hjust = 0.8), # Second row with box and dot plots
                                   nrow = 2,
                                   labels = "A")# Labels of the scatter plot
                                   
ppAtr_combined <- annotate_figure(pAtr_combined_pending, left = textGrob("Number of fibres\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                  bottom = textGrob("\nWash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));ppAtr_combined

ggsave("ppAtr_combined.png", ppAtr_combined, width = 6, height = 6, units = "in", dpi=300, path = "Results")

