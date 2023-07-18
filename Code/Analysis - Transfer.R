#########################################################
#####      NUMBER OF FIBRES FOLLOWING TRANSFER      #####
#########################################################
#### select the column After transfer only in each dataframe ####
# apply the select function to each data frame to extract the columns:
#"Coder, Coder2, and Before transfer"
# The result is a new list called Transfer that contains the modified data frames
# using the data_list created by running the previous code "Analysis - Control and Background"
Transfer <- lapply(data_list, function(data) {
  data %>%
    dplyr::select(Coder, Coder2, `After transfer`)
})

# convert the list Transfer back to a single data frame
Transfer <- bind_rows(Transfer)
names(Transfer) <- c("wash", "garment", "value")

# Create a dataframe per Garment
Transfer_G1 <- Transfer[Transfer$garment =='G1',]
Transfer_G5 <- Transfer[Transfer$garment =='G5',]
Transfer_G12 <- Transfer %>% filter(grepl('G12', garment))

Transfer_G12A <- Transfer[Transfer$garment =='G12A',]
Transfer_G12B <- Transfer[Transfer$garment =='G12B',]
Transfer_G12C <- Transfer[Transfer$garment =='G12C',]

# Create a table with descriptive statistics
meanAtr_G1 <- aggregate(value ~  wash, Transfer_G1, function(x) {round(mean(x), digits=2)})
SDAtr_G1 <- aggregate(value ~  wash, Transfer_G1, function(x) {round(SD(x), digits=2)})
medianAtr_G1 <- aggregate(value ~  wash, Transfer_G1, median)
datatableAtr_G1 <- cbind(meanAtr_G1, medianAtr_G1$value, SDAtr_G1$value)
names(datatableAtr_G1) <- c("Wash number", "Average", "median", "SD")
datatableAtr_G1$Forthesis <- paste(datatableAtr_G1$Average, datatableAtr_G1$SD, sep=" ± ")
write.table(datatableAtr_G1, file = "Results/Stats-Atr-G1.csv", sep = ",")

meanAtr_G5 <- aggregate(value ~  wash, Transfer_G5, function(x) {round(mean(x), digits=2)})
SDAtr_G5 <- aggregate(value ~  wash, Transfer_G5, function(x) {round(SD(x), digits=2)})
medianAtr_G5 <- aggregate(value ~  wash, Transfer_G5, median)
datatableAtr_G5 <- cbind(meanAtr_G5, medianAtr_G5$value, SDAtr_G5$value)
names(datatableAtr_G5) <- c("Wash number", "Average", "median", "SD")
datatableAtr_G5$Forthesis <- paste(datatableAtr_G5$Average, datatableAtr_G5$SD, sep=" ± ")
write.table(datatableAtr_G5, file = "Results/Stats-Atr-G5.csv", sep = ",")

meanAtr_G12 <- aggregate(value ~  wash, Transfer_G12, function(x) {round(mean(x), digits=2)})
SDAtr_G12 <- aggregate(value ~  wash, Transfer_G12, function(x) {round(SD(x), digits=2)})
medianAtr_G12 <- aggregate(value ~  wash, Transfer_G12, median)
datatableAtr_G12 <- cbind(meanAtr_G12, medianAtr_G12$value, SDAtr_G12$value)
names(datatableAtr_G12) <- c("Wash number", "Average", "median", "SD")
datatableAtr_G12$Forthesis <- paste(datatableAtr_G12$Average, datatableAtr_G12$SD, sep=" ± ")
write.table(datatableAtr_G12, file = "Results/Stats-Atr-G12.csv", sep = ",")

#### GRAPH - Figure XXX ####
pAtr_G1 <- ggplot(Transfer_G1, aes(x=wash, y=value)) +
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

pAtr_G5 <- ggplot(Transfer_G5, aes(x=wash, y=value)) +
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
ggsave("Fibre Count boxplot_ATr_G5.png", pAtr_G5, width = 6, height = 7, units = "in", dpi=150, path = "Results")

pAtr_G12 <- ggplot(Transfer_G12, aes(x=wash, y=value)) +
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
ggsave("Fibre Count boxplot_ATr_G12.png", pAtr_G12, width = 6, height = 7, units = "in", dpi=150, path = "Results")

#### Combined Graph ####
Transfer_G1$Coder <-"1 garment"
Transfer_G5 <- Transfer_G5[is.element(Transfer_G5$wash, c('W000','W001','W002','W003','W004','W005','W006','W007','W008','W009','W010')),]
Transfer_G5$Coder <-"5 garments"
Transfer_G12$Coder <-"12 garments"
Transfer_Total <- rbind(Transfer_G1, Transfer_G5, Transfer_G12)

pAtr_Total<- ggplot(Transfer_Total, aes(x=wash, y=value,fill=Coder)) +
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
