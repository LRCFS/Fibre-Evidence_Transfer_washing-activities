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
Transfer_G12[6,] <- c("W032", "GA4", NA)

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

#############################################################
#####       Repetitive Transfer - control garment       #####
#############################################################
# substract background
RT_Dataset$value <-  RT_Dataset$`After transfer` -  RT_Dataset$`Before transfer`

#Assign a Coder to each wash
RTB1_Dataset<- RT_Dataset %>% filter(grepl('G1_1', Sample))
RTB3_Dataset<- RT_Dataset %>% filter(grepl('G1_3', Sample))
RTB6_Dataset<- RT_Dataset %>% filter(grepl('G1_6', Sample))
RTB8_Dataset<- RT_Dataset %>% filter(grepl('G1_8', Sample))

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
        legend.position = "bottom",
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
        legend.position = "bottom",
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
        legend.position = "bottom",
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
        legend.position = "bottom",
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

#############################################################
#####            Repetitive Transfer vs. Wash           #####
#############################################################
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

n=33
numS <- data.frame(setdiff(0:n, c(16,17,24,26,28,30,32)))
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
fit1 <- lm(value~Transfer, data=forplotTotRT)

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
     xlab='\nExperiment number',
     ylab='Average number of Fibres',
     xaxt='n',
     cex.axis = 1.2,
     cex.lab = 1.2)
axis(1, xaxp=c(0, 51, 51), las=1,cex.axis = 1.2) +theme_bw()
# add legend
legend(35, 24, legend=c("1 garment", "12 garments", "5 garments", "Control garment"),
       col=c("#469990", "darkred","#000000","grey" ), lty=1:2, cex=1.2)

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

x_range <- seq(1, 33, length = 51)  # Adjust the range as needed
predictions <- predict(fitG12, data.frame(Transfer = x_range))
lines(x_range, predictions, col = 'darkred')
