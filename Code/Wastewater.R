# This code is to upload the data related to the number of fibres released in the wastewater and the volume of water used each wash

# The .csv file to upload for the volume of wastewater will need to have the following columns:
# Washnumber; Wash; Rinse; Total;
# The data are available in Appendix 9 for the red donor
# The data are available in Appendix 18 for the yellow donor

# The .csv file to upload for the fibres released in the wastewater will need to have the following columns:
# Filter; Date; Experiment; Filtration; mass1; mass2; mass3; mass4; mass5;
# The data are available in Appendix 10 for the red donor
# The data are available in Appendix 19 for the yellow donor

#############################################################
#####                     to load                       #####
#############################################################

Wastewaterfibres_G1 <- read.csv('./Data_filtration_fibre weight_G1.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres_G1$Date<-as.factor(Wastewaterfibres_G1$Date)
Wastewatervolume_G1 <- read.csv('./Data_filtration_water volume_G1.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

Wastewaterfibres_G5 <- read.csv('./Data_filtration_fibre weight_G5.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres_G5$Date<-as.factor(Wastewaterfibres_G5$Date)
Wastewatervolume_G5 <- read.csv('./Data_filtration_water volume_G5.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

Wastewaterfibres_G12 <- read.csv('./Data_filtration_fibre weight_G12.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
Wastewaterfibres_G12$Date<-as.factor(Wastewaterfibres_G12$Date)
Wastewatervolume_G12 <- read.csv('./Data_filtration_water volume_G12.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

###### Wastewater volume ######
# 1) Uncertainty related to the graduation of the barrel (calibration)
# To calibrate the barrel, a 500 ml burette was filled up to the 500 ml graduation and pour into the barrel.
# At the first calibration line on the barrel, the uncertainty on the position of the line corresponds to the uncertainty...
#... on the measured volume (standard uncertainty of reading): Uread = (1 graduation/√12)
# The burette used was graduated every 10 mL
Uread <- 10/sqrt(12)

#The second graduation was obtained by adding again 500 ml of water using the same burette, and so on until the Nth graduation
# the uncertainty is therefore calculated as: Ucalibration = √N*Uread
Wastewatervolume_G1$Ucalibration <- sqrt(Wastewatervolume_G1$Total)*Uread
Wastewatervolume_G5$Ucalibration <- sqrt(Wastewatervolume_G5$Total)*Uread
Wastewatervolume_G12$Ucalibration <- sqrt(Wastewatervolume_G12$Total)*Uread

#2)	Uncertainty related to the reading of the volume in the barrel
# The barrel was not graduated with consistent precision and therefore if a reading was done between graduations N and (N+1)
# the standard reading uncertainty was calculated as: Ureadbarrel = (1 graduation)/√12
# The barrel was  graduated every 500 mL, therefore:
Ureadbarrel <- 500/sqrt(12)

# 3) Considering calibration and reading uncertainty
#To consider these uncertainties (calibration and reading) on the estimation of the volume V, between V(N+1) and V(N):
# U(V) = √(Ucalibration)^2+ (Ureadbarrel)^2 )
Wastewatervolume_G1$U.V <- (sqrt((Wastewatervolume_G1$Ucalibration)^2 +(Ureadbarrel)^2))/1000 #the division by 1000 is to convert ml to L
Wastewatervolume_G5$U.V <- (sqrt((Wastewatervolume_G5$Ucalibration)^2 +(Ureadbarrel)^2))/1000 
Wastewatervolume_G12$U.V <- (sqrt((Wastewatervolume_G12$Ucalibration)^2 +(Ureadbarrel)^2))/1000

# 4) Confidence interval
# One single measure of the volume of water in the barrel was done each wash.
# the Guide to the expression of Uncertainty in Measurement (GUM) recommends to calculate the confidence interval from the standard reading uncertainty (U(V))...
#...taking into account the uniform distribution associated with this type of uncertainty (uncertainty of B type), as follow : I=[-2U(V)  ; + 2U(V)]
# In our particular situation, Wastewatervolume_G1$U.V2 is used for the interval
Wastewatervolume_G1$U.V2 <-Wastewatervolume_G1$U.V*2
Wastewatervolume_G5$U.V2 <-Wastewatervolume_G5$U.V*2
Wastewatervolume_G12$U.V2 <-Wastewatervolume_G12$U.V*2
#write.table(Wastewatervolume_G1, file = "Data_filtration_Volume_analysed_red.csv", quote = F, sep = ",", row.names = F)

# max(Wastewatervolume_G1$Total)
# min(Wastewatervolume_G1$Total)
# mean(Wastewatervolume_G1$Total)
# SD(Wastewatervolume_G1$Total)

###### Wastewater fibres ######
# mean for each rows (selected columns)
Wastewaterfibres_G1$meanValue <- rowMeans(subset(Wastewaterfibres_G1, select = c(mass1: mass5)), na.rm = TRUE)
Wastewaterfibres_G5$meanValue <- rowMeans(subset(Wastewaterfibres_G5, select = c(mass1: mass5)), na.rm = TRUE)
Wastewaterfibres_G12$meanValue <- rowMeans(subset(Wastewaterfibres_G12, select = c(mass1: mass5)), na.rm = TRUE)

# standard deviation for each row (selected columns)
Wastewaterfibres_G1$standardDeviation <- rowSds(as.matrix(Wastewaterfibres_G1[,c(5,6,7,8,9)]))
Wastewaterfibres_G5$standardDeviation <- rowSds(as.matrix(Wastewaterfibres_G5[,c(5,6,7,8,9)]))
Wastewaterfibres_G12$standardDeviation <- rowSds(as.matrix(Wastewaterfibres_G12[,c(5,6,7,8,9)]))

# number of replicate measurements
Wastewaterfibres_G1$Number <- rowSums( !is.na(Wastewaterfibres_G1[,5:9]))
Wastewaterfibres_G5$Number <- rowSums( !is.na(Wastewaterfibres_G5[,5:9]))
Wastewaterfibres_G12$Number <- rowSums( !is.na(Wastewaterfibres_G12[,5:9]))

# SEM per row
Wastewaterfibres_G1$SEM <- Wastewaterfibres_G1$standardDeviation/sqrt(Wastewaterfibres_G1$Number)
Wastewaterfibres_G5$SEM <- Wastewaterfibres_G5$standardDeviation/sqrt(Wastewaterfibres_G5$Number)
Wastewaterfibres_G12$SEM <- Wastewaterfibres_G12$standardDeviation/sqrt(Wastewaterfibres_G12$Number)

# create an identifier
Wastewaterfibres_G1$ID <- Wastewaterfibres_G1$Filter
Wastewaterfibres_G5$ID <- Wastewaterfibres_G5$Filter
Wastewaterfibres_G12$ID <- Wastewaterfibres_G12$Filter

# split out each set using the ID
p1_G1 = Wastewaterfibres_G1 %>% filter(ID == "FA") %>% dplyr::select(Experiment,meanValue,SEM)
p2_G1 = Wastewaterfibres_G1 %>% filter(ID == "FN") %>% dplyr::select(Experiment,meanValue,SEM)
p1_G5 = Wastewaterfibres_G5 %>% filter(ID == "FA") %>% dplyr::select(Experiment,meanValue,SEM)
p2_G5 = Wastewaterfibres_G5 %>% filter(ID == "FN") %>% dplyr::select(Experiment,meanValue,SEM)
p1_G12 = Wastewaterfibres_G12 %>% filter(ID == "FA") %>% dplyr::select(Experiment,meanValue,SEM)
p2_G12 = Wastewaterfibres_G12 %>% filter(ID == "FN") %>% dplyr::select(Experiment,meanValue,SEM)

# and merge
merge.dat_G1 = data.frame(p2_G1[,c("Experiment")], p1_G1$meanValue, p2_G1$meanValue, p1_G1$SEM, p2_G1$SEM)
names(merge.dat_G1) = c("Experiment","P1","P2","SEM1", "SEM2")
merge.dat_G5 = data.frame(p2_G5[,c("Experiment")], p1_G5$meanValue, p2_G5$meanValue, p1_G5$SEM, p2_G5$SEM)
names(merge.dat_G5) = c("Experiment","P1","P2","SEM1", "SEM2")
merge.dat_G12 = data.frame(p2_G12[,c("Experiment")], p1_G12$meanValue, p2_G12$meanValue, p1_G12$SEM, p2_G12$SEM)
names(merge.dat_G12) = c("Experiment","P1","P2","SEM1", "SEM2")

# add the filter ID
merge.dat_G1 <- cbind(Wastewaterfibres_G1$Filter,merge.dat_G1)
merge.dat_G1 <- merge.dat_G1[1:30,]
names(merge.dat_G1)[names(merge.dat_G1) == 'Wastewaterfibres_G1$Filter'] <- 'Filter'
merge.dat_G5 <- cbind(Wastewaterfibres_G5$Filter,merge.dat_G5)
merge.dat_G5 <- merge.dat_G5[1:100,]
names(merge.dat_G5)[names(merge.dat_G5) == 'Wastewaterfibres_G5$Filter'] <- 'Filter'
merge.dat_G12 <- cbind(Wastewaterfibres_G12$Filter,merge.dat_G12)
merge.dat_G12 <- merge.dat_G12[1:66,]
names(merge.dat_G12)[names(merge.dat_G12) == 'Wastewaterfibres_G12$Filter'] <- 'Filter'

# Calculate difference and U.F
Wastewaterfibres_G1 <- merge.dat_G1 %>% mutate(Diff =  P2 -P1, U.F = sqrt((SEM1*SEM1)+(SEM2*SEM2)))
Wastewaterfibres_G5 <- merge.dat_G5 %>% mutate(Diff =  P2 -P1, U.F = sqrt((SEM1*SEM1)+(SEM2*SEM2)))
Wastewaterfibres_G12 <- merge.dat_G12 %>% mutate(Diff =  P2 -P1, U.F = sqrt((SEM1*SEM1)+(SEM2*SEM2)))

# split out each set by reshaping
Wastewaterfibres_G1p3 <- Wastewaterfibres_G1 %>%
  dplyr::select(Filter, Experiment, Diff, U.F)
Wastewaterfibres_G1p4 <- reshape(Wastewaterfibres_G1p3, idvar = "Experiment", timevar = "Filter", direction = "wide")
Wastewaterfibres_G1p4 <- data.frame(Wastewaterfibres_G1p4)

Wastewaterfibres_G5p3 <- Wastewaterfibres_G5 %>%
  dplyr::select(Filter, Experiment, Diff, U.F)
Wastewaterfibres_G5p4 <- reshape(Wastewaterfibres_G5p3, idvar = "Experiment", timevar = "Filter", direction = "wide")
Wastewaterfibres_G5p4 <- data.frame(Wastewaterfibres_G5p4)

Wastewaterfibres_G12p3 <- Wastewaterfibres_G12 %>%
  dplyr::select(Filter, Experiment, Diff, U.F)
Wastewaterfibres_G12p4 <- reshape(Wastewaterfibres_G12p3, idvar = "Experiment", timevar = "Filter", direction = "wide")
Wastewaterfibres_G12p4 <- data.frame(Wastewaterfibres_G12p4)

# Calculate difference and U.F
Wastewaterfibres_G1p4$Wf <- round(Wastewaterfibres_G1p4$Diff.FN-Wastewaterfibres_G1p4$Diff.FA, digit=2)
Wastewaterfibres_G1p4$U.C <- sqrt((Wastewaterfibres_G1p4$U.F.FA*Wastewaterfibres_G1p4$U.F.FA)+ (Wastewaterfibres_G1p4$U.F.FN*Wastewaterfibres_G1p4$U.F.FN))
Wastewaterfibres_G1p4$U.C3 <- round(Wastewaterfibres_G1p4$U.C*3, digit=2)

Wastewaterfibres_G5p4$Wf <- round(Wastewaterfibres_G5p4$Diff.FN-Wastewaterfibres_G5p4$Diff.FA, digit=2)
Wastewaterfibres_G5p4$U.C <- sqrt((Wastewaterfibres_G5p4$U.F.FA*Wastewaterfibres_G5p4$U.F.FA)+ (Wastewaterfibres_G5p4$U.F.FN*Wastewaterfibres_G5p4$U.F.FN))
Wastewaterfibres_G5p4$U.C3 <- round(Wastewaterfibres_G5p4$U.C*3, digit=2)

Wastewaterfibres_G12p4$Wf <- round(Wastewaterfibres_G12p4$Diff.FN-Wastewaterfibres_G12p4$Diff.FA, digit=2)
Wastewaterfibres_G12p4$U.C <- sqrt((Wastewaterfibres_G12p4$U.F.FA*Wastewaterfibres_G12p4$U.F.FA)+ (Wastewaterfibres_G12p4$U.F.FN*Wastewaterfibres_G12p4$U.F.FN))
Wastewaterfibres_G12p4$U.C3 <- round(Wastewaterfibres_G12p4$U.C*3, digit=2)

# max(Wastewaterfibres_G1p4$Diff.FN)
# min(Wastewaterfibres_G1p4$Diff.FN)
# mean(Wastewaterfibres_G1p4$Diff.FN)
# SD(Wastewaterfibres_G1p4$Diff.FN)
write.table(Wastewaterfibres_G1p4, file = "Data_filtration G1_fibre weight_processed.csv", quote = F, sep = ",", row.names = F)
write.table(Wastewaterfibres_G5p4, file = "Data_filtration G5_fibre weight_processed.csv", quote = F, sep = ",", row.names = F)
write.table(Wastewaterfibres_G12p4, file = "Data_filtration G12_fibre weigh_processed.csv", quote = F, sep = ",", row.names = F)


###### GRAPH - Volume ######
# exclude wash W034 from Wastewaterfibres_G5p4 and Wastewatervolume_G5
Wastewaterfibres_G5p4 <- Wastewaterfibres_G5p4[Wastewaterfibres_G5p4$Experiment != 34, ]
Wastewatervolume_G5 <- Wastewatervolume_G5[Wastewatervolume_G5$Washnumber != 34, ]

# Data obtained with 1 garment
lm(Total~Washnumber, data=Wastewatervolume_G1)
pVolume_G1 <- ggplot(data = Wastewatervolume_G1, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "#469990")+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 2), limits = c(18, 29),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 15, by = 2), limits = c(1, 15),expand = c(0.03,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5,  color="#469990")+
  annotate(geom="text", x=13, y=19, label="y = 22.3 - 0.04 x",color="black")# values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibres_G1p4) above
meanVolume_G1 <- round(mean(Wastewatervolume_G1$Total),digits = 2)
pVolume_G1 <-pVolume_G1 + annotate("text",  x=Inf, y = Inf, label = meanVolume_G1, vjust=2, hjust=1.5)
show(pVolume_G1)
ggsave("Wastewater volume_G1.png", pVolume_G1, width = 7, height = 4, units = "in", dpi=600, path = "Results")

# Data obtained with 5 garments
lm(Total~Washnumber, data=Wastewatervolume_G5)
pVolume_G5 <- ggplot(data = Wastewatervolume_G5, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "black")+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 2), limits = c(18, 29),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51),expand = c(0.03,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5)+
  annotate(geom="text", x=44, y=19, label="y = 22.4 - 0.06 x",color="black")# values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibres_G5p4) above
meanVolume_G5 <- round(mean(Wastewatervolume_G5$Total),digits = 2)
pVolume_G5 <-pVolume_G5 + annotate("text",  x=Inf, y = Inf, label = meanVolume_G5, vjust=2, hjust=1.5)
show(pVolume_G5)
ggsave("Wastewater volume_G5.png", pVolume_G5, width = 7, height = 4, units = "in", dpi=600, path = "Results")

# Data obtained with 12 garments
lm(Total~Washnumber, data=Wastewatervolume_G12)
pVolume_G12 <- ggplot(data = Wastewatervolume_G12, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "darkred")+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 2), limits = c(18, 29),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 33, by = 2), limits = c(1, 33),expand = c(0.03,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5, color="darkred")+
  annotate(geom="text", x=27, y=19, label="y = 22.7 - 0.04 x",color="black")# values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibres_G12p4) above
meanVolume_G12 <- round(mean(Wastewatervolume_G12$Total),digits = 2)
pVolume_G12 <-pVolume_G12 + annotate("text",  x=Inf, y = Inf, label = meanVolume_G12, vjust=2, hjust=1.5)
show(pVolume_G12)
ggsave("Wastewater volume_G12.png", pVolume_G12, width = 7, height = 4, units = "in", dpi=600, path = "Results")

#### Combined results ####
pVolume_combined_pending <- ggarrange(pVolume_G1+ rremove("ylab") + rremove("xlab"),
                                      pVolume_G5+ rremove("ylab") + rremove("xlab"),
                                      pVolume_G12+ rremove("ylab") + rremove("xlab"),
                                      nrow = 3, labels = c("A", "B", "C"),
                                      vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)

pVolume_combined <- annotate_figure(pVolume_combined_pending, left = textGrob("Volume of wastewater (L)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                    bottom = textGrob("\nWash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pVolume_combined

ggsave("pVolume_combined.png", pVolume_combined, width = 8, height = 7, units = "in", dpi=300, path = "Results")

###### GRAPH - Fibre ######
# obtained with 1 garment
lm(Diff.FN~Experiment, data=Wastewaterfibres_G1p4)
pfibres_G1 <- ggplot(data = Wastewaterfibres_G1p4, aes(x =Experiment, y = Diff.FN)) +
  geom_line(colour = "Tomato")+
  labs(x="Wash number", y="Fibres (mg)")+
  scale_y_continuous(breaks = seq(0, 120, by = 10), limits = c(0, 120),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 15, by = 2), limits = c(1, 15),expand = c(0.01,0))+
  theme_bw( base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Diff.FN-U.C3, ymax=Diff.FN+U.C3), width=0.5)+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  annotate(geom="text", x=7, y=25, label="y =  68.608 - 3.771 x", # values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibres_G1p4) above
               color="black")
pfibres_G1
ggsave("Wastewater fibres_G1.png", pfibres_G1, width = 7, height = 4, units = "in", dpi=600, path = "Results")

# obtained with 5 garment
lm(Diff.FN~Experiment, data=Wastewaterfibres_G5p4)
pfibres_G5 <- ggplot(data = Wastewaterfibres_G5p4, aes(x =Experiment, y = Diff.FN)) +
  geom_line(colour = "Tomato")+
  labs(x="Wash number", y="Fibres (mg)")+
  scale_y_continuous(breaks = seq(0, 170, by = 10), limits = c(0, 170),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51),expand = c(0.01,0))+
  theme_bw( base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Diff.FN-U.C3, ymax=Diff.FN+U.C3), width=0.5)+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  annotate(geom="text", x=7, y=25, label="y = 61.386 - 1.844 x", # values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibres_G5p4) above
           color="black")
show(pfibres_G5)
ggsave("Wastewater fibres_G5.png", pfibres_G5, width = 7, height = 4, units = "in", dpi=600, path = "Results")

meanfibre_G5 <- round(mean(Wastewaterfibres_G5p4$Diff.FN),digits = 2);meanfibre_G5
meanfibre_G5_norm <-meanfibre_G5
SDfibre_G5 <- round(sd(Wastewaterfibres_G5p4$Diff.FN),digits = 2);SDfibre_G5

# obtained with 12 garments
lm(Diff.FN~Experiment, data=Wastewaterfibres_G12p4)
pfibres_G12 <- ggplot(data = Wastewaterfibres_G12p4, aes(x =Experiment, y = Diff.FN)) +
  geom_line(colour = "Tomato")+
  labs(x="Wash number", y="Fibres (mg)")+
  scale_y_continuous(breaks = seq(0, 80, by = 120), limits = c(0, 120),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 31, by = 2), limits = c(1, 31),expand = c(0.01,0))+
  theme_bw( base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Diff.FN-U.C3, ymax=Diff.FN+U.C3), width=0.5)+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  annotate(geom="text", x=7, y=25, label="y = 61.386 - 1.844 x", # values obtained with lm(Diff.FN~Experiment, data=Wastewaterfibres_G12p4) above
           color="black")
meanfibre_G12 <- round(mean(Wastewaterfibres_G12p4$Diff.FN),digits = 2)
pfibres_G12 <-pfibres_G12 + annotate("text",  x=Inf, y = Inf, label = meanfibre_G12, vjust=2, hjust=1.5)
show(pfibres_G12)
ggsave("Wastewater fibres_G12.png", pfibres_G12, width = 7, height = 4, units = "in", dpi=600, path = "Results")

###### GRAPH - Fibre normalised to the weight of the garment ######
Wastewaterfibres_G1p4$norm <- Wastewaterfibres_G1p4$Diff.FN/0.343
Wastewaterfibres_G1p4$normUC <- Wastewaterfibres_G1p4$U.C3/0.343
Wastewaterfibres_G1p4$Coder <- "1 garment"
Wastewaterfibres_G5p4$norm <- Wastewaterfibres_G5p4$Diff.FN/1.500
Wastewaterfibres_G5p4$normUC <- Wastewaterfibres_G5p4$U.C3/1.500
Wastewaterfibres_G5p4$Coder <- "5 garments"
Wastewaterfibres_G12p4$norm <- Wastewaterfibres_G12p4$Diff.FN/3.000
Wastewaterfibres_G12p4$normUC <- Wastewaterfibres_G12p4$U.C3/3.000
Wastewaterfibres_G12p4$Coder <- "12 garments"
Wastewaterfibres_Total <- rbind(Wastewaterfibres_G1p4,Wastewaterfibres_G5p4,Wastewaterfibres_G12p4)

# Set different shapes for points based on the coder variable
coder_shapes <- c(16, 17, 15)  # You can add more shape codes if needed

# Set different colors for the linear regression lines based on the coder variable
coder_colors <- c("#469990", "darkred", "black")  # You can add more colors if needed

# Change the ggplot code to plot all linear regression lines with equations
modelG1 <- lm(Experiment ~ norm, data = Wastewaterfibres_G1p4);modelG1
modelG5 <- lm(Experiment ~ norm, data = Wastewaterfibres_G5p4);modelG5
modelG12 <- lm(Experiment ~ norm, data = Wastewaterfibres_G12p4);modelG12

pfibres_Total <- ggplot(data = Wastewaterfibres_Total, aes(x = Experiment, y = norm, color = Coder)) +
  geom_point(shape = coder_shapes[1], size = 2) +     # Use the first shape code for all points
  labs(x = "Wash number", y = "Fibres (mg)") +
  guides(color = guide_legend(title = NULL)) +
  scale_y_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51), expand = c(0.01, 0)) +
  scale_color_manual(values = coder_colors) +   # Set different colors for the linear regression lines
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "grey95", size = 1, linetype = "solid", colour = "grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +  # top, right, bottom, left
  geom_errorbar(aes(ymin = norm - U.C3, ymax = norm + U.C3), width = 0.5) +
  geom_smooth(method = lm, se = FALSE, size = 0.8) +  # Add all linear regression lines
  # Add regression equations as annotations using annotate()
  annotate(geom = "text", x = 45, y = 190, label = "y = -3.2 + 0.5 * x", color = "#469990") +
  annotate(geom = "text", x = 45, y = 175, label = "y = 17.2 - 0.1 * x", color = "darkred") +
  annotate(geom = "text", x = 45, y = 160, label = "y = 43.0 - 0.2* x", color = "black")
show(pfibres_Total)

ggsave("Wastewater fibres_Total normalised.png", pfibres_Total, width = 7, height = 5, units = "in", dpi=600, path = "Results")

#### GRAPH - Pearson correlation ####
# Null hypothesis – There is no significant correlation between the volume and the wash number
# The alternative hypothesis – There is a significant correlation between the volume and the wash number
# set alpha level to 0.05
# Prepare the data
PersonG1 <- cbind(Wastewaterfibres_G1p4, Volume =Wastewatervolume_G1$Total)
PersonG5 <- cbind(Wastewaterfibres_G5p4, Volume =Wastewatervolume_G5$Total)
PersonG12 <- cbind(Wastewaterfibres_G12p4, Volume =Wastewatervolume_G12$Total)

#Visualize data using scatter plots Fibres VS wash number
# obtained with 1 garment
PearsonFW_G1 <- ggscatter(PersonG1, x = "Volume", y = "Diff.FN",
                          add = "reg.line",
                          xlab = "Volume of water (L)", ylab = "Fibre (mg)",
                          ylim = c(0, 80),
                          xlim = c(21, 23.5),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 23,label.y = 5, label.sep = "\n"))
PearsonFW_G1                 
ggsave("Pearson fibre VS wash number_G1.png", PearsonFW_G1, width = 7, height = 4, units = "in", dpi=600, path = "Results")

# obtained with 5 garments
PearsonFW_G5 <- ggscatter(PersonG5, x = "Volume", y = "Diff.FN",
                          add = "reg.line",
                          xlab = "Volume of water (L)", ylab = "Fibre (mg)",
                          ylim = c(50, 160),
                          xlim = c(19.5, 27),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 26,label.y = 55, label.sep = "\n"))
PearsonFW_G5                 
ggsave("Pearson fibre VS wash number_G5.png", PearsonFW_G5, width = 7, height = 4, units = "in", dpi=600, path = "Results")

# obtained with 12 garments
PearsonFW_G12 <- ggscatter(PersonG12, x = "Volume", y = "Diff.FN",
                           add = "reg.line",
                           xlab = "Volume of water (L)", ylab = "Fibre (mg)",
                          ylim = c(25, 110),
                          xlim = c(21.5, 24.5),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 24,label.y = 30, label.sep = "\n"))
PearsonFW_G12                 
ggsave("Pearson fibre VS wash number_G12.png", PearsonFW_G12, width = 7, height = 4, units = "in", dpi=600, path = "Results")

#### Combined results 
pPearson_combined_pending <- ggarrange(PearsonFW_G1+ rremove("ylab") + rremove("xlab"),
                                      PearsonFW_G5+ rremove("ylab") + rremove("xlab"),
                                      PearsonFW_G12+ rremove("ylab") + rremove("xlab"),
                                      nrow = 3, labels = c("A", "B", "C"),
                                      vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)

pPearson_combined <- annotate_figure(pPearson_combined_pending, left = textGrob("Volume of water (L)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                    bottom = textGrob("\nWash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pVolume_combined

ggsave("pPearson_combined.png", pPearson_combined, width = 5, height = 9, units = "in", dpi=300, path = "Results")

### STATS FOR ARTICLE ###
### Volume of water
Wastewatervolume_G1$Coder <- "G1"
Wastewatervolume_G5$Coder <- "G5"
Wastewatervolume_G12$Coder <- "G12"
TotalWastewatervolume <- rbind(Wastewatervolume_G1, Wastewatervolume_G5,Wastewatervolume_G12)
meanVolume_Total <- aggregate(Total ~  Coder, TotalWastewatervolume, function(x) {round(mean(x), digits=2)})
SDVolume_Total <- aggregate(Total ~  Coder, TotalWastewatervolume, function(x) {round(SD(x), digits=2)})
medianVolume_Total <- aggregate(Total ~  Coder, TotalWastewatervolume, median)
Table_Volume_Total <- cbind(meanVolume_Total, SDVolume_Total$Total, medianVolume_Total$Total)

### Fibre released in the wastewater
meanFibre_Total <- aggregate(Diff.FN ~  Coder, TotalWastewaterfibres, function(x) {round(mean(x), digits=2)})
SDFibre_Total <- aggregate(Diff.FN ~  Coder, TotalWastewaterfibres, function(x) {round(SD(x), digits=2)})
medianFibre_Total <- aggregate(Diff.FN ~  Coder, TotalWastewaterfibres, median)
SampleSizeFibre_Total <- aggregate(Diff.FN ~  Coder, TotalWastewaterfibres, function(x) {round(length(x), digits=2)})
SEMFibre_Total <- round((SDFibre_Total$Diff.FN)/(sqrt(SampleSizeFibre_Total$Diff.FN)),digits=2)
Table_Fibre_Total <- cbind(meanFibre_Total, SDFibre_Total$Diff.FN, medianFibre_Total$Diff.FN, SEMFibre_Total)
