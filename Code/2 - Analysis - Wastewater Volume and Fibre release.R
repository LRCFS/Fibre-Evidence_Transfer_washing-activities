#############################################################
#####                     To read                       #####
#############################################################
# This R script is to generate the figures related to the Wastewater Volume and Fibre release
# 1. Wastewater volume
# 2. Fibres released in the wastewater
# 3: Mass of fibres VS volume of wastewater

# ------------------------------------------------------------------------
# Section 1: Wastewater volume
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
  # 1) Uncertainty related to the graduation of the barrel (calibration)
# To calibrate the barrel, a 500 ml burette was filled up to the 500 ml graduation and pour into the barrel.
# At the first calibration line on the barrel, the uncertainty on the position of the line corresponds to the uncertainty
# on the measured volume (standard uncertainty of reading): Uread = (1 graduation/√12)
# The burette used was graduated every 10 mL
Uread <- 10/sqrt(12)

#The second graduation was obtained by adding again 500 ml of water using the same burette, and so on until the Nth graduation
# the uncertainty is therefore calculated as: Ucalibration = √N*Uread
Wastewatervolume_G1$Ucalibration <- sqrt(Wastewatervolume_G1$Total)*Uread
Wastewatervolume_G5$Ucalibration <- sqrt(Wastewatervolume_G5$Total)*Uread
Wastewatervolume_G12$Ucalibration <- sqrt(Wastewatervolume_G12$Total)*Uread

  # 2)	Uncertainty related to the reading of the volume in the barrel
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

#### Intermediate Data Visualization ####
# exclude wash W034 from Wastewatervolume_G5
Wastewatervolume_G5 <- Wastewatervolume_G5[Wastewatervolume_G5$Washnumber != 34, ]

# Data obtained with 1 garment
lm(Total~Washnumber, data=Wastewatervolume_G1)
meanVolume_G1 <- round(mean(Wastewatervolume_G1$Total), digits = 2);meanVolume_G1
pVolume_G1 <- ggplot(data = Wastewatervolume_G1, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "#469990")+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 2), limits = c(15, 30),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 15, by = 2), limits = c(1, 15),expand = c(0.03,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5,  color="#469990")+
  annotate(geom="text", x=13.75, y=29.1, label="y = - 0.04x + 22.3",color="black")+
  annotate(geom="text", x=13.75, y=27.5, label="bar(x) == 21.95",color="black",parse=T)
show(pVolume_G1)

# Data obtained with 5 garments
lm(Total~Washnumber, data=Wastewatervolume_G5)
meanVolume_G5 <- round(mean(Wastewatervolume_G5$Total), digits = 2);meanVolume_G5
pVolume_G5 <- ggplot(data = Wastewatervolume_G5, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "black")+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 2), limits = c(15, 30),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51),expand = c(0.03,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5)+
  annotate(geom="text", x=47, y=29.1, label="y = - 0.06 x + 22.4 ",color="black")+
  annotate(geom="text", x=47, y=27.5, label="bar(x) == 23.88",color="black",parse=T)
show(pVolume_G5)

# Data obtained with 12 garments
lm(Total~Washnumber, data=Wastewatervolume_G12)
meanVolume_G12 <- round(mean(Wastewatervolume_G12$Total), digits = 2);meanVolume_G12
pVolume_G12 <- ggplot(data = Wastewatervolume_G12, aes(x =Washnumber, y = Total)) +
  geom_line(colour = "darkred")+
  geom_smooth(method = lm, se = FALSE,formula = y ~ x, color="black", linetype="dashed", size=0.5)+
  labs(x="Wash number", y="Volume of water (L)")+
  scale_y_continuous(breaks = seq(0, 35, by = 2), limits = c(15, 30),expand = c(0,0))+
  scale_x_continuous(breaks = seq(1, 41, by = 2), limits = c(1, 41),expand = c(0.03,0))+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Total-U.V2, ymax=Total+U.V2), width=0.5, color="darkred")+
  annotate(geom="text", x=37.5, y=29.1, label="y = 0.01 x + 23.11",color="black")+
  annotate(geom="text", x=37.5, y=27.5, label="bar(x) == 23.4",color="black",parse=T)
show(pVolume_G12)

#### Final graph - Figure 6 ####
pVolume_combined_pending <- ggarrange(pVolume_G1+ rremove("ylab") + rremove("xlab"),
                                      pVolume_G5+ rremove("ylab") + rremove("xlab"),
                                      pVolume_G12+ rremove("ylab") + rremove("xlab"),
                                      nrow = 3, labels = c("A", "B", "C"),
                                      vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)

pVolume_combined <- annotate_figure(pVolume_combined_pending, left = textGrob("Volume of wastewater (L)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                    bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pVolume_combined
pVolume_combined

# to save the graph
ggsave("pVolume_combined.png", pVolume_combined, width = 7, height = 8, units = "in", dpi=300, path = "Results")

# ------------------------------------------------------------------------
# Section 2: Fibres released in the wastewater
# ------------------------------------------------------------------------
#### Data Cleaning and Processing ####
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
merge.dat_G12 <- merge.dat_G12[1:82,]
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

# Normalising the mass of fibres released in the wastewater to the weight of the garment(s)
Wastewaterfibres_G1p4$norm <- Wastewaterfibres_G1p4$Diff.FN/G1weightTotal
Wastewaterfibres_G1p4$normUC <- Wastewaterfibres_G1p4$U.C3/G1weightTotal
Wastewaterfibres_G1p4$Coder <- "1 garment"
Wastewaterfibres_G5p4$norm <- Wastewaterfibres_G5p4$Diff.FN/G5weightTotal
Wastewaterfibres_G5p4$normUC <- Wastewaterfibres_G5p4$U.C3/G5weightTotal
Wastewaterfibres_G5p4$Coder <- "5 garments"
Wastewaterfibres_G12p4$norm <- Wastewaterfibres_G12p4$Diff.FN/G12weightTotal
Wastewaterfibres_G12p4$normUC <- Wastewaterfibres_G12p4$U.C3/G12weightTotal
Wastewaterfibres_G12p4$Coder <- "12 garments"
Wastewaterfibres_Total <- rbind(Wastewaterfibres_G1p4,Wastewaterfibres_G5p4,Wastewaterfibres_G12p4)

#### Final graph - Figure 7  #####
# Set different shapes for points based on the coder variable
coder_shapes <- c(16, 17, 15)  # You can add more shape codes if needed

# Set different colors for the linear regression lines based on the coder variable
coder_colors <- c("#469990", "darkred", "black")  # You can add more colors if needed

# Change the ggplot code to plot all linear regression lines with equations
modelG1 <- lm(Experiment ~ norm, data = Wastewaterfibres_G1p4);modelG1
modelG5 <- lm(Experiment ~ norm, data = Wastewaterfibres_G5p4);modelG5
modelG12 <- lm(Experiment ~ norm, data = Wastewaterfibres_G12p4);modelG12

# plot the graph
pfibres_Total <- ggplot(data = Wastewaterfibres_Total, aes(x = Experiment, y = norm, color = Coder)) +
  geom_point(shape = coder_shapes[1], size = 2) +     # Use the first shape code for all points
  labs(x = "Wash number", y = "Fibres released (mg)\n normalised to the weight of the garment(s)") +
  guides(color = guide_legend(title = NULL)) +
  scale_y_continuous(breaks = seq(0, 200, by = 20), limits = c(0, 200), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1, 51, by = 2), limits = c(1, 51), expand = c(0.01, 0)) +
  scale_color_manual(values = coder_colors) +   # Set different colors for the linear regression lines
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "white"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "grey95", size = 1, linetype = "solid", colour = "grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +  # top, right, bottom, left
  geom_errorbar(aes(ymin = norm - U.C3, ymax = norm + U.C3), width = 0.5) +
  geom_smooth(method = lm, se = FALSE, size = 0.8) +  # Add all linear regression lines
  # Add regression equations as annotations using annotate()
  annotate(geom = "text", x = 41.5, y = 190, label = "1 garment:  y = - 0.1 x + 22.8 ", color = "#469990") +
  annotate(geom = "text", x = 41.8, y = 175, label = "5 garments:  y = - 0.2 x + 43.0", color = "black") +
  annotate(geom = "text", x = 41.8, y = 160, label = "12 garments:  y = 1.1 x - 12.8", color = "darkred")
show(pfibres_Total)

# To save the graph
ggsave("Wastewater fibres_Total normalised.png", pfibres_Total, width = 7, height = 5, units = "in", dpi=600, path = "Results")

# the warning message showing correspond to the wash W034 performed with G5, excluded at line 49

# ------------------------------------------------------------------------
# Section 3: Mass of fibres VS volume of wastewater
# ------------------------------------------------------------------------
#### Intermediate Data Visualization ####
# Null hypothesis – There is no significant correlation between the volume and the wash number
# The alternative hypothesis – There is a significant correlation between the volume and the wash number
# set alpha level to 0.05
# Prepare the data
PearsonG1 <- cbind(Wastewaterfibres_G1p4, Volume =Wastewatervolume_G1$Total)
PearsonG5 <- cbind(Wastewaterfibres_G5p4, Volume =Wastewatervolume_G5$Total)
PearsonG12 <- cbind(Wastewaterfibres_G12p4, Volume =Wastewatervolume_G12$Total)

#Visualize data using scatter plots Fibres VS wash number
# obtained with 1 garment
PearsonFW_G1 <- ggscatter(PearsonG1, x = "Volume", y = "Diff.FN",
                          add = "reg.line",
                          xlab = "Volume of water (L)", ylab = "Fibre (mg)",
                          ylim = c(20, 80),
                          xlim = c(21, 23.5),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 23.25,label.y = 30, label.sep = "\n"))
PearsonFW_G1     

# obtained with 5 garments
PearsonFW_G5 <- ggscatter(PearsonG5, x = "Volume", y = "Diff.FN",
                          add = "reg.line",
                          xlab = "Volume of water (L)", ylab = "Fibre (mg)",
                          ylim = c(50, 160),
                          xlim = c(19.5, 27.5),
                          cor.coef = TRUE,
                          cor.coeff.args = list(method = "pearson", label.x = 26.25,label.y = 65, label.sep = "\n"))
PearsonFW_G5       

# obtained with 12 garments
PearsonFW_G12 <- ggscatter(PearsonG12, x = "Volume", y = "Diff.FN",
                           add = "reg.line",
                           xlab = "Volume of water (L)", ylab = "Fibre (mg)",
                           ylim = c(40, 130),
                           xlim = c(17, 26.5),
                           cor.coef = TRUE,
                           cor.coeff.args = list(method = "pearson", label.x = 25.25,label.y = 55, label.sep = "\n"))
PearsonFW_G12                 

#### Final graph - Figure S3 (supplementary information) #### 
pPearson_combined_pending <- ggarrange(PearsonFW_G1+ rremove("ylab") + rremove("xlab"),
                                       PearsonFW_G5+ rremove("ylab") + rremove("xlab"),
                                       PearsonFW_G12+ rremove("ylab") + rremove("xlab"),
                                       nrow = 3, labels = c("A", "B", "C"),
                                       vjust = 0.9, hjust = 0.9)+
  theme(plot.margin = margin(0.5,0,0,0, "cm")) # in order (Top,left,bottom,right)

pPearson_combined <- annotate_figure(pPearson_combined_pending, left = textGrob("Fibres (mg)\n", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                                     bottom = textGrob("\nVolume of water (L)", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pVolume_combined
pPearson_combined

# To save the graph
ggsave("pPearson_combined.png", pPearson_combined, width = 6, height = 8, units = "in", dpi=300, path = "Results")

#remove unused dataframe
rm(merge.dat_G1,merge.dat_G12,merge.dat_G5,p1_G1,p1_G12,p1_G5,p2_G1,p2_G12,p2_G5,
   PearsonFW_G1,PearsonFW_G5,PearsonFW_G12,PearsonG1,PearsonG12, PearsonG5,modelG1,
   modelG5,modelG12,Wastewaterfibres_G12p3,Wastewaterfibres_G1p3,Wastewaterfibres_G5p3,
   pPearson_combined,pPearson_combined_pending,pVolume_combined,pVolume_combined_pending,
   pVolume_G1,pVolume_G5,pVolume_G12,pfibres_Total,Wastewaterfibres_G1,Wastewaterfibres_G5,
   Wastewaterfibres_G12, Wastewaterfibres_Total)
