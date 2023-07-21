#############################################################
#####                      Calibration                  #####
#############################################################
# First Calibration
Calibration <- read.csv('Load Cell calibration -03052021.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
linear.regression <-lm(Weight~Loadcell, Calibration)
summary(linear.regression)

# adjusted R²
summary(linear.regression)$adj.r.squared
# R²
summary(linear.regression)$r.squared

eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

pCalibration <- ggplot(data = Calibration, aes(x = Weight, y = Loadcell)) +
  geom_point(size=2, colour= "darkred")+
  labs(x="MCalib (g)", y="XCalib (g)")+
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Loadcell-Uloadcell, ymax=Loadcell+Uloadcell), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 15,label.x = 900) #this means at 9th unit regression line equation will be shown
show(pCalibration)
ggsave("Calibration Load Cell - 03052021.png", pCalibration, width = 7, height = 4, units = "in", dpi=150, path = "Results")

# Second Calibration
Calibration2 <- read.csv('Load Cell Calibration -22022022.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
linear.regression <-lm(Weight~Loadcell, Calibration2)
summary(linear.regression)

# adjusted R²
summary(linear.regression)$adj.r.squared
# R²
summary(linear.regression)$r.squared

pCalibration2 <- ggplot(data = Calibration2, aes(x = Weight, y = Loadcell)) +
  geom_point(size=0.4, colour= "darkred")+
  labs(x="MCalib (g)", y="XCalib (g)")+
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Loadcell-Uloadcell, ymax=Loadcell+Uloadcell), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  geom_text(x = 150, y = 800, label = eq(Calibration$Weight,Calibration$Loadcell), parse = T)
show(pCalibration2)
ggsave("Calibration Load Cell - 22022022.png", pCalibration2, width = 7, height = 4, units = "in", dpi=900, path = "Results")

# Third Calibration
Calibration3 <- read.csv('Load Cell Calibration -16062022.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
linear.regression <-lm(Weight~Loadcell, Calibration3)
summary(linear.regression)

# adjusted R²
summary(linear.regression)$adj.r.squared
# R²
summary(linear.regression)$r.squared

pCalibration3 <- ggplot(data = Calibration3, aes(x = Weight, y = Loadcell)) +
  geom_point(size=0.4, colour= "darkred")+
  labs(x="MCalib (g)", y="XCalib (g)")+
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+ # top, right,bottom,left
  geom_errorbar(aes(ymin=Loadcell-Uloadcell, ymax=Loadcell+Uloadcell), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
show(pCalibration3)
ggsave("Calibration Load Cell - 16062022.png", pCalibration3, width = 7, height = 4, units = "in", dpi=900, path = "Results")

#############################################################
#####                      Data upload                  #####
#############################################################
# Select the working directory with the data
setwd("C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Arduino/Wash/first calibration")
# specify the type of file we want to import (.csv)
filesfirstcalib <- list.files(pattern="*.csv")
# create a list of data frame where each data frame correspond to a single .csv file
data_listfirstcalib <- lapply(filesfirstcalib, read.csv, skip=1, header = F,  sep = ",", stringsAsFactors = F)
# rename each data frame in the list with the name of the .csv
names(data_listfirstcalib) <- gsub(".*/(.*)\\..*", "\\1", filesfirstcalib)

# Select the working directory with the data
setwd("C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Arduino/Wash/second calibration")
# specify the type of file we want to import (.csv)
filessecondcalib <- list.files(pattern="*.csv")
# create a list of data frame where each data frame correspond to a single .csv file
data_listsecondcalib <- lapply(filessecondcalib, read.csv, skip=1, header = F,  sep = ",", stringsAsFactors = F)
# rename each data frame in the list with the name of the .csv
names(data_listsecondcalib) <- gsub(".*/(.*)\\..*", "\\1", filessecondcalib)

# Select the working directory with the data
setwd("C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Arduino/Wash/third calibration")
# specify the type of file we want to import (.csv)
filesthirdcalib <- list.files(pattern="*.csv")
# create a list of data frame where each data frame correspond to a single .csv file
data_listthirdcalib <- lapply(filesthirdcalib, read.csv, skip=1, header = F,  sep = ",", stringsAsFactors = F)
# rename each data frame in the list with the name of the .csv
names(data_listthirdcalib) <- gsub(".*/(.*)\\..*", "\\1", filesthirdcalib)

# Rename columns
colnames <- c("analogVal","load","distance")
data_listfirstcalib <- lapply(data_listfirstcalib, setNames, colnames)
data_listsecondcalib <- lapply(data_listsecondcalib, setNames, colnames)
data_listthirdcalib <- lapply(data_listthirdcalib, setNames, colnames)

# remove all the rows in load that have a value lower than 100
data_listfirstcalibreduced <- lapply(data_listfirstcalib, function(x) x[x[[2]] > 100, ])
data_listsecondcalibreduced <- lapply(data_listsecondcalib, function(x) x[x[[2]] > 100, ])
data_listthirdcalibreduced <- lapply(data_listthirdcalib, function(x) x[x[[2]] > 100, ])

# add a column distance
data_listfirstcalibreducedbis <- lapply(data_listfirstcalibreduced, function(x) {
  mutate(x, Diff = abs(x[1,3] - distance))})
data_listsecondcalibreducedbis <- lapply(data_listsecondcalibreduced, function(x) {
  mutate(x, Diff = abs(x[1,3] - distance))})
tidy_linear_regression <- lapply(data_listthirdcalibreduced, function(x) {
  x[] <- lapply(x, as.numeric)
  x
})

data_listthirdcalibreducedbis <- lapply(tidy_linear_regression, function(x) {
  mutate(x, Diff = abs(x[1,3] - distance))})

# extract all the local maximum from the data frame
local_max <- function(x) {which(diff(sign(diff(x)))==-2)+1}
localmaxfirstcalib <- lapply(data_listfirstcalibreducedbis, function(x) x$load[local_max(x$load)])
localmaxsecondcalib <- lapply(data_listsecondcalibreducedbis, function(x) x$load[local_max(x$load)])
localmaxthirdcalib <- lapply(data_listthirdcalibreducedbis, function(x) x$load[local_max(x$load)])

# select the first local maximum from the data frame (static coefficient)
localmaxfirstcalib <- lapply(localmaxfirstcalib, function(x) x[[1]][1])
localmaxsecondcalib <- lapply(localmaxsecondcalib, function(x) x[[1]][1])
localmaxthirdcalib <- lapply(localmaxthirdcalib, function(x) x[[1]][1])

# create a data frame with all the the first local maximum
StaticTotalfirstcalib1 <- data.frame(Name=names(data_listfirstcalib))
StaticTotalfirstcalib2 <- do.call(rbind.data.frame, localmaxfirstcalib)
names(StaticTotalfirstcalib2) <- c("Fstat")
StaticTotalfirstcalib <- cbind(StaticTotalfirstcalib1,StaticTotalfirstcalib2)

StaticTotalsecondcalib1 <- data.frame(Name=names(data_listsecondcalib))
StaticTotalsecondcalib2 <- do.call(rbind.data.frame, localmaxsecondcalib)
names(StaticTotalsecondcalib2) <- c("Fstat")
StaticTotalsecondcalib <- cbind(StaticTotalsecondcalib1,StaticTotalsecondcalib2)

StaticTotalthirdcalib1 <- data.frame(Name=names(data_listthirdcalib))
StaticTotalthirdcalib2 <- do.call(rbind.data.frame, localmaxthirdcalib)
names(StaticTotalthirdcalib2) <- c("Fstat")
StaticTotalthirdcalib <- cbind(StaticTotalthirdcalib1,StaticTotalthirdcalib2)

#####################################################
# load the data for the calibration
setwd("C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Arduino/Calibration files/100CB")
XrefW000 <- read.csv('W000_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW001 <- read.csv('W001_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW002 <- read.csv('W002_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW003 <- read.csv('W003_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW004 <- read.csv('W004_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW005 <- read.csv('W005_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW006 <- read.csv('W006_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW007 <- read.csv('W007_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW008 <- read.csv('W008_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW009 <- read.csv('W009_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW010 <- read.csv('W010_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW011 <- read.csv('W011_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW012 <- read.csv('W012_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW013 <- read.csv('W013_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW014 <- read.csv('W014_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW015 <- read.csv('W015_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW016 <- read.csv('W016_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW017 <- read.csv('W017_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW018 <- read.csv('W018_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW019 <- read.csv('W019_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW020 <- read.csv('W020_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW021 <- read.csv('W021_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW022 <- read.csv('W022_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW023 <- read.csv('W023_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW024 <- read.csv('W024_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW025 <- read.csv('W025_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW026 <- read.csv('W026_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW027 <- read.csv('W027_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW028 <- read.csv('W028_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW029 <- read.csv('W029_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW030 <- read.csv('W030_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW031 <- read.csv('W031_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW032 <- read.csv('W032_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW033 <- read.csv('W033_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW034 <- read.csv('W034_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW035 <- read.csv('W035_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW036 <- read.csv('W036_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW037 <- read.csv('W037_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW038 <- read.csv('W038_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW039 <- read.csv('W039_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW040 <- read.csv('W040_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW041 <- read.csv('W041_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW042 <- read.csv('W042_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW043 <- read.csv('W043_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW044 <- read.csv('W044_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW045 <- read.csv('W045_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW046 <- read.csv('W046_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW047 <- read.csv('W047_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW048 <- read.csv('W048_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW049 <- read.csv('W049_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW050 <- read.csv('W050_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW051 <- read.csv('W051_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

XrefW000y <- read.csv('W000y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW001y <- read.csv('W001y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW002y <- read.csv('W002y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW003y <- read.csv('W003y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW004y <- read.csv('W004y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW005y <- read.csv('W005y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW010y <- read.csv('W010y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefW015y <- read.csv('W015y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

XrefRTB1 <- read.csv('RT_B1_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefRTB3 <- read.csv('RT_B3_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefRTB6 <- read.csv('RT_B6_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefRTB8 <- read.csv('RT_B8_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

XrefRTB1y <- read.csv('RT_B1y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefRTB3y <- read.csv('RT_B3y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefRTB6y <- read.csv('RT_B6y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')
XrefRTB8y <- read.csv('RT_B8y_Arduino_100CB.csv', sep="," ,header = T,fileEncoding = 'UTF-8-BOM')

#################################
# Before each set of transfer experiments, the one-point calibration was performed using a mass M_ref
M_ref100 <- 99.63
# UM_ref is calculated as:
UM_ref <- 0.010/sqrt(12)

# The calibration data allow to validate the linear model (F_corrected=F_recorded*α),
# taking into account the existence of additional sources of uncertainty (drift, mechanical friction),
# named sigma_mechanical but not analysed. sigma_mechanical for the three calibration are expressed bellow:
sigma_mechanical1 <- 0.7465
sigma_mechanical2 <- 2.261
sigma_mechanical3 <- 1.997

### W000 ###
names(XrefW000) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW000 <- mean(XrefW000$load)
# Calculation of the SD
SD_XrefW000 <- sd(XrefW000$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW000 <- sqrt((SD_XrefW000^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW000 <- SD2_XrefW000/sqrt(nrow(XrefW000))
# calculation of α
α_XrefW000 <- M_ref100/Mean_XrefW000
# Calculation of the standard uncertainty linked to α
Uα_XrefW000 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW000/Mean_XrefW000)^2))

### W001 ###
names(XrefW001) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW001 <- mean(XrefW001$load)
# Calculation of the SD
SD_XrefW001 <- sd(XrefW001$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW001 <- sqrt((SD_XrefW001^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW001 <- SD2_XrefW001/sqrt(nrow(XrefW001))
# calculation of α
α_XrefW001 <- M_ref100/Mean_XrefW001
# Calculation of the standard uncertainty linked to α
Uα_XrefW001 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW001/Mean_XrefW001)^2))

### W002 ###
names(XrefW002) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW002 <- mean(XrefW002$load)
# Calculation of the SD
SD_XrefW002 <- sd(XrefW002$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW002 <- sqrt((SD_XrefW002^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW002 <- SD2_XrefW002/sqrt(nrow(XrefW002))
# calculation of α
α_XrefW002 <- M_ref100/Mean_XrefW002
# Calculation of the standard uncertainty linked to α
Uα_XrefW002 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW002/Mean_XrefW002)^2))

### W003 ###
names(XrefW003) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW003 <- mean(XrefW003$load)
# Calculation of the SD
SD_XrefW003 <- sd(XrefW003$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW003 <- sqrt((SD_XrefW003^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW003 <- SD2_XrefW003/sqrt(nrow(XrefW003))
# calculation of α
α_XrefW003 <- M_ref100/Mean_XrefW003
# Calculation of the standard uncertainty linked to α
Uα_XrefW003 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW003/Mean_XrefW003)^2))

### W004 ###
names(XrefW004) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW004 <- mean(XrefW004$load)
# Calculation of the SD
SD_XrefW004 <- sd(XrefW004$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW004 <- sqrt((SD_XrefW004^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW004 <- SD2_XrefW004/sqrt(nrow(XrefW004))
# calculation of α
α_XrefW004 <- M_ref100/Mean_XrefW004
# Calculation of the standard uncertainty linked to α
Uα_XrefW004 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW004/Mean_XrefW004)^2))

### W005 ###
names(XrefW005) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW005 <- mean(XrefW005$load)
# Calculation of the SD
SD_XrefW005 <- sd(XrefW005$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW005 <- sqrt((SD_XrefW005^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW005 <- SD2_XrefW005/sqrt(nrow(XrefW005))
# calculation of α
α_XrefW005 <- M_ref100/Mean_XrefW005
# Calculation of the standard uncertainty linked to α
Uα_XrefW005 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW005/Mean_XrefW005)^2))

### W006 ###
names(XrefW006) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW006 <- mean(XrefW006$load)
# Calculation of the SD
SD_XrefW006 <- sd(XrefW006$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW006 <- sqrt((SD_XrefW006^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW006 <- SD2_XrefW006/sqrt(nrow(XrefW006))
# calculation of α
α_XrefW006 <- M_ref100/Mean_XrefW006
# Calculation of the standard uncertainty linked to α
Uα_XrefW006 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW006/Mean_XrefW006)^2))

### W007 ###
names(XrefW007) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW007 <- mean(XrefW007$load)
# Calculation of the SD
SD_XrefW007 <- sd(XrefW007$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW007 <- sqrt((SD_XrefW007^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW007 <- SD2_XrefW007/sqrt(nrow(XrefW007))
# calculation of α
α_XrefW007 <- M_ref100/Mean_XrefW007
# Calculation of the standard uncertainty linked to α
Uα_XrefW007 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW007/Mean_XrefW007)^2))

### W008 ###
names(XrefW008) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW008 <- mean(XrefW008$load)
# Calculation of the SD
SD_XrefW008 <- sd(XrefW008$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW008 <- sqrt((SD_XrefW008^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW008 <- SD2_XrefW008/sqrt(nrow(XrefW008))
# calculation of α
α_XrefW008 <- M_ref100/Mean_XrefW008
# Calculation of the standard uncertainty linked to α
Uα_XrefW008 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW008/Mean_XrefW008)^2))

### W009 ###
names(XrefW009) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW009 <- mean(XrefW009$load)
# Calculation of the SD
SD_XrefW009 <- sd(XrefW009$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW009 <- sqrt((SD_XrefW009^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW009 <- SD2_XrefW009/sqrt(nrow(XrefW009))
# calculation of α
α_XrefW009 <- M_ref100/Mean_XrefW009
# Calculation of the standard uncertainty linked to α
Uα_XrefW009 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW009/Mean_XrefW009)^2))

### W010 ###
names(XrefW010) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW010 <- mean(XrefW010$load)
# Calculation of the SD
SD_XrefW010 <- sd(XrefW010$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW010 <- sqrt((SD_XrefW010^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW010 <- SD2_XrefW010/sqrt(nrow(XrefW010))
# calculation of α
α_XrefW010 <- M_ref100/Mean_XrefW010
# Calculation of the standard uncertainty linked to α
Uα_XrefW010 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW010/Mean_XrefW010)^2))

### W011 ###
names(XrefW011) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW011 <- mean(XrefW011$load)
# Calculation of the SD
SD_XrefW011 <- sd(XrefW011$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW011 <- sqrt((SD_XrefW011^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW011 <- SD2_XrefW011/sqrt(nrow(XrefW011))
# calculation of α
α_XrefW011 <- M_ref100/Mean_XrefW011
# Calculation of the standard uncertainty linked to α
Uα_XrefW011 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW011/Mean_XrefW011)^2))

### W012 ###
names(XrefW012) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW012 <- mean(XrefW012$load)
# Calculation of the SD
SD_XrefW012 <- sd(XrefW012$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW012 <- sqrt((SD_XrefW012^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW012 <- SD2_XrefW012/sqrt(nrow(XrefW012))
# calculation of α
α_XrefW012 <- M_ref100/Mean_XrefW012
# Calculation of the standard uncertainty linked to α
Uα_XrefW012 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW012/Mean_XrefW012)^2))

### W013 ###
names(XrefW013) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW013 <- mean(XrefW013$load)
# Calculation of the SD
SD_XrefW013 <- sd(XrefW013$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW013 <- sqrt((SD_XrefW013^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW013 <- SD2_XrefW013/sqrt(nrow(XrefW013))
# calculation of α
α_XrefW013 <- M_ref100/Mean_XrefW013
# Calculation of the standard uncertainty linked to α
Uα_XrefW013 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW013/Mean_XrefW013)^2))

### W014 ###
names(XrefW014) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW014 <- mean(XrefW014$load)
# Calculation of the SD
SD_XrefW014 <- sd(XrefW014$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW014 <- sqrt((SD_XrefW014^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW014 <- SD2_XrefW014/sqrt(nrow(XrefW014))
# calculation of α
α_XrefW014 <- M_ref100/Mean_XrefW014
# Calculation of the standard uncertainty linked to α
Uα_XrefW014 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW014/Mean_XrefW014)^2))

### W015 ###
names(XrefW015) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW015 <- mean(XrefW015$load)
# Calculation of the SD
SD_XrefW015 <- sd(XrefW015$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW015 <- sqrt((SD_XrefW015^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW015 <- SD2_XrefW015/sqrt(nrow(XrefW015))
# calculation of α
α_XrefW015 <- M_ref100/Mean_XrefW015
# Calculation of the standard uncertainty linked to α
Uα_XrefW015 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW015/Mean_XrefW015)^2))

### W016 ###
names(XrefW016) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW016 <- mean(XrefW016$load)
# Calculation of the SD
SD_XrefW016 <- sd(XrefW016$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW016 <- sqrt((SD_XrefW016^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW016 <- SD2_XrefW016/sqrt(nrow(XrefW016))
# calculation of α
α_XrefW016 <- M_ref100/Mean_XrefW016
# Calculation of the standard uncertainty linked to α
Uα_XrefW016 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW016/Mean_XrefW016)^2))

### W017 ###
names(XrefW017) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW017 <- mean(XrefW017$load)
# Calculation of the SD
SD_XrefW017 <- sd(XrefW017$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW017 <- sqrt((SD_XrefW017^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW017 <- SD2_XrefW017/sqrt(nrow(XrefW017))
# calculation of α
α_XrefW017 <- M_ref100/Mean_XrefW017
# Calculation of the standard uncertainty linked to α
Uα_XrefW017 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW017/Mean_XrefW017)^2))

### W018 ###
names(XrefW018) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW018 <- mean(XrefW018$load)
# Calculation of the SD
SD_XrefW018 <- sd(XrefW018$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW018 <- sqrt((SD_XrefW018^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW018 <- SD2_XrefW018/sqrt(nrow(XrefW018))
# calculation of α
α_XrefW018 <- M_ref100/Mean_XrefW018
# Calculation of the standard uncertainty linked to α
Uα_XrefW018 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW018/Mean_XrefW018)^2))

### W019 ###
names(XrefW019) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW019 <- mean(XrefW019$load)
# Calculation of the SD
SD_XrefW019 <- sd(XrefW019$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW019 <- sqrt((SD_XrefW019^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW019 <- SD2_XrefW019/sqrt(nrow(XrefW019))
# calculation of α
α_XrefW019 <- M_ref100/Mean_XrefW019
# Calculation of the standard uncertainty linked to α
Uα_XrefW019 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW019/Mean_XrefW019)^2))

### W020 ###
names(XrefW020) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW020 <- mean(XrefW020$load)
# Calculation of the SD
SD_XrefW020 <- sd(XrefW020$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW020 <- sqrt((SD_XrefW020^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW020 <- SD2_XrefW020/sqrt(nrow(XrefW020))
# calculation of α
α_XrefW020 <- M_ref100/Mean_XrefW020
# Calculation of the standard uncertainty linked to α
Uα_XrefW020 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW020/Mean_XrefW020)^2))

### W021 ###
names(XrefW021) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW021 <- mean(XrefW021$load)
# Calculation of the SD
SD_XrefW021 <- sd(XrefW021$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW021 <- sqrt((SD_XrefW021^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW021 <- SD2_XrefW021/sqrt(nrow(XrefW021))
# calculation of α
α_XrefW021 <- M_ref100/Mean_XrefW021
# Calculation of the standard uncertainty linked to α
Uα_XrefW021 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW021/Mean_XrefW021)^2))

### W022 ###
names(XrefW022) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW022 <- mean(XrefW022$load)
# Calculation of the SD
SD_XrefW022 <- sd(XrefW022$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW022 <- sqrt((SD_XrefW022^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW022 <- SD2_XrefW022/sqrt(nrow(XrefW022))
# calculation of α
α_XrefW022 <- M_ref100/Mean_XrefW022
# Calculation of the standard uncertainty linked to α
Uα_XrefW022 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW022/Mean_XrefW022)^2))

### W023 ###
names(XrefW023) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW023 <- mean(XrefW023$load)
# Calculation of the SD
SD_XrefW023 <- sd(XrefW023$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW023 <- sqrt((SD_XrefW023^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW023 <- SD2_XrefW023/sqrt(nrow(XrefW023))
# calculation of α
α_XrefW023 <- M_ref100/Mean_XrefW023
# Calculation of the standard uncertainty linked to α
Uα_XrefW023 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW023/Mean_XrefW023)^2))

### W024 ###
names(XrefW024) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW024 <- mean(XrefW024$load)
# Calculation of the SD
SD_XrefW024 <- sd(XrefW024$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW024 <- sqrt((SD_XrefW024^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW024 <- SD2_XrefW024/sqrt(nrow(XrefW024))
# calculation of α
α_XrefW024 <- M_ref100/Mean_XrefW024
# Calculation of the standard uncertainty linked to α
Uα_XrefW024 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW024/Mean_XrefW024)^2))

### W025 ###
names(XrefW025) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW025 <- mean(XrefW025$load)
# Calculation of the SD
SD_XrefW025 <- sd(XrefW025$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW025 <- sqrt((SD_XrefW025^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW025 <- SD2_XrefW025/sqrt(nrow(XrefW025))
# calculation of α
α_XrefW025 <- M_ref100/Mean_XrefW025
# Calculation of the standard uncertainty linked to α
Uα_XrefW025 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW025/Mean_XrefW025)^2))

### W026 ###
names(XrefW026) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW026 <- mean(XrefW026$load)
# Calculation of the SD
SD_XrefW026 <- sd(XrefW026$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW026 <- sqrt((SD_XrefW026^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW026 <- SD2_XrefW026/sqrt(nrow(XrefW026))
# calculation of α
α_XrefW026 <- M_ref100/Mean_XrefW026
# Calculation of the standard uncertainty linked to α
Uα_XrefW026 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW026/Mean_XrefW026)^2))

### W027 ###
names(XrefW027) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW027 <- mean(XrefW027$load)
# Calculation of the SD
SD_XrefW027 <- sd(XrefW027$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW027 <- sqrt((SD_XrefW027^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW027 <- SD2_XrefW027/sqrt(nrow(XrefW027))
# calculation of α
α_XrefW027 <- M_ref100/Mean_XrefW027
# Calculation of the standard uncertainty linked to α
Uα_XrefW027 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW027/Mean_XrefW027)^2))

### W028 ###
names(XrefW028) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW028 <- mean(XrefW028$load)
# Calculation of the SD
SD_XrefW028 <- sd(XrefW028$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW028 <- sqrt((SD_XrefW028^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW028 <- SD2_XrefW028/sqrt(nrow(XrefW028))
# calculation of α
α_XrefW028 <- M_ref100/Mean_XrefW028
# Calculation of the standard uncertainty linked to α
Uα_XrefW028 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW028/Mean_XrefW028)^2))

### W029 ###
names(XrefW029) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW029 <- mean(XrefW029$load)
# Calculation of the SD
SD_XrefW029 <- sd(XrefW029$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW029 <- sqrt((SD_XrefW029^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW029 <- SD2_XrefW029/sqrt(nrow(XrefW029))
# calculation of α
α_XrefW029 <- M_ref100/Mean_XrefW029
# Calculation of the standard uncertainty linked to α
Uα_XrefW029 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW029/Mean_XrefW029)^2))

### W030 ###
names(XrefW030) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW030 <- mean(XrefW030$load)
# Calculation of the SD
SD_XrefW030 <- sd(XrefW030$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW030 <- sqrt((SD_XrefW030^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW030 <- SD2_XrefW030/sqrt(nrow(XrefW030))
# calculation of α
α_XrefW030 <- M_ref100/Mean_XrefW030
# Calculation of the standard uncertainty linked to α
Uα_XrefW030 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW030/Mean_XrefW030)^2))

### W031 ###
names(XrefW031) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW031 <- mean(XrefW031$load)
# Calculation of the SD
SD_XrefW031 <- sd(XrefW031$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW031 <- sqrt((SD_XrefW031^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW031 <- SD2_XrefW031/sqrt(nrow(XrefW031))
# calculation of α
α_XrefW031 <- M_ref100/Mean_XrefW031
# Calculation of the standard uncertainty linked to α
Uα_XrefW031 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW031/Mean_XrefW031)^2))

### W032 ###
names(XrefW032) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW032 <- mean(XrefW032$load)
# Calculation of the SD
SD_XrefW032 <- sd(XrefW032$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW032 <- sqrt((SD_XrefW032^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW032 <- SD2_XrefW032/sqrt(nrow(XrefW032))
# calculation of α
α_XrefW032 <- M_ref100/Mean_XrefW032
# Calculation of the standard uncertainty linked to α
Uα_XrefW032 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW032/Mean_XrefW032)^2))

### W033 ###
names(XrefW033) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW033 <- mean(XrefW033$load)
# Calculation of the SD
SD_XrefW033 <- sd(XrefW033$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW033 <- sqrt((SD_XrefW033^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW033 <- SD2_XrefW033/sqrt(nrow(XrefW033))
# calculation of α
α_XrefW033 <- M_ref100/Mean_XrefW033
# Calculation of the standard uncertainty linked to α
Uα_XrefW033 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW033/Mean_XrefW033)^2))

### W034 ###
names(XrefW034) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW034 <- mean(XrefW034$load)
# Calculation of the SD
SD_XrefW034 <- sd(XrefW034$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW034 <- sqrt((SD_XrefW034^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW034 <- SD2_XrefW034/sqrt(nrow(XrefW034))
# calculation of α
α_XrefW034 <- M_ref100/Mean_XrefW034
# Calculation of the standard uncertainty linked to α
Uα_XrefW034 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW034/Mean_XrefW034)^2))

### W035 ###
names(XrefW035) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW035 <- mean(XrefW035$load)
# Calculation of the SD
SD_XrefW035 <- sd(XrefW035$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW035 <- sqrt((SD_XrefW035^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW035 <- SD2_XrefW035/sqrt(nrow(XrefW035))
# calculation of α
α_XrefW035 <- M_ref100/Mean_XrefW035
# Calculation of the standard uncertainty linked to α
Uα_XrefW035 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW035/Mean_XrefW035)^2))

### W036 ###
names(XrefW036) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW036 <- mean(XrefW036$load)
# Calculation of the SD
SD_XrefW036 <- sd(XrefW036$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW036 <- sqrt((SD_XrefW036^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW036 <- SD2_XrefW036/sqrt(nrow(XrefW036))
# calculation of α
α_XrefW036 <- M_ref100/Mean_XrefW036
# Calculation of the standard uncertainty linked to α
Uα_XrefW036 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW036/Mean_XrefW036)^2))

### W037 ###
names(XrefW037) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW037 <- mean(XrefW037$load)
# Calculation of the SD
SD_XrefW037 <- sd(XrefW037$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW037 <- sqrt((SD_XrefW037^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW037 <- SD2_XrefW037/sqrt(nrow(XrefW037))
# calculation of α
α_XrefW037 <- M_ref100/Mean_XrefW037
# Calculation of the standard uncertainty linked to α
Uα_XrefW037 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW037/Mean_XrefW037)^2))

### W038 ###
names(XrefW038) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW038 <- mean(XrefW038$load)
# Calculation of the SD
SD_XrefW038 <- sd(XrefW038$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW038 <- sqrt((SD_XrefW038^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW038 <- SD2_XrefW038/sqrt(nrow(XrefW038))
# calculation of α
α_XrefW038 <- M_ref100/Mean_XrefW038
# Calculation of the standard uncertainty linked to α
Uα_XrefW038 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW038/Mean_XrefW038)^2))

### W039 ###
names(XrefW039) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW039 <- mean(XrefW039$load)
# Calculation of the SD
SD_XrefW039 <- sd(XrefW039$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW039 <- sqrt((SD_XrefW039^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW039 <- SD2_XrefW039/sqrt(nrow(XrefW039))
# calculation of α
α_XrefW039 <- M_ref100/Mean_XrefW039
# Calculation of the standard uncertainty linked to α
Uα_XrefW039 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW039/Mean_XrefW039)^2))

### W040 ###
names(XrefW040) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW040 <- mean(XrefW040$load)
# Calculation of the SD
SD_XrefW040 <- sd(XrefW040$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW040 <- sqrt((SD_XrefW040^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW040 <- SD2_XrefW040/sqrt(nrow(XrefW040))
# calculation of α
α_XrefW040 <- M_ref100/Mean_XrefW040
# Calculation of the standard uncertainty linked to α
Uα_XrefW040 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW040/Mean_XrefW040)^2))

### W041 ###
names(XrefW041) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW041 <- mean(XrefW041$load)
# Calculation of the SD
SD_XrefW041 <- sd(XrefW041$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW041 <- sqrt((SD_XrefW041^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW041 <- SD2_XrefW041/sqrt(nrow(XrefW041))
# calculation of α
α_XrefW041 <- M_ref100/Mean_XrefW041
# Calculation of the standard uncertainty linked to α
Uα_XrefW041 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW041/Mean_XrefW041)^2))

### W042 ###
names(XrefW042) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW042 <- mean(XrefW042$load)
# Calculation of the SD
SD_XrefW042 <- sd(XrefW042$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW042 <- sqrt((SD_XrefW042^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW042 <- SD2_XrefW042/sqrt(nrow(XrefW042))
# calculation of α
α_XrefW042 <- M_ref100/Mean_XrefW042
# Calculation of the standard uncertainty linked to α
Uα_XrefW042 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW042/Mean_XrefW042)^2))

### W043 ###
names(XrefW043) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW043 <- mean(XrefW043$load)
# Calculation of the SD
SD_XrefW043 <- sd(XrefW043$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW043 <- sqrt((SD_XrefW043^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW043 <- SD2_XrefW043/sqrt(nrow(XrefW043))
# calculation of α
α_XrefW043 <- M_ref100/Mean_XrefW043
# Calculation of the standard uncertainty linked to α
Uα_XrefW043 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW043/Mean_XrefW043)^2))

### W044 ###
names(XrefW044) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW044 <- mean(XrefW044$load)
# Calculation of the SD
SD_XrefW044 <- sd(XrefW044$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW044 <- sqrt((SD_XrefW044^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW044 <- SD2_XrefW044/sqrt(nrow(XrefW044))
# calculation of α
α_XrefW044 <- M_ref100/Mean_XrefW044
# Calculation of the standard uncertainty linked to α
Uα_XrefW044 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW044/Mean_XrefW044)^2))

### W045 ###
names(XrefW045) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW045 <- mean(XrefW045$load)
# Calculation of the SD
SD_XrefW045 <- sd(XrefW045$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW045 <- sqrt((SD_XrefW045^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW045 <- SD2_XrefW045/sqrt(nrow(XrefW045))
# calculation of α
α_XrefW045 <- M_ref100/Mean_XrefW045
# Calculation of the standard uncertainty linked to α
Uα_XrefW045 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW045/Mean_XrefW045)^2))

### W046 ###
names(XrefW046) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW046 <- mean(XrefW046$load)
# Calculation of the SD
SD_XrefW046 <- sd(XrefW046$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW046 <- sqrt((SD_XrefW046^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW046 <- SD2_XrefW046/sqrt(nrow(XrefW046))
# calculation of α
α_XrefW046 <- M_ref100/Mean_XrefW046
# Calculation of the standard uncertainty linked to α
Uα_XrefW046 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW046/Mean_XrefW046)^2))

### W047 ###
names(XrefW047) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW047 <- mean(XrefW047$load)
# Calculation of the SD
SD_XrefW047 <- sd(XrefW047$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW047 <- sqrt((SD_XrefW047^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW047 <- SD2_XrefW047/sqrt(nrow(XrefW047))
# calculation of α
α_XrefW047 <- M_ref100/Mean_XrefW047
# Calculation of the standard uncertainty linked to α
Uα_XrefW047 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW047/Mean_XrefW047)^2))

### W048 ###
names(XrefW048) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW048 <- mean(XrefW048$load)
# Calculation of the SD
SD_XrefW048 <- sd(XrefW048$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW048 <- sqrt((SD_XrefW048^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW048 <- SD2_XrefW048/sqrt(nrow(XrefW048))
# calculation of α
α_XrefW048 <- M_ref100/Mean_XrefW048
# Calculation of the standard uncertainty linked to α
Uα_XrefW048 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW048/Mean_XrefW048)^2))

### W049 ###
names(XrefW049) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW049 <- mean(XrefW049$load)
# Calculation of the SD
SD_XrefW049 <- sd(XrefW049$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW049 <- sqrt((SD_XrefW049^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW049 <- SD2_XrefW049/sqrt(nrow(XrefW049))
# calculation of α
α_XrefW049 <- M_ref100/Mean_XrefW049
# Calculation of the standard uncertainty linked to α
Uα_XrefW049 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW049/Mean_XrefW049)^2))

### W050 ###
names(XrefW050) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW050 <- mean(XrefW050$load)
# Calculation of the SD
SD_XrefW050 <- sd(XrefW050$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW050 <- sqrt((SD_XrefW050^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW050 <- SD2_XrefW050/sqrt(nrow(XrefW050))
# calculation of α
α_XrefW050 <- M_ref100/Mean_XrefW050
# Calculation of the standard uncertainty linked to α
Uα_XrefW050 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW050/Mean_XrefW050)^2))

### W051 ###
names(XrefW051) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW051 <- mean(XrefW051$load)
# Calculation of the SD
SD_XrefW051 <- sd(XrefW051$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW051 <- sqrt((SD_XrefW051^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW051 <- SD2_XrefW051/sqrt(nrow(XrefW051))
# calculation of α
α_XrefW051 <- M_ref100/Mean_XrefW051
# Calculation of the standard uncertainty linked to α
Uα_XrefW051 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW051/Mean_XrefW051)^2))

### RTB1 ###
names(XrefRTB1) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefRTB1 <- mean(XrefRTB1$load)
# Calculation of the SD
SD_XrefRTB1 <- sd(XrefRTB1$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefRTB1 <- sqrt((SD_XrefRTB1^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refRTB1 <- SD2_XrefRTB1/sqrt(nrow(XrefRTB1))
# calculation of α
α_XrefRTB1 <- M_ref100/Mean_XrefRTB1
# Calculation of the standard uncertainty linked to α
Uα_XrefRTB1 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refRTB1/Mean_XrefRTB1)^2))

### RTB3 ###
names(XrefRTB3) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefRTB3 <- mean(XrefRTB3$load)
# Calculation of the SD
SD_XrefRTB3 <- sd(XrefRTB3$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefRTB3 <- sqrt((SD_XrefRTB3^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refRTB3 <- SD2_XrefRTB3/sqrt(nrow(XrefRTB3))
# calculation of α
α_XrefRTB3 <- M_ref100/Mean_XrefRTB3
# Calculation of the standard uncertainty linked to α
Uα_XrefRTB3 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refRTB3/Mean_XrefRTB3)^2))

### RTB6 ###
names(XrefRTB6) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefRTB6 <- mean(XrefRTB6$load)
# Calculation of the SD
SD_XrefRTB6 <- sd(XrefRTB6$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefRTB6 <- sqrt((SD_XrefRTB6^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refRTB6 <- SD2_XrefRTB6/sqrt(nrow(XrefRTB6))
# calculation of α
α_XrefRTB6 <- M_ref100/Mean_XrefRTB6
# Calculation of the standard uncertainty linked to α
Uα_XrefRTB6 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refRTB6/Mean_XrefRTB6)^2))

### RTB8 ###
names(XrefRTB8) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefRTB8 <- mean(XrefRTB8$load)
# Calculation of the SD
SD_XrefRTB8 <- sd(XrefRTB8$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefRTB8 <- sqrt((SD_XrefRTB8^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refRTB8 <- SD2_XrefRTB8/sqrt(nrow(XrefRTB8))
# calculation of α
α_XrefRTB8 <- M_ref100/Mean_XrefRTB8
# Calculation of the standard uncertainty linked to α
Uα_XrefRTB8 <- sqrt(((UM_ref/M_ref100)^2)+((UX_refRTB8/Mean_XrefRTB8)^2))

### RTB1y ###
names(XrefRTB1y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefRTB1y <- mean(XrefRTB1y$load)
# Calculation of the SD
SD_XrefRTB1y <- sd(XrefRTB1y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefRTB1y <- sqrt((SD_XrefRTB1y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refRTB1y <- SD2_XrefRTB1y/sqrt(nrow(XrefRTB1y))
# calculation of α
α_XrefRTB1y <- M_ref100/Mean_XrefRTB1y
# Calculation of the standard uncertainty linked to α
Uα_XrefRTB1y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refRTB1y/Mean_XrefRTB1y)^2))

### RTB3y ###
names(XrefRTB3y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefRTB3y <- mean(XrefRTB3y$load)
# Calculation of the SD
SD_XrefRTB3y <- sd(XrefRTB3y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefRTB3y <- sqrt((SD_XrefRTB3y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refRTB3y <- SD2_XrefRTB3y/sqrt(nrow(XrefRTB3y))
# calculation of α
α_XrefRTB3y <- M_ref100/Mean_XrefRTB3y
# Calculation of the standard uncertainty linked to α
Uα_XrefRTB3y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refRTB3y/Mean_XrefRTB3y)^2))

### RTB6y ###
names(XrefRTB6y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefRTB6y <- mean(XrefRTB6y$load)
# Calculation of the SD
SD_XrefRTB6y <- sd(XrefRTB6y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefRTB6y <- sqrt((SD_XrefRTB6y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refRTB6y <- SD2_XrefRTB6y/sqrt(nrow(XrefRTB6y))
# calculation of α
α_XrefRTB6y <- M_ref100/Mean_XrefRTB6y
# Calculation of the standard uncertainty linked to α
Uα_XrefRTB6y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refRTB6y/Mean_XrefRTB6y)^2))

### RTB8y ###
names(XrefRTB8y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefRTB8y <- mean(XrefRTB8y$load)
# Calculation of the SD
SD_XrefRTB8y <- sd(XrefRTB8y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefRTB8y <- sqrt((SD_XrefRTB8y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refRTB8y <- SD2_XrefRTB8y/sqrt(nrow(XrefRTB8y))
# calculation of α
α_XrefRTB8y <- M_ref100/Mean_XrefRTB8y
# Calculation of the standard uncertainty linked to α
Uα_XrefRTB8y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refRTB8y/Mean_XrefRTB8y)^2))

### W000y ###
names(XrefW000y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW000y <- mean(XrefW000y$load)
# Calculation of the SD
SD_XrefW000y <- sd(XrefW000y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW000y <- sqrt((SD_XrefW000y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW000y <- SD2_XrefW000y/sqrt(nrow(XrefW000y))
# calculation of α
α_XrefW000y <- M_ref100/Mean_XrefW000y
# Calculation of the standard uncertainty linked to α
Uα_XrefW000y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW000y/Mean_XrefW000y)^2))

### W001y ###
# 
names(XrefW001y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW001y <- mean(XrefW001y$load)
# Calculation of the SD
SD_XrefW001y <- sd(XrefW001y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW001y <- sqrt((SD_XrefW001y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW001y <- SD2_XrefW001y/sqrt(nrow(XrefW001y))
# calculation of α
α_XrefW001y <- M_ref100/Mean_XrefW001y
# Calculation of the standard uncertainty linked to α
Uα_XrefW001y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW001y/Mean_XrefW001y)^2))

### W002y ###
names(XrefW002y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW002y <- mean(XrefW002y$load)
# Calculation of the SD
SD_XrefW002y <- sd(XrefW002y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW002y <- sqrt((SD_XrefW002y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW002y <- SD2_XrefW002y/sqrt(nrow(XrefW002y))
# calculation of α
α_XrefW002y <- M_ref100/Mean_XrefW002y
# Calculation of the standard uncertainty linked to α
Uα_XrefW002y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW002y/Mean_XrefW002y)^2))

### W003yy ###
names(XrefW003y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW003y <- mean(XrefW003y$load)
# Calculation of the SD
SD_XrefW003y <- sd(XrefW003y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW003y <- sqrt((SD_XrefW003y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW003y <- SD2_XrefW003y/sqrt(nrow(XrefW003y))
# calculation of α
α_XrefW003y <- M_ref100/Mean_XrefW003y
# Calculation of the standard uncertainty linked to α
Uα_XrefW003y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW003y/Mean_XrefW003y)^2))

### W004y ###
names(XrefW004y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW004y <- mean(XrefW004y$load)
# Calculation of the SD
SD_XrefW004y <- sd(XrefW004y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW004y <- sqrt((SD_XrefW004y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW004y <- SD2_XrefW004y/sqrt(nrow(XrefW004y))
# calculation of α
α_XrefW004y <- M_ref100/Mean_XrefW004y
# Calculation of the standard uncertainty linked to α
Uα_XrefW004y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW004y/Mean_XrefW004y)^2))

### W005y ###
names(XrefW005y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW005y <- mean(XrefW005y$load)
# Calculation of the SD
SD_XrefW005y <- sd(XrefW005y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW005y <- sqrt((SD_XrefW005y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW005y <- SD2_XrefW005y/sqrt(nrow(XrefW005y))
# calculation of α
α_XrefW005y <- M_ref100/Mean_XrefW005y
# Calculation of the standard uncertainty linked to α
Uα_XrefW005y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW005y/Mean_XrefW005y)^2))

### W010y ###
names(XrefW010y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW010y <- mean(XrefW010y$load)
# Calculation of the SD
SD_XrefW010y <- sd(XrefW010y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW010y <- sqrt((SD_XrefW010y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW010y <- SD2_XrefW010y/sqrt(nrow(XrefW010y))
# calculation of α
α_XrefW010y <- M_ref100/Mean_XrefW010y
# Calculation of the standard uncertainty linked to α
Uα_XrefW010y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW010y/Mean_XrefW010y)^2))

### W015y ###
names(XrefW015y) <- c("analogVal","load","distance")
# Calculation of the mean
Mean_XrefW015y <- mean(XrefW015y$load)
# Calculation of the SD
SD_XrefW015y <- sd(XrefW015y$load)
# New Calculation of SD taking into account the additional sources of uncertainty
SD2_XrefW015y <- sqrt((SD_XrefW015y^2)+(sigma_mechanical1^2))
# Calculation of the standard uncertainty linked to Xref
UX_refW015y <- SD2_XrefW015y/sqrt(nrow(XrefW015y))
# calculation of α
α_XrefW015y <- M_ref100/Mean_XrefW015y
# Calculation of the standard uncertainty linked to α
Uα_XrefW015y <- sqrt(((UM_ref/M_ref100)^2)+((UX_refW015y/Mean_XrefW015y)^2))

##############################################
# Split StaticTotal per wash
# add column in StaticTotal with the calibration weight used in the experiments
StaticW000 <- StaticTotalfirstcalib %>% filter(grepl('W000_', Name))
StaticW001 <- StaticTotalfirstcalib %>% filter(grepl('W001_', Name))
StaticW002 <- StaticTotalfirstcalib %>% filter(grepl('W002_', Name))
StaticW003 <- StaticTotalfirstcalib %>% filter(grepl('W003_', Name))
StaticW004 <- StaticTotalfirstcalib %>% filter(grepl('W004_', Name))
StaticW005 <- StaticTotalfirstcalib %>% filter(grepl('W005_', Name))
StaticW006 <- StaticTotalfirstcalib %>% filter(grepl('W006_', Name))
StaticW007 <- StaticTotalfirstcalib %>% filter(grepl('W007_', Name))
StaticW008 <- StaticTotalfirstcalib %>% filter(grepl('W008_', Name))
StaticW009 <- StaticTotalfirstcalib %>% filter(grepl('W009_', Name))
StaticW010 <- StaticTotalfirstcalib %>% filter(grepl('W010_', Name))
StaticW011 <- StaticTotalfirstcalib %>% filter(grepl('W011_', Name))
StaticW012 <- StaticTotalfirstcalib %>% filter(grepl('W012_', Name))
StaticW013 <- StaticTotalfirstcalib %>% filter(grepl('W013_', Name))
StaticW014 <- StaticTotalfirstcalib %>% filter(grepl('W014_', Name))
StaticW015 <- StaticTotalfirstcalib %>% filter(grepl('W015_', Name))
StaticW016 <- StaticTotalfirstcalib %>% filter(grepl('W016_', Name))
StaticW017 <- StaticTotalfirstcalib %>% filter(grepl('W017_', Name))
StaticW018 <- StaticTotalfirstcalib %>% filter(grepl('W018_', Name))
StaticW019 <- StaticTotalfirstcalib %>% filter(grepl('W019_', Name))
StaticW020 <- StaticTotalfirstcalib %>% filter(grepl('W020_', Name))
StaticW021 <- StaticTotalfirstcalib %>% filter(grepl('W021_', Name))
StaticW022 <- StaticTotalfirstcalib %>% filter(grepl('W022_', Name))
StaticW023 <- StaticTotalfirstcalib %>% filter(grepl('W023_', Name))
StaticW024 <- StaticTotalfirstcalib %>% filter(grepl('W024', Name))
StaticW025 <- StaticTotalfirstcalib %>% filter(grepl('W025', Name))
StaticW026 <- StaticTotalfirstcalib %>% filter(grepl('W026', Name))
StaticW027 <- StaticTotalfirstcalib %>% filter(grepl('W027', Name))
StaticW028 <- StaticTotalfirstcalib %>% filter(grepl('W028', Name))
StaticW029 <- StaticTotalfirstcalib %>% filter(grepl('W029', Name))
StaticW030 <- StaticTotalfirstcalib %>% filter(grepl('W030', Name))
StaticW031 <- StaticTotalfirstcalib %>% filter(grepl('W031', Name))
StaticW032 <- StaticTotalfirstcalib %>% filter(grepl('W032', Name))
StaticW033 <- StaticTotalfirstcalib %>% filter(grepl('W033', Name))
StaticW034 <- StaticTotalfirstcalib %>% filter(grepl('W034', Name))
StaticW035 <- StaticTotalfirstcalib %>% filter(grepl('W035', Name))
StaticW036 <- StaticTotalfirstcalib %>% filter(grepl('W036', Name))
StaticW037 <- StaticTotalfirstcalib %>% filter(grepl('W037', Name))
StaticW038 <- StaticTotalfirstcalib %>% filter(grepl('W038', Name))
StaticW039 <- StaticTotalfirstcalib %>% filter(grepl('W039', Name))
StaticW040 <- StaticTotalfirstcalib %>% filter(grepl('W040', Name))
StaticW041 <- StaticTotalfirstcalib %>% filter(grepl('W041', Name))
StaticW042 <- StaticTotalfirstcalib %>% filter(grepl('W042', Name))
StaticW043 <- StaticTotalfirstcalib %>% filter(grepl('W043', Name))
StaticW044 <- StaticTotalfirstcalib %>% filter(grepl('W044', Name))
StaticW045 <- StaticTotalfirstcalib %>% filter(grepl('W045', Name))
StaticW046 <- StaticTotalfirstcalib %>% filter(grepl('W046', Name))
StaticW047 <- StaticTotalfirstcalib %>% filter(grepl('W047', Name))
StaticW048 <- StaticTotalfirstcalib %>% filter(grepl('W048', Name))
StaticW049 <- StaticTotalfirstcalib %>% filter(grepl('W049', Name))
StaticW050 <- StaticTotalfirstcalib %>% filter(grepl('W050', Name))
StaticW051 <- StaticTotalfirstcalib %>% filter(grepl('W051', Name))

StaticW000y <- StaticTotalthirdcalib %>% filter(grepl('W000y_', Name))
StaticW001y <- StaticTotalthirdcalib %>% filter(grepl('W001y_', Name))
StaticW002y <- StaticTotalthirdcalib %>% filter(grepl('W002y_', Name))
StaticW003y <- StaticTotalthirdcalib %>% filter(grepl('W003y_', Name))
StaticW004y <- StaticTotalthirdcalib %>% filter(grepl('W004y_', Name))
StaticW005y <- StaticTotalthirdcalib %>% filter(grepl('W005y_', Name))
StaticW010y <- StaticTotalthirdcalib %>% filter(grepl('W010y_', Name))
StaticW015y <- StaticTotalthirdcalib %>% filter(grepl('W015y_', Name))

StaticRTB1 <- StaticTotalsecondcalib %>% filter(grepl('RT_B1_', Name))
StaticRTB3 <- StaticTotalsecondcalib %>% filter(grepl('RT_B3_', Name))
StaticRTB6 <- StaticTotalsecondcalib %>% filter(grepl('RT_B6_', Name))
StaticRTB8 <- StaticTotalsecondcalib %>% filter(grepl('RT_B8_', Name))

StaticRTB1y <- StaticTotalthirdcalib %>% filter(grepl('RT_B1y_', Name))
StaticRTB3y <- StaticTotalthirdcalib %>% filter(grepl('RT_B3Y_', Name))
StaticRTB6y <- StaticTotalthirdcalib %>% filter(grepl('RT_B6Y', Name))
StaticRTB8y <- StaticTotalthirdcalib %>% filter(grepl('RT_B8Y', Name))

####################################################
# Correction of Fstat based on the calibration Fstat_corrected = Fstat*α
StaticW000$Fstat_corrected <- StaticW000$Fstat*α_XrefW000
StaticW001$Fstat_corrected <- StaticW001$Fstat*α_XrefW001
StaticW002$Fstat_corrected <- StaticW002$Fstat*α_XrefW002
StaticW003$Fstat_corrected <- StaticW003$Fstat*α_XrefW003
StaticW004$Fstat_corrected <- StaticW004$Fstat*α_XrefW004
StaticW005$Fstat_corrected <- StaticW005$Fstat*α_XrefW005
StaticW006$Fstat_corrected <- StaticW006$Fstat*α_XrefW006
StaticW007$Fstat_corrected <- StaticW007$Fstat*α_XrefW007
StaticW008$Fstat_corrected <- StaticW008$Fstat*α_XrefW008
StaticW009$Fstat_corrected <- StaticW009$Fstat*α_XrefW009
StaticW010$Fstat_corrected <- StaticW010$Fstat*α_XrefW010
StaticW011$Fstat_corrected <- StaticW011$Fstat*α_XrefW011
StaticW012$Fstat_corrected <- StaticW012$Fstat*α_XrefW012
StaticW013$Fstat_corrected <- StaticW013$Fstat*α_XrefW013
StaticW014$Fstat_corrected <- StaticW014$Fstat*α_XrefW014
StaticW015$Fstat_corrected <- StaticW015$Fstat*α_XrefW015
StaticW016$Fstat_corrected <- StaticW016$Fstat*α_XrefW016
StaticW017$Fstat_corrected <- StaticW017$Fstat*α_XrefW017
StaticW018$Fstat_corrected <- StaticW018$Fstat*α_XrefW018
StaticW019$Fstat_corrected <- StaticW019$Fstat*α_XrefW019
StaticW020$Fstat_corrected <- StaticW020$Fstat*α_XrefW020
StaticW021$Fstat_corrected <- StaticW021$Fstat*α_XrefW021
StaticW022$Fstat_corrected <- StaticW022$Fstat*α_XrefW022
StaticW023$Fstat_corrected <- StaticW023$Fstat*α_XrefW023
StaticW024$Fstat_corrected <- StaticW024$Fstat*α_XrefW024
StaticW025$Fstat_corrected <- StaticW025$Fstat*α_XrefW025
StaticW026$Fstat_corrected <- StaticW026$Fstat*α_XrefW026
StaticW027$Fstat_corrected <- StaticW027$Fstat*α_XrefW027
StaticW028$Fstat_corrected <- StaticW028$Fstat*α_XrefW028
StaticW029$Fstat_corrected <- StaticW029$Fstat*α_XrefW029
StaticW030$Fstat_corrected <- StaticW030$Fstat*α_XrefW030
StaticW031$Fstat_corrected <- StaticW031$Fstat*α_XrefW031
StaticW032$Fstat_corrected <- StaticW032$Fstat*α_XrefW032
StaticW033$Fstat_corrected <- StaticW033$Fstat*α_XrefW033
StaticW034$Fstat_corrected <- StaticW034$Fstat*α_XrefW034
StaticW035$Fstat_corrected <- StaticW035$Fstat*α_XrefW035
StaticW036$Fstat_corrected <- StaticW036$Fstat*α_XrefW036
StaticW037$Fstat_corrected <- StaticW037$Fstat*α_XrefW037
StaticW038$Fstat_corrected <- StaticW038$Fstat*α_XrefW038
StaticW039$Fstat_corrected <- StaticW039$Fstat*α_XrefW039
StaticW040$Fstat_corrected <- StaticW040$Fstat*α_XrefW040
StaticW041$Fstat_corrected <- StaticW041$Fstat*α_XrefW041
StaticW042$Fstat_corrected <- StaticW042$Fstat*α_XrefW042
StaticW043$Fstat_corrected <- StaticW043$Fstat*α_XrefW043
StaticW044$Fstat_corrected <- StaticW044$Fstat*α_XrefW044
StaticW045$Fstat_corrected <- StaticW045$Fstat*α_XrefW045
StaticW046$Fstat_corrected <- StaticW046$Fstat*α_XrefW046
StaticW047$Fstat_corrected <- StaticW047$Fstat*α_XrefW047
StaticW048$Fstat_corrected <- StaticW048$Fstat*α_XrefW048
StaticW049$Fstat_corrected <- StaticW049$Fstat*α_XrefW049
StaticW000$Fstat_corrected <- StaticW000$Fstat*α_XrefW000
StaticW050$Fstat_corrected <- StaticW050$Fstat*α_XrefW050
StaticW051$Fstat_corrected <- StaticW051$Fstat*α_XrefW051

StaticW000y$Fstat_corrected <- StaticW000y$Fstat*α_XrefW000y
StaticW001y$Fstat_corrected <- StaticW001y$Fstat*α_XrefW001y
StaticW002y$Fstat_corrected <- StaticW002y$Fstat*α_XrefW002y
StaticW003y$Fstat_corrected <- StaticW003y$Fstat*α_XrefW003y
StaticW004y$Fstat_corrected <- StaticW004y$Fstat*α_XrefW004y
StaticW005y$Fstat_corrected <- StaticW005y$Fstat*α_XrefW005y
StaticW010y$Fstat_corrected <- StaticW010y$Fstat*α_XrefW010y
StaticW015y$Fstat_corrected <- StaticW015y$Fstat*α_XrefW015y

StaticRTB1$Fstat_corrected <- StaticRTB1$Fstat*α_XrefRTB1
StaticRTB3$Fstat_corrected <- StaticRTB3$Fstat*α_XrefRTB3
StaticRTB6$Fstat_corrected <- StaticRTB6$Fstat*α_XrefRTB6
StaticRTB8$Fstat_corrected <- StaticRTB8$Fstat*α_XrefRTB8

StaticRTB1y$Fstat_corrected <- StaticRTB1y$Fstat*α_XrefRTB1y
StaticRTB3y$Fstat_corrected <- StaticRTB3y$Fstat*α_XrefRTB3y
StaticRTB6y$Fstat_corrected <- StaticRTB6y$Fstat*α_XrefRTB6y
StaticRTB8y$Fstat_corrected <- StaticRTB8y$Fstat*α_XrefRTB8y

####################################################
# Add UFstat_corrected in all dataframe
StaticW000$UFstat_corrected <- StaticW000$Fstat_corrected*(sqrt(((UX_refW000/StaticW000$Fstat)^2)+((Uα_XrefW000/α_XrefW000)^2)))
StaticW001$UFstat_corrected <- StaticW001$Fstat_corrected*(sqrt(((UX_refW001/StaticW001$Fstat)^2)+((Uα_XrefW001/α_XrefW001)^2)))
StaticW002$UFstat_corrected <- StaticW002$Fstat_corrected*(sqrt(((UX_refW002/StaticW002$Fstat)^2)+((Uα_XrefW002/α_XrefW002)^2)))
StaticW003$UFstat_corrected <- StaticW003$Fstat_corrected*(sqrt(((UX_refW003/StaticW003$Fstat)^2)+((Uα_XrefW003/α_XrefW003)^2)))
StaticW004$UFstat_corrected <- StaticW004$Fstat_corrected*(sqrt(((UX_refW004/StaticW004$Fstat)^2)+((Uα_XrefW004/α_XrefW004)^2)))
StaticW005$UFstat_corrected <- StaticW005$Fstat_corrected*(sqrt(((UX_refW005/StaticW005$Fstat)^2)+((Uα_XrefW005/α_XrefW005)^2)))
StaticW006$UFstat_corrected <- StaticW006$Fstat_corrected*(sqrt(((UX_refW006/StaticW006$Fstat)^2)+((Uα_XrefW006/α_XrefW006)^2)))
StaticW007$UFstat_corrected <- StaticW007$Fstat_corrected*(sqrt(((UX_refW007/StaticW007$Fstat)^2)+((Uα_XrefW007/α_XrefW007)^2)))
StaticW008$UFstat_corrected <- StaticW008$Fstat_corrected*(sqrt(((UX_refW008/StaticW008$Fstat)^2)+((Uα_XrefW008/α_XrefW008)^2)))
StaticW009$UFstat_corrected <- StaticW009$Fstat_corrected*(sqrt(((UX_refW009/StaticW009$Fstat)^2)+((Uα_XrefW009/α_XrefW009)^2)))
StaticW010$UFstat_corrected <- StaticW010$Fstat_corrected*(sqrt(((UX_refW010/StaticW010$Fstat)^2)+((Uα_XrefW010/α_XrefW010)^2)))
StaticW011$UFstat_corrected <- StaticW011$Fstat_corrected*(sqrt(((UX_refW011/StaticW011$Fstat)^2)+((Uα_XrefW011/α_XrefW011)^2)))
StaticW012$UFstat_corrected <- StaticW012$Fstat_corrected*(sqrt(((UX_refW012/StaticW012$Fstat)^2)+((Uα_XrefW012/α_XrefW012)^2)))
StaticW013$UFstat_corrected <- StaticW013$Fstat_corrected*(sqrt(((UX_refW013/StaticW013$Fstat)^2)+((Uα_XrefW013/α_XrefW013)^2)))
StaticW014$UFstat_corrected <- StaticW014$Fstat_corrected*(sqrt(((UX_refW014/StaticW014$Fstat)^2)+((Uα_XrefW014/α_XrefW014)^2)))
StaticW015$UFstat_corrected <- StaticW015$Fstat_corrected*(sqrt(((UX_refW015/StaticW015$Fstat)^2)+((Uα_XrefW015/α_XrefW015)^2)))
StaticW016$UFstat_corrected <- StaticW016$Fstat_corrected*(sqrt(((UX_refW016/StaticW016$Fstat)^2)+((Uα_XrefW016/α_XrefW016)^2)))
StaticW017$UFstat_corrected <- StaticW017$Fstat_corrected*(sqrt(((UX_refW017/StaticW017$Fstat)^2)+((Uα_XrefW017/α_XrefW017)^2)))
StaticW018$UFstat_corrected <- StaticW018$Fstat_corrected*(sqrt(((UX_refW018/StaticW018$Fstat)^2)+((Uα_XrefW018/α_XrefW018)^2)))
StaticW019$UFstat_corrected <- StaticW019$Fstat_corrected*(sqrt(((UX_refW019/StaticW019$Fstat)^2)+((Uα_XrefW019/α_XrefW019)^2)))
StaticW020$UFstat_corrected <- StaticW020$Fstat_corrected*(sqrt(((UX_refW020/StaticW020$Fstat)^2)+((Uα_XrefW020/α_XrefW020)^2)))
StaticW021$UFstat_corrected <- StaticW021$Fstat_corrected*(sqrt(((UX_refW021/StaticW021$Fstat)^2)+((Uα_XrefW021/α_XrefW021)^2)))
StaticW022$UFstat_corrected <- StaticW022$Fstat_corrected*(sqrt(((UX_refW022/StaticW022$Fstat)^2)+((Uα_XrefW022/α_XrefW022)^2)))
StaticW023$UFstat_corrected <- StaticW023$Fstat_corrected*(sqrt(((UX_refW023/StaticW023$Fstat)^2)+((Uα_XrefW023/α_XrefW023)^2)))
StaticW024$UFstat_corrected <- StaticW024$Fstat_corrected*(sqrt(((UX_refW024/StaticW024$Fstat)^2)+((Uα_XrefW024/α_XrefW024)^2)))
StaticW025$UFstat_corrected <- StaticW025$Fstat_corrected*(sqrt(((UX_refW025/StaticW025$Fstat)^2)+((Uα_XrefW025/α_XrefW025)^2)))
StaticW026$UFstat_corrected <- StaticW026$Fstat_corrected*(sqrt(((UX_refW026/StaticW026$Fstat)^2)+((Uα_XrefW026/α_XrefW026)^2)))
StaticW027$UFstat_corrected <- StaticW027$Fstat_corrected*(sqrt(((UX_refW027/StaticW027$Fstat)^2)+((Uα_XrefW027/α_XrefW027)^2)))
StaticW028$UFstat_corrected <- StaticW028$Fstat_corrected*(sqrt(((UX_refW028/StaticW028$Fstat)^2)+((Uα_XrefW028/α_XrefW028)^2)))
StaticW029$UFstat_corrected <- StaticW029$Fstat_corrected*(sqrt(((UX_refW029/StaticW029$Fstat)^2)+((Uα_XrefW029/α_XrefW029)^2)))
StaticW030$UFstat_corrected <- StaticW030$Fstat_corrected*(sqrt(((UX_refW030/StaticW030$Fstat)^2)+((Uα_XrefW030/α_XrefW030)^2)))
StaticW031$UFstat_corrected <- StaticW031$Fstat_corrected*(sqrt(((UX_refW031/StaticW031$Fstat)^2)+((Uα_XrefW031/α_XrefW031)^2)))
StaticW032$UFstat_corrected <- StaticW032$Fstat_corrected*(sqrt(((UX_refW032/StaticW032$Fstat)^2)+((Uα_XrefW032/α_XrefW032)^2)))
StaticW033$UFstat_corrected <- StaticW033$Fstat_corrected*(sqrt(((UX_refW033/StaticW033$Fstat)^2)+((Uα_XrefW033/α_XrefW033)^2)))
StaticW034$UFstat_corrected <- StaticW034$Fstat_corrected*(sqrt(((UX_refW034/StaticW034$Fstat)^2)+((Uα_XrefW034/α_XrefW034)^2)))
StaticW035$UFstat_corrected <- StaticW035$Fstat_corrected*(sqrt(((UX_refW035/StaticW035$Fstat)^2)+((Uα_XrefW035/α_XrefW035)^2)))
StaticW036$UFstat_corrected <- StaticW036$Fstat_corrected*(sqrt(((UX_refW036/StaticW036$Fstat)^2)+((Uα_XrefW036/α_XrefW036)^2)))
StaticW037$UFstat_corrected <- StaticW037$Fstat_corrected*(sqrt(((UX_refW037/StaticW037$Fstat)^2)+((Uα_XrefW037/α_XrefW037)^2)))
StaticW038$UFstat_corrected <- StaticW038$Fstat_corrected*(sqrt(((UX_refW038/StaticW038$Fstat)^2)+((Uα_XrefW038/α_XrefW038)^2)))
StaticW039$UFstat_corrected <- StaticW039$Fstat_corrected*(sqrt(((UX_refW039/StaticW039$Fstat)^2)+((Uα_XrefW039/α_XrefW039)^2)))
StaticW040$UFstat_corrected <- StaticW040$Fstat_corrected*(sqrt(((UX_refW040/StaticW040$Fstat)^2)+((Uα_XrefW040/α_XrefW040)^2)))
StaticW041$UFstat_corrected <- StaticW041$Fstat_corrected*(sqrt(((UX_refW041/StaticW041$Fstat)^2)+((Uα_XrefW041/α_XrefW041)^2)))
StaticW042$UFstat_corrected <- StaticW042$Fstat_corrected*(sqrt(((UX_refW042/StaticW042$Fstat)^2)+((Uα_XrefW042/α_XrefW042)^2)))
StaticW043$UFstat_corrected <- StaticW043$Fstat_corrected*(sqrt(((UX_refW043/StaticW043$Fstat)^2)+((Uα_XrefW043/α_XrefW043)^2)))
StaticW044$UFstat_corrected <- StaticW044$Fstat_corrected*(sqrt(((UX_refW044/StaticW044$Fstat)^2)+((Uα_XrefW044/α_XrefW044)^2)))
StaticW045$UFstat_corrected <- StaticW045$Fstat_corrected*(sqrt(((UX_refW045/StaticW045$Fstat)^2)+((Uα_XrefW045/α_XrefW045)^2)))
StaticW046$UFstat_corrected <- StaticW046$Fstat_corrected*(sqrt(((UX_refW046/StaticW046$Fstat)^2)+((Uα_XrefW046/α_XrefW046)^2)))
StaticW047$UFstat_corrected <- StaticW047$Fstat_corrected*(sqrt(((UX_refW047/StaticW047$Fstat)^2)+((Uα_XrefW047/α_XrefW047)^2)))
StaticW048$UFstat_corrected <- StaticW048$Fstat_corrected*(sqrt(((UX_refW048/StaticW048$Fstat)^2)+((Uα_XrefW048/α_XrefW048)^2)))
StaticW049$UFstat_corrected <- StaticW049$Fstat_corrected*(sqrt(((UX_refW049/StaticW049$Fstat)^2)+((Uα_XrefW049/α_XrefW049)^2)))
StaticW050$UFstat_corrected <- StaticW050$Fstat_corrected*(sqrt(((UX_refW050/StaticW050$Fstat)^2)+((Uα_XrefW050/α_XrefW050)^2)))
StaticW051$UFstat_corrected <- StaticW051$Fstat_corrected*(sqrt(((UX_refW051/StaticW051$Fstat)^2)+((Uα_XrefW051/α_XrefW051)^2)))

StaticW000y$UFstat_corrected <- StaticW000y$Fstat_corrected*(sqrt(((UX_refW000y/StaticW000y$Fstat)^2)+((Uα_XrefW000y/α_XrefW000y)^2)))
StaticW001y$UFstat_corrected <- StaticW001y$Fstat_corrected*(sqrt(((UX_refW001y/StaticW001y$Fstat)^2)+((Uα_XrefW001y/α_XrefW001y)^2)))
StaticW002y$UFstat_corrected <- StaticW002y$Fstat_corrected*(sqrt(((UX_refW002y/StaticW002y$Fstat)^2)+((Uα_XrefW002y/α_XrefW002y)^2)))
StaticW003y$UFstat_corrected <- StaticW003y$Fstat_corrected*(sqrt(((UX_refW003y/StaticW003y$Fstat)^2)+((Uα_XrefW003y/α_XrefW003y)^2)))
StaticW004y$UFstat_corrected <- StaticW004y$Fstat_corrected*(sqrt(((UX_refW004y/StaticW004y$Fstat)^2)+((Uα_XrefW004y/α_XrefW004y)^2)))
StaticW005y$UFstat_corrected <- StaticW005y$Fstat_corrected*(sqrt(((UX_refW005y/StaticW005y$Fstat)^2)+((Uα_XrefW005y/α_XrefW005y)^2)))
StaticW010y$UFstat_corrected <- StaticW010y$Fstat_corrected*(sqrt(((UX_refW010y/StaticW010y$Fstat)^2)+((Uα_XrefW010y/α_XrefW010y)^2)))
StaticW015y$UFstat_corrected <- StaticW015y$Fstat_corrected*(sqrt(((UX_refW015y/StaticW015y$Fstat)^2)+((Uα_XrefW015y/α_XrefW015y)^2)))

StaticRTB1$UFstat_corrected <- StaticRTB1$Fstat_corrected*(sqrt(((UX_refRTB1/StaticRTB1$Fstat)^2)+((Uα_XrefRTB1/α_XrefRTB1)^2)))
StaticRTB3$UFstat_corrected <- StaticRTB3$Fstat_corrected*(sqrt(((UX_refRTB3/StaticRTB3$Fstat)^2)+((Uα_XrefRTB3/α_XrefRTB3)^2)))
StaticRTB6$UFstat_corrected <- StaticRTB6$Fstat_corrected*(sqrt(((UX_refRTB6/StaticRTB6$Fstat)^2)+((Uα_XrefRTB6/α_XrefRTB6)^2)))
StaticRTB8$UFstat_corrected <- StaticRTB8$Fstat_corrected*(sqrt(((UX_refRTB8/StaticRTB8$Fstat)^2)+((Uα_XrefRTB8/α_XrefRTB8)^2)))
StaticRTB1y$UFstat_corrected <- StaticRTB1y$Fstat_corrected*(sqrt(((UX_refRTB1y/StaticRTB1y$Fstat)^2)+((Uα_XrefRTB1y/α_XrefRTB1y)^2)))
StaticRTB3y$UFstat_corrected <- StaticRTB3y$Fstat_corrected*(sqrt(((UX_refRTB3y/StaticRTB3y$Fstat)^2)+((Uα_XrefRTB3y/α_XrefRTB3y)^2)))
StaticRTB6y$UFstat_corrected <- StaticRTB6y$Fstat_corrected*(sqrt(((UX_refRTB6y/StaticRTB6y$Fstat)^2)+((Uα_XrefRTB6y/α_XrefRTB6y)^2)))
StaticRTB8y$UFstat_corrected <- StaticRTB8y$Fstat_corrected*(sqrt(((UX_refRTB8y/StaticRTB8y$Fstat)^2)+((Uα_XrefRTB8y/α_XrefRTB8y)^2)))

####################################################################
Totalwashred <- rbind(StaticW000,StaticW001,StaticW002,StaticW003,StaticW004,StaticW005,StaticW006,StaticW007,StaticW008,StaticW008,StaticW009,
             StaticW010,StaticW011,StaticW012,StaticW013,StaticW014,StaticW015,StaticW016,StaticW017,StaticW018,StaticW018,StaticW019,
             StaticW020,StaticW021,StaticW022,StaticW023,StaticW024,StaticW025,StaticW026,StaticW027,StaticW028,StaticW028,StaticW029,
             StaticW030,StaticW031,StaticW032,StaticW033,StaticW034,StaticW035,StaticW036,StaticW037,StaticW038,StaticW038,StaticW039,
             StaticW040,StaticW041,StaticW042,StaticW043,StaticW044,StaticW045,StaticW046,StaticW047,StaticW048,StaticW048,StaticW049,
             StaticW050,StaticW051)
TotRTred <- rbind(StaticRTB1,StaticRTB3,StaticRTB6,StaticRTB8)

Totalwashyellow <- rbind(StaticW000y,StaticW001y,StaticW002y,StaticW003y,StaticW004y,StaticW005y,StaticW010y,StaticW015y)

TotRTyellow <- rbind(StaticRTB1y,StaticRTB3y,StaticRTB6y,StaticRTB8y)

#### data for figure
StaticB1G1 <- Totalwashred %>% filter(grepl('_G1_B1_', Name))
StaticB1G1 <- StaticB1G1 %>% distinct()
StaticB2G1 <- Totalwashred %>% filter(grepl('_G1_B2_', Name))
StaticB2G1 <- StaticB2G1 %>% distinct()
StaticB3G1 <- Totalwashred %>% filter(grepl('_G1_B3_', Name))
StaticB3G1 <- StaticB3G1 %>% distinct()
StaticB4G1 <- Totalwashred %>% filter(grepl('_G1_B4_', Name))
StaticB4G1 <- StaticB4G1 %>% distinct()
StaticB5G1 <- Totalwashred %>% filter(grepl('_G1_B5_', Name))
StaticB5G1 <- StaticB5G1 %>% distinct()
StaticB6G1 <- Totalwashred %>% filter(grepl('_G1_B6_', Name))
StaticB6G1 <- StaticB6G1 %>% distinct()
StaticB7G1 <- Totalwashred %>% filter(grepl('_G1_B7_', Name))
StaticB7G1 <- StaticB7G1 %>% distinct()
StaticB8G1 <- Totalwashred %>% filter(grepl('_G1_B8_', Name))
StaticB8G1 <- StaticB8G1 %>% distinct()
StaticB9G1 <- Totalwashred %>% filter(grepl('_G1_B9_', Name))
StaticB9G1 <- StaticB9G1 %>% distinct()
StaticB10G1 <- Totalwashred %>% filter(grepl('_G1_B10_', Name))
StaticB10G1 <- StaticB10G1 %>% distinct()

StaticB1G2 <- Totalwashred %>% filter(grepl('_G2_B1_', Name))
StaticB1G2 <- StaticB1G2 %>% distinct()
StaticB2G2 <- Totalwashred %>% filter(grepl('_G2_B2_', Name))
StaticB2G2 <- StaticB2G2 %>% distinct()
StaticB3G2 <- Totalwashred %>% filter(grepl('_G2_B3_', Name))
StaticB3G2 <- StaticB3G2 %>% distinct()
StaticB4G2 <- Totalwashred %>% filter(grepl('_G2_B4_', Name))
StaticB4G2 <- StaticB4G2 %>% distinct()
StaticB5G2 <- Totalwashred %>% filter(grepl('_G2_B5_', Name))
StaticB5G2 <- StaticB5G2 %>% distinct()
StaticB6G2 <- Totalwashred %>% filter(grepl('_G2_B6_', Name))
StaticB6G2 <- StaticB6G2 %>% distinct()
StaticB7G2 <- Totalwashred %>% filter(grepl('_G2_B7_', Name))
StaticB7G2 <- StaticB7G2 %>% distinct()
StaticB8G2 <- Totalwashred %>% filter(grepl('_G2_B8_', Name))
StaticB8G2 <- StaticB8G2 %>% distinct()
StaticB9G2 <- Totalwashred %>% filter(grepl('_G2_B9_', Name))
StaticB9G2 <- StaticB9G2 %>% distinct()
StaticB10G2 <- Totalwashred %>% filter(grepl('_G2_B10_', Name))
StaticB10G2 <- StaticB10G2 %>% distinct()

StaticB1G3 <- Totalwashred %>% filter(grepl('_G3_B1_', Name))
StaticB1G3 <- StaticB1G3 %>% distinct()
StaticB2G3 <- Totalwashred %>% filter(grepl('_G3_B2_', Name))
StaticB2G3 <- StaticB2G3 %>% distinct()
StaticB3G3 <- Totalwashred %>% filter(grepl('_G3_B3_', Name))
StaticB3G3 <- StaticB3G3 %>% distinct()
StaticB4G3 <- Totalwashred %>% filter(grepl('_G3_B4_', Name))
StaticB4G3 <- StaticB4G3 %>% distinct()
StaticB5G3 <- Totalwashred %>% filter(grepl('_G3_B5_', Name))
StaticB5G3 <- StaticB5G3 %>% distinct()
StaticB6G3 <- Totalwashred %>% filter(grepl('_G3_B6_', Name))
StaticB6G3 <- StaticB6G3 %>% distinct()
StaticB7G3 <- Totalwashred %>% filter(grepl('_G3_B7_', Name))
StaticB7G3 <- StaticB7G3 %>% distinct()
StaticB8G3 <- Totalwashred %>% filter(grepl('_G3_B8_', Name))
StaticB8G3 <- StaticB8G3 %>% distinct()
StaticB9G3 <- Totalwashred %>% filter(grepl('_G3_B9_', Name))
StaticB9G3 <- StaticB9G3 %>% distinct()
StaticB10G3 <- Totalwashred %>% filter(grepl('_G3_B10_', Name))
StaticB10G3 <- StaticB10G3 %>% distinct()

StaticB1G4 <- Totalwashred %>% filter(grepl('_G4_B1_', Name))
StaticB1G4 <- StaticB1G4 %>% distinct()
StaticB2G4 <- Totalwashred %>% filter(grepl('_G4_B2_', Name))
StaticB2G4 <- StaticB2G4 %>% distinct()
StaticB3G4 <- Totalwashred %>% filter(grepl('_G4_B3_', Name))
StaticB3G4 <- StaticB3G4 %>% distinct()
StaticB4G4 <- Totalwashred %>% filter(grepl('_G4_B4_', Name))
StaticB4G4 <- StaticB4G4 %>% distinct()
StaticB5G4 <- Totalwashred %>% filter(grepl('_G4_B5_', Name))
StaticB5G4 <- StaticB5G4 %>% distinct()
StaticB6G4 <- Totalwashred %>% filter(grepl('_G4_B6_', Name))
StaticB6G4 <- StaticB6G4 %>% distinct()
StaticB7G4 <- Totalwashred %>% filter(grepl('_G4_B7_', Name))
StaticB7G4 <- StaticB7G4 %>% distinct()
StaticB8G4 <- Totalwashred %>% filter(grepl('_G4_B8_', Name))
StaticB8G4 <- StaticB8G4 %>% distinct()
StaticB9G4 <- Totalwashred %>% filter(grepl('_G4_B9_', Name))
StaticB9G4 <- StaticB9G4 %>% distinct()
StaticB10G4 <- Totalwashred %>% filter(grepl('_G4_B10_', Name))
StaticB10G4 <- StaticB10G4 %>% distinct()

StaticB1G5 <- Totalwashred %>% filter(grepl('_G5_B1_', Name))
StaticB1G5 <- StaticB1G5 %>% distinct()
StaticB2G5 <- Totalwashred %>% filter(grepl('_G5_B2_', Name))
StaticB2G5 <- StaticB2G5 %>% distinct()
StaticB3G5 <- Totalwashred %>% filter(grepl('_G5_B3_', Name))
StaticB3G5 <- StaticB3G5 %>% distinct()
StaticB4G5 <- Totalwashred %>% filter(grepl('_G5_B4_', Name))
StaticB4G5 <- StaticB4G5 %>% distinct()
StaticB5G5 <- Totalwashred %>% filter(grepl('_G5_B5_', Name))
StaticB5G5 <- StaticB5G5 %>% distinct()
StaticB6G5 <- Totalwashred %>% filter(grepl('_G5_B6_', Name))
StaticB6G5 <- StaticB6G5 %>% distinct()
StaticB7G5 <- Totalwashred %>% filter(grepl('_G5_B7_', Name))
StaticB7G5 <- StaticB7G5 %>% distinct()
StaticB8G5 <- Totalwashred %>% filter(grepl('_G5_B8_', Name))
StaticB8G5 <- StaticB8G5 %>% distinct()
StaticB9G5 <- Totalwashred %>% filter(grepl('_G5_B9_', Name))
StaticB9G5 <- StaticB9G5 %>% distinct()
StaticB10G5 <- Totalwashred %>% filter(grepl('_G5_B10_', Name))
StaticB10G5 <- StaticB10G5 %>% distinct()

StaticRTB1y <- TotRTyellow %>% filter(grepl('RT_B1y_', Name))
StaticRTB1y <- StaticRTB1y %>% distinct()
StaticRTB3y <- TotRTyellow %>% filter(grepl('RT_B3Y_', Name))
StaticRTB3y <- StaticRTB3y %>% distinct()
StaticRTB6y<- TotRTyellow %>% filter(grepl('RT_B6Y_', Name))
StaticRTB6y <- StaticRTB6y %>% distinct()
StaticRTB8y <- TotRTyellow %>% filter(grepl('RT_B8Y_', Name))
StaticRTB8y <- StaticRTB8y %>% distinct()

StaticB1G1y <- Totalwashyellow %>% filter(grepl('_G1_B1_', Name))
StaticB1G1y <- StaticB1G1y %>% distinct()
StaticB3G1y <- Totalwashyellow %>% filter(grepl('_G1_B3_', Name))
StaticB3G1y <- StaticB3G1y %>% distinct()
StaticB6G1y <- Totalwashyellow %>% filter(grepl('_G1_B6_', Name))
StaticB6G1y <- StaticB6G1y %>% distinct()
StaticB8G1y <- Totalwashyellow %>% filter(grepl('_G1_B8_', Name))
StaticB8G1y <- StaticB8G1y %>% distinct()

##############################
# Calculation of the Static coefficient and uncertainty of measurement Uµs
M1_transfer <- 799.07
UM1_transfer <- 0.010/sqrt(12)

n <- 51
numS <- data.frame(setdiff(0:n, c()))
names(numS) <- c("Wash")
numSB1G1 <- data.frame(setdiff(0:n, c(46))) #W046 missing
names(numSB1G1) <- c("Wash")
numSB1G3 <- data.frame(setdiff(0:n, c(18))) #W046 missing
names(numSB1G3) <- c("Wash")
numSB6G1 <- data.frame(setdiff(0:n, c(19))) #W019 missing
names(numSB6G1) <- c("Wash")
numSB6G2 <- data.frame(setdiff(0:n, c(0,41))) #W000 and W041 missing
names(numSB6G2) <- c("Wash")
numS2 <- data.frame(setdiff(0:n, c(18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50)))
names(numS2) <- c("Wash")
numSB3G2 <- data.frame(setdiff(0:n, c(0,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50))) #W000 missing
names(numSB3G2) <- c("Wash")
numSB7G1 <- data.frame(setdiff(0:n, c(6,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50))) #W006 missing
names(numSB7G1) <- c("Wash")
numSB7G2 <- data.frame(setdiff(0:n, c(0,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50))) #W000 missing
names(numSB7G2) <- c("Wash")
numSB8G2 <- data.frame(setdiff(0:n, c(0,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50))) #W000 missing
names(numSB8G2) <- c("Wash")
numS3 <- data.frame(setdiff(0:n, c(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,
                                  27,28,29,30,32,33,34,35,37,38,39,40,42,43,44,45,47,48,49,50)))
names(numS3) <- c("Wash")
numSB9G2 <- data.frame(setdiff(0:n, c(0,2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,
                                   27,28,29,30,32,33,34,35,37,38,39,40,42,43,44,45,47,48,49,50)))#W000 missing
names(numSB9G2) <- c("Wash")
numS4 <- data.frame(setdiff(0:n, c(2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,22,23,24,25,26,
                                   27,28,29,30,32,33,34,35,36,37,38,39,40,42,43,44,45,46,47,48,49,50)))
names(numS4) <- c("Wash")
numSB10G2 <- data.frame(setdiff(0:n, c(0,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,20,22,23,24,25,26,
                                   27,28,29,30,32,33,34,35,36,37,38,39,40,42,43,44,45,46,47,48,49,50)))#W000 missing
names(numSB10G2) <- c("Wash")

#### B1G1 ####
B1G1 <-cbind(StaticB1G1,numSB1G1)
B1G1$µs <- round(B1G1$Fstat_corrected/(M1_transfer), digit=3)
B1G1$Uµs <- B1G1$µs*(sqrt(((B1G1$UFstat_corrected/B1G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B1G1$'2Uµs' <- (B1G1$Uµs)*2
#### B2G1 ####
B2G1 <-cbind(StaticB2G1,numS2)
B2G1$µs <- round(B2G1$Fstat_corrected/(M1_transfer), digit=3)
B2G1$Uµs <- B2G1$µs*(sqrt(((B2G1$UFstat_corrected/B2G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B2G1$'2Uµs' <- (B2G1$Uµs)*2
#### B3G1 ####
B3G1 <-cbind(StaticB3G1,numS2)
B3G1$µs <- round(B3G1$Fstat_corrected/(M1_transfer), digit=3)
B3G1$Uµs <- B3G1$µs*(sqrt(((B3G1$UFstat_corrected/B3G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B3G1$'2Uµs' <- (B3G1$Uµs)*2
#### B4G1 ####
B4G1 <-cbind(StaticB4G1,numS3)
B4G1$µs <- round(B4G1$Fstat_corrected/(M1_transfer), digit=3)
B4G1$Uµs <- B4G1$µs*(sqrt(((B4G1$UFstat_corrected/B4G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B4G1$'2Uµs' <- (B4G1$Uµs)*2
#### B5G1 ####
B5G1 <-cbind(StaticB5G1,numS4)
B5G1$µs <- round(B5G1$Fstat_corrected/(M1_transfer), digit=3)
B5G1$Uµs <- B5G1$µs*(sqrt(((B5G1$UFstat_corrected/B5G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B5G1$'2Uµs' <- (B5G1$Uµs)*2
#### B6G1 ####
B6G1 <-cbind(StaticB6G1,numSB6G1)
B6G1$µs <- round(B6G1$Fstat_corrected/(M1_transfer), digit=3)
B6G1$Uµs <- B6G1$µs*(sqrt(((B6G1$UFstat_corrected/B6G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B6G1$'2Uµs' <- (B6G1$Uµs)*2
#### B7G1 ####
B7G1 <-cbind(StaticB7G1,numSB7G1)
B7G1$µs <- round(B7G1$Fstat_corrected/(M1_transfer), digit=3)
B7G1$Uµs <- B7G1$µs*(sqrt(((B7G1$UFstat_corrected/B7G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B7G1$'2Uµs' <- (B7G1$Uµs)*2
#### B8G1 ####
B8G1 <-cbind(StaticB8G1,numS2)
B8G1$µs <- round(B8G1$Fstat_corrected/(M1_transfer), digit=3)
B8G1$Uµs <- B8G1$µs*(sqrt(((B8G1$UFstat_corrected/B8G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B8G1$'2Uµs' <- (B8G1$Uµs)*2
#### B9G1 ####
B9G1 <-cbind(StaticB9G1,numS3)
B9G1$µs <- round(B9G1$Fstat_corrected/(M1_transfer), digit=3)
B9G1$Uµs <- B9G1$µs*(sqrt(((B9G1$UFstat_corrected/B9G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B9G1$'2Uµs' <- (B9G1$Uµs)*2
#### B10G1 ####
B10G1 <-cbind(StaticB10G1,numS4)
B10G1$µs <- round(B10G1$Fstat_corrected/(M1_transfer), digit=3)
B10G1$Uµs <- B10G1$µs*(sqrt(((B10G1$UFstat_corrected/B10G1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B10G1$'2Uµs' <- (B10G1$Uµs)*2
#### B1G2 ####
B1G2 <-cbind(StaticB1G2,numS)
B1G2$µs <- round(B1G2$Fstat_corrected/(M1_transfer), digit=3)
B1G2$Uµs <- B1G2$µs*(sqrt(((B1G2$UFstat_corrected/B1G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B1G2$'2Uµs' <- (B1G2$Uµs)*2
#### B2G2 ####
B2G2 <-cbind(StaticB2G2,numS2)
B2G2$µs <- round(B2G2$Fstat_corrected/(M1_transfer), digit=3)
B2G2$Uµs <- B2G2$µs*(sqrt(((B2G2$UFstat_corrected/B2G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B2G2$'2Uµs' <- (B2G2$Uµs)*2
#### B3G2 ####
B3G2 <-cbind(StaticB3G2,numSB3G2)
B3G2$µs <- round(B3G2$Fstat_corrected/(M1_transfer), digit=3)
B3G2$Uµs <- B3G2$µs*(sqrt(((B3G2$UFstat_corrected/B3G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B3G2$'2Uµs' <- (B3G2$Uµs)*2
#### B4G2 ####
B4G2 <-cbind(StaticB4G2,numS3)
B4G2$µs <- round(B4G2$Fstat_corrected/(M1_transfer), digit=3)
B4G2$Uµs <- B4G2$µs*(sqrt(((B4G2$UFstat_corrected/B4G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B4G2$'2Uµs' <- (B4G2$Uµs)*2
#### B5G2 ####
B5G2 <-cbind(StaticB5G2,numS4)
B5G2$µs <- round(B5G2$Fstat_corrected/(M1_transfer), digit=3)
B5G2$Uµs <- B5G2$µs*(sqrt(((B5G2$UFstat_corrected/B5G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B5G2$'2Uµs' <- (B5G2$Uµs)*2
#### B6G2 ####
B6G2 <-cbind(StaticB6G2,numSB6G2)
B6G2$µs <- round(B6G2$Fstat_corrected/(M1_transfer), digit=3)
B6G2$Uµs <- B6G2$µs*(sqrt(((B6G2$UFstat_corrected/B6G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B6G2$'2Uµs' <- (B6G2$Uµs)*2
#### B7G2 ####
B7G2 <-cbind(StaticB7G2,numSB7G2)
B7G2$µs <- round(B7G2$Fstat_corrected/(M1_transfer), digit=3)
B7G2$Uµs <- B7G2$µs*(sqrt(((B7G2$UFstat_corrected/B7G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B7G2$'2Uµs' <- (B7G2$Uµs)*2
#### B8G2 ####
B8G2 <-cbind(StaticB8G2,numSB8G2)
B8G2$µs <- round(B8G2$Fstat_corrected/(M1_transfer), digit=3)
B8G2$Uµs <- B8G2$µs*(sqrt(((B8G2$UFstat_corrected/B8G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B8G2$'2Uµs' <- (B8G2$Uµs)*2
#### B9G2 ####
B9G2 <-cbind(StaticB9G2,numSB9G2)
B9G2$µs <- round(B9G2$Fstat_corrected/(M1_transfer), digit=3)
B9G2$Uµs <- B9G2$µs*(sqrt(((B9G2$UFstat_corrected/B9G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B9G2$'2Uµs' <- (B9G2$Uµs)*2
#### B10G2 ####
B10G2 <-cbind(StaticB10G2,numSB10G2)
B10G2$µs <- round(B10G2$Fstat_corrected/(M1_transfer), digit=3)
B10G2$Uµs <- B10G2$µs*(sqrt(((B10G2$UFstat_corrected/B10G2$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B10G2$'2Uµs' <- (B10G2$Uµs)*2
#### B1G3 ####
B1G3 <-cbind(StaticB1G3,numSB1G3)
B1G3$µs <- round(B1G3$Fstat_corrected/(M1_transfer), digit=3)
B1G3$Uµs <- B1G3$µs*(sqrt(((B1G3$UFstat_corrected/B1G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B1G3$'2Uµs' <- (B1G3$Uµs)*2
#### B2G3 ####
B2G3 <-cbind(StaticB2G3,numS2)
B2G3$µs <- round(B2G3$Fstat_corrected/(M1_transfer), digit=3)
B2G3$Uµs <- B2G3$µs*(sqrt(((B2G3$UFstat_corrected/B2G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B2G3$'2Uµs' <- (B2G3$Uµs)*2
#### B3G3 ####
B3G3 <-cbind(StaticB3G3,numS2)
B3G3$µs <- round(B3G3$Fstat_corrected/(M1_transfer), digit=3)
B3G3$Uµs <- B3G3$µs*(sqrt(((B3G3$UFstat_corrected/B3G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B3G3$'2Uµs' <- (B3G3$Uµs)*2
#### B4G3 ####
B4G3 <-cbind(StaticB4G3,numS3)
B4G3$µs <- round(B4G3$Fstat_corrected/(M1_transfer), digit=3)
B4G3$Uµs <- B4G3$µs*(sqrt(((B4G3$UFstat_corrected/B4G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B4G3$'2Uµs' <- (B4G3$Uµs)*2
#### B5G3 ####
B5G3 <-cbind(StaticB5G3,numS4)
B5G3$µs <- round(B5G3$Fstat_corrected/(M1_transfer), digit=3)
B5G3$Uµs <- B5G3$µs*(sqrt(((B5G3$UFstat_corrected/B5G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B5G3$'2Uµs' <- (B5G3$Uµs)*2
#### B6G3 ####
B6G3 <-cbind(StaticB6G3,numS)
B6G3$µs <- round(B6G3$Fstat_corrected/(M1_transfer), digit=3)
B6G3$Uµs <- B6G3$µs*(sqrt(((B6G3$UFstat_corrected/B6G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B6G3$'2Uµs' <- (B6G3$Uµs)*2
#### B7G3 ####
B7G3 <-cbind(StaticB7G3,numS2)
B7G3$µs <- round(B7G3$Fstat_corrected/(M1_transfer), digit=3)
B7G3$Uµs <- B7G3$µs*(sqrt(((B7G3$UFstat_corrected/B7G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B7G3$'2Uµs' <- (B7G3$Uµs)*2
#### B8G3 ####
B8G3 <-cbind(StaticB8G3,numS2)
B8G3$µs <- round(B8G3$Fstat_corrected/(M1_transfer), digit=3)
B8G3$Uµs <- B8G3$µs*(sqrt(((B8G3$UFstat_corrected/B8G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B8G3$'2Uµs' <- (B8G3$Uµs)*2
#### B9G3 ####
B9G3 <-cbind(StaticB9G3,numS3)
B9G3$µs <- round(B9G3$Fstat_corrected/(M1_transfer), digit=3)
B9G3$Uµs <- B9G3$µs*(sqrt(((B9G3$UFstat_corrected/B9G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B9G3$'2Uµs' <- (B9G3$Uµs)*2
#### B10G3 ####
B10G3 <-cbind(StaticB10G3,numS4)
B10G3$µs <- round(B10G3$Fstat_corrected/(M1_transfer), digit=3)
B10G3$Uµs <- B10G3$µs*(sqrt(((B10G3$UFstat_corrected/B10G3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B10G3$'2Uµs' <- (B10G3$Uµs)*2
#### B1G4 ####
B1G4 <-cbind(StaticB1G4,numS)
B1G4$µs <- round(B1G4$Fstat_corrected/(M1_transfer), digit=3)
B1G4$Uµs <- B1G4$µs*(sqrt(((B1G4$UFstat_corrected/B1G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B1G4$'2Uµs' <- (B1G4$Uµs)*2
#### B2G4 ####
B2G4 <-cbind(StaticB2G4,numS2)
B2G4$µs <- round(B2G4$Fstat_corrected/(M1_transfer), digit=3)
B2G4$Uµs <- B2G4$µs*(sqrt(((B2G4$UFstat_corrected/B2G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B2G4$'2Uµs' <- (B2G4$Uµs)*2
#### B3G4 ####
B3G4 <-cbind(StaticB3G4,numS2)
B3G4$µs <- round(B3G4$Fstat_corrected/(M1_transfer), digit=3)
B3G4$Uµs <- B3G4$µs*(sqrt(((B3G4$UFstat_corrected/B3G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B3G4$'2Uµs' <- (B3G4$Uµs)*2
#### B4G4 ####
B4G4 <-cbind(StaticB4G4,numS3)
B4G4$µs <- round(B4G4$Fstat_corrected/(M1_transfer), digit=3)
B4G4$Uµs <- B4G4$µs*(sqrt(((B4G4$UFstat_corrected/B4G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B4G4$'2Uµs' <- (B4G4$Uµs)*2
#### B5G4 ####
B5G4 <-cbind(StaticB5G4,numS4)
B5G4$µs <- round(B5G4$Fstat_corrected/(M1_transfer), digit=3)
B5G4$Uµs <- B5G4$µs*(sqrt(((B5G4$UFstat_corrected/B5G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B5G4$'2Uµs' <- (B5G4$Uµs)*2
#### B6G4 ####
B6G4 <-cbind(StaticB6G4,numS)
B6G4$µs <- round(B6G4$Fstat_corrected/(M1_transfer), digit=3)
B6G4$Uµs <- B6G4$µs*(sqrt(((B6G4$UFstat_corrected/B6G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B6G4$'2Uµs' <- (B6G4$Uµs)*2
#### B7G4 ####
B7G4 <-cbind(StaticB7G4,numS2)
B7G4$µs <- round(B7G4$Fstat_corrected/(M1_transfer), digit=3)
B7G4$Uµs <- B7G4$µs*(sqrt(((B7G4$UFstat_corrected/B7G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B7G4$'2Uµs' <- (B7G4$Uµs)*2
#### B8G4 ####
B8G4 <-cbind(StaticB8G4,numS2)
B8G4$µs <- round(B8G4$Fstat_corrected/(M1_transfer), digit=3)
B8G4$Uµs <- B8G4$µs*(sqrt(((B8G4$UFstat_corrected/B8G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B8G4$'2Uµs' <- (B8G4$Uµs)*2
#### B9G4 ####
B9G4 <-cbind(StaticB9G4,numS3)
B9G4$µs <- round(B9G4$Fstat_corrected/(M1_transfer), digit=3)
B9G4$Uµs <- B9G4$µs*(sqrt(((B9G4$UFstat_corrected/B9G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B9G4$'2Uµs' <- (B9G4$Uµs)*2
#### B10G4 ####
B10G4 <-cbind(StaticB10G4,numS4)
B10G4$µs <- round(B10G4$Fstat_corrected/(M1_transfer), digit=3)
B10G4$Uµs <- B10G4$µs*(sqrt(((B10G4$UFstat_corrected/B10G4$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B10G4$'2Uµs' <- (B10G4$Uµs)*2
#### B1G5 ####
B1G5 <-cbind(StaticB1G5,numS)
B1G5$µs <- round(B1G5$Fstat_corrected/(M1_transfer), digit=3)
B1G5$Uµs <- B1G5$µs*(sqrt(((B1G5$UFstat_corrected/B1G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B1G5$'2Uµs' <- (B1G5$Uµs)*2
#### B2G5 ####
B2G5 <-cbind(StaticB2G5,numS2)
B2G5$µs <- round(B2G5$Fstat_corrected/(M1_transfer), digit=3)
B2G5$Uµs <- B2G5$µs*(sqrt(((B2G5$UFstat_corrected/B2G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B2G5$'2Uµs' <- (B2G5$Uµs)*2
#### B3G5 ####
B3G5 <-cbind(StaticB3G5,numS2)
B3G5$µs <- round(B3G5$Fstat_corrected/(M1_transfer), digit=3)
B3G5$Uµs <- B3G5$µs*(sqrt(((B3G5$UFstat_corrected/B3G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B3G5$'2Uµs' <- (B3G5$Uµs)*2
#### B4G5 ####
B4G5 <-cbind(StaticB4G5,numS3)
B4G5$µs <- round(B4G5$Fstat_corrected/(M1_transfer), digit=3)
B4G5$Uµs <- B4G5$µs*(sqrt(((B4G5$UFstat_corrected/B4G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B4G5$'2Uµs' <- (B4G5$Uµs)*2
#### B5G5 ####
B5G5 <-cbind(StaticB5G5,numS4)
B5G5$µs <- round(B5G5$Fstat_corrected/(M1_transfer), digit=3)
B5G5$Uµs <- B5G5$µs*(sqrt(((B5G5$UFstat_corrected/B5G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B5G5$'2Uµs' <- (B5G5$Uµs)*2
#### B6G5 ####
B6G5 <-cbind(StaticB6G5,numS)
B6G5$µs <- round(B6G5$Fstat_corrected/(M1_transfer), digit=3)
B6G5$Uµs <- B6G5$µs*(sqrt(((B6G5$UFstat_corrected/B6G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B6G5$'2Uµs' <- (B6G5$Uµs)*2
#### B7G5 ####
B7G5 <-cbind(StaticB7G5,numS2)
B7G5$µs <- round(B7G5$Fstat_corrected/(M1_transfer), digit=3)
B7G5$Uµs <- B7G5$µs*(sqrt(((B7G5$UFstat_corrected/B7G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B7G5$'2Uµs' <- (B7G5$Uµs)*2
#### B8G5 ####
B8G5 <-cbind(StaticB8G5,numS2)
B8G5$µs <- round(B8G5$Fstat_corrected/(M1_transfer), digit=3)
B8G5$Uµs <- B8G5$µs*(sqrt(((B8G5$UFstat_corrected/B8G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B8G5$'2Uµs' <- (B8G5$Uµs)*2
#### B9G5 ####
B9G5 <-cbind(StaticB9G5,numS3)
B9G5$µs <- round(B9G5$Fstat_corrected/(M1_transfer), digit=3)
B9G5$Uµs <- B9G5$µs*(sqrt(((B9G5$UFstat_corrected/B9G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B9G5$'2Uµs' <- (B9G5$Uµs)*2
#### B10G5 ####
B10G5 <-cbind(StaticB10G5,numS4)
B10G5$µs <- round(B10G5$Fstat_corrected/(M1_transfer), digit=3)
B10G5$Uµs <- B10G5$µs*(sqrt(((B10G5$UFstat_corrected/B10G5$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B10G5$'2Uµs' <- (B10G5$Uµs)*2

n <- 100
numSRT <- data.frame(setdiff(1:n, c()))
names(numSRT) <- c("Repetitive transfer")
#### RTB1 ####
RTB1 <-cbind(StaticRTB1,numSRT)
RTB1$µs <- round(RTB1$Fstat_corrected/(M1_transfer), digit=3)
RTB1$Uµs <- RTB1$µs*(sqrt(((RTB1$UFstat_corrected/RTB1$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
RTB1$'2Uµs' <- (RTB1$Uµs)*2
#### RTB3 ####
RTB3 <-cbind(StaticRTB3,numSRT)
RTB3$µs <- round(RTB3$Fstat_corrected/(M1_transfer), digit=3)
RTB3$Uµs <- RTB3$µs*(sqrt(((RTB3$UFstat_corrected/RTB3$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
RTB3$'2Uµs' <- (RTB3$Uµs)*2
#### RTB6 ####
RTB6 <-cbind(StaticRTB6,numSRT)
RTB6$µs <- round(RTB6$Fstat_corrected/(M1_transfer), digit=3)
RTB6$Uµs <- RTB6$µs*(sqrt(((RTB6$UFstat_corrected/RTB6$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
RTB6$'2Uµs' <- (RTB6$Uµs)*2
#### RTB8 ####
RTB8 <-cbind(StaticRTB8,numSRT)
RTB8$µs <- round(RTB8$Fstat_corrected/(M1_transfer), digit=3)
RTB8$Uµs <- RTB8$µs*(sqrt(((RTB8$UFstat_corrected/RTB8$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
RTB8$'2Uµs' <- (RTB8$Uµs)*2
#### RTB1y ####
RTB1y <-cbind(StaticRTB1y,numSRT)
RTB1y$µs <- round(RTB1y$Fstat_corrected/(M1_transfer), digit=3)
RTB1y$Uµs <- RTB1y$µs*(sqrt(((RTB1y$UFstat_corrected/RTB1y$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
RTB1y$'2Uµs' <- (RTB1y$Uµs)*2
#### RTB3y ####
RTB3y <-cbind(StaticRTB3y,numSRT)
RTB3y$µs <- round(RTB3y$Fstat_corrected/(M1_transfer), digit=3)
RTB3y$Uµs <- RTB3y$µs*(sqrt(((RTB3y$UFstat_corrected/RTB3y$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
RTB3y$'2Uµs' <- (RTB3y$Uµs)*2
#### RTB6y ####
RTB6y <-cbind(StaticRTB6y,numSRT)
RTB6y$µs <- round(RTB6y$Fstat_corrected/(M1_transfer), digit=3)
RTB6y$Uµs <- RTB6y$µs*(sqrt(((RTB6y$UFstat_corrected/RTB6y$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
RTB6y$'2Uµs' <- (RTB6y$Uµs)*2
#### RTB8y ####
RTB8y <-cbind(StaticRTB8y,numSRT)
RTB8y$µs <- round(RTB8y$Fstat_corrected/(M1_transfer), digit=3)
RTB8y$Uµs <- RTB8y$µs*(sqrt(((RTB8y$UFstat_corrected/RTB8y$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
RTB8y$'2Uµs' <- (RTB8y$Uµs)*2

n <- 15
numSWy <- data.frame(setdiff(0:n, c(6,7,8,9,11,12,13,14)))
names(numSWy) <- c("Wash")
#### B1G1y ####
B1G1y <-cbind(StaticB1G1y,numSWy)
B1G1y$µs <- round(B1G1y$Fstat_corrected/(M1_transfer), digit=3)
B1G1y$Uµs <- B1G1y$µs*(sqrt(((B1G1y$UFstat_corrected/B1G1y$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B1G1y$'2Uµs' <- (B1G1y$Uµs)*2
#### B3G1y ####
B3G1y <-cbind(StaticB3G1y,numSWy)
B3G1y$µs <- round(B3G1y$Fstat_corrected/(M1_transfer), digit=3)
B3G1y$Uµs <- B3G1y$µs*(sqrt(((B3G1y$UFstat_corrected/B3G1y$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B3G1y$'2Uµs' <- (B3G1y$Uµs)*2
#### B6G1y ####
B6G1y <-cbind(StaticB6G1y,numSWy)
B6G1y$µs <- round(B6G1y$Fstat_corrected/(M1_transfer), digit=3)
B6G1y$Uµs <- B6G1y$µs*(sqrt(((B6G1y$UFstat_corrected/B6G1y$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B6G1y$'2Uµs' <- (B6G1y$Uµs)*2
#### B8G1y ####
B8G1y <-cbind(StaticB8G1y,numSWy)
B8G1y$µs <- round(B8G1y$Fstat_corrected/(M1_transfer), digit=3)
B8G1y$Uµs <- B8G1y$µs*(sqrt(((B8G1y$UFstat_corrected/B8G1y$Fstat)^2)+((UM1_transfer/M1_transfer)^2)))
B8G1y$'2Uµs' <- (B8G1y$Uµs)*2

#############################################################
#####             GRAPHS Static coeff                   #####
#############################################################
#### GRAPH B1G5 ####
pB1G5 <-ggplot(data = B1G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour= "#000000")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA1G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B1G5$µs),digits = 2)
pB1G5 <-pB1G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB1G5
ggsave("Arduino_B1G5.png", pB1G5, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B2G5 ####
pB2G5 <-ggplot(data = B2G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour= "#077d1b")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA2G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B2G5$µs),digits = 2)
pB2G5 <-pB2G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB2G5

ggsave("Arduino_B2G5.png", pB2G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B3G5 ####
pB3G5 <-ggplot(data = B3G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour= "#125fdb")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA3G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B3G5$µs),digits = 2)
pB3G5 <-pB3G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB3G5

ggsave("Arduino_B3G5.png", pB3G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B4G5 ####
pB4G5 <-ggplot(data = B4G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour ="#bf192a")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA4G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B4G5$µs),digits = 2)
pB4G5 <-pB4G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB4G5

ggsave("Arduino_B4G5.png", pB4G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B5G5 ####
pB5G5 <-ggplot(data = B5G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour="#db9e1a")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA5G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B5G5$µs),digits = 2)
pB5G5 <-pB5G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB5G5

ggsave("Arduino_B5G5.png", pB5G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B6G5 ####
pB6G5 <-ggplot(data = B6G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour= "#000000")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA6G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B6G5$µs),digits = 2)
pB6G5 <-pB6G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB6G5

ggsave("Arduino_B6G5.png", pB6G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")
#### GRAPH B7G5 ####
pB7G5 <-ggplot(data = B7G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour= "#077d1b")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA7G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B7G5$µs),digits = 2)
pB7G5 <-pB7G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB7G5

ggsave("Arduino_B7G5.png", pB7G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B8G5 ####
pB8G5 <-ggplot(data = B8G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour= "#125fdb")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA8G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B8G5$µs),digits = 2)
pB8G5 <-pB8G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB8G5

ggsave("Arduino_B8G5.png", pB8G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B9G5 ####
pB9G5 <-ggplot(data = B9G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour ="#bf192a")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA9G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B9G5$µs),digits = 2)
pB9G5 <-pB9G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB9G5

ggsave("Arduino_B9G5.png", pB9G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B10G5 ####
pB10G5 <-ggplot(data = B10G5, aes(Wash,µs)) +
  geom_line(size=0.3, colour="#db9e1a")+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.30,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA10G5")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.35,label.x = 20)
mean <- round(mean(B10G5$µs),digits = 2)
pB10G5 <-pB10G5 + annotate("text",  x=Inf, y = Inf, label = mean, vjust=2, hjust=3.5)
pB10G5

ggsave("Arduino_B10G5.png", pB10G5, width = 6, height = 4, units = "in", dpi=200,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### Combined grap red - FIGURE XXX ####
pCombined_pending <- ggarrange(pB1G5+ rremove("ylab") + rremove("xlab"),
                               pB6G5+ rremove("ylab") + rremove("xlab"),
                               pB3G5+ rremove("ylab") + rremove("xlab"),
                               pB8G5+ rremove("ylab") + rremove("xlab"),
                               pB4G5+ rremove("ylab") + rremove("xlab"),
                               pB9G5+ rremove("ylab") + rremove("xlab"),
                               pB5G5+ rremove("ylab") + rremove("xlab"),
                               pB10G5+ rremove("ylab") + rremove("xlab"),
                               labels = NULL,
                               common.legend = TRUE, legend = "bottom",
                               align = "hv",
                               ncol = 2, nrow = 4,
                               font.label = list(size = 8, color = "black", family = NULL, position = "top"))

pCombined <- annotate_figure(pCombined_pending, left = textGrob("Static coefficient", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombined

ggsave("Arduino_combined graph_red jumpers.png", pCombined, width = 7, height = 9, units = "in", dpi=600,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino")

#### Combined grap red - ANNEXES ####
pCombined_pending <- ggarrange(pB1G5+ rremove("ylab") + rremove("xlab"),
                               pB6G5+ rremove("ylab") + rremove("xlab"),
                               pB2G5+ rremove("ylab") + rremove("xlab"),
                               pB7G5+ rremove("ylab") + rremove("xlab"),
                               pB3G5+ rremove("ylab") + rremove("xlab"),
                               pB8G5+ rremove("ylab") + rremove("xlab"),
                               pB4G5+ rremove("ylab") + rremove("xlab"),
                               pB9G5+ rremove("ylab") + rremove("xlab"),
                               pB5G5+ rremove("ylab") + rremove("xlab"),
                               pB10G5+ rremove("ylab") + rremove("xlab"),
                               labels = NULL,
                               common.legend = TRUE, legend = "bottom",
                               align = "hv",
                               ncol = 2, nrow = 5,
                               font.label = list(size = 8, color = "black", family = NULL, position = "top"))
pCombined_pending

pCombined <- annotate_figure(pCombined_pending,
                             left = textGrob("Static coefficient", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                             bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombined

ggsave("Arduino_combined graph_Annexe_G5.png", pCombined, width = 7, height = 10, units = "in", dpi=600,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino")

#### Mean and SD control garment ####
meanRTB1µs <- round(mean(RTB1$µs),digits=2)
meanRTB3µs <- round(mean(RTB3$µs),digits=2)
meanRTB6µs <- round(mean(RTB6$µs),digits=2)
meanRTB8µs <- round(mean(RTB8$µs),digits=2)
sdRTB1µs <- round(sd(RTB1$µs),digits=2)
sdRTB3µs <- round(sd(RTB3$µs),digits=2)
sdRTB6µs <- round(sd(RTB6$µs),digits=2)
sdRTB8µs <- round(sd(RTB8$µs),digits=2)

#### GRAPH RTB1 ####
pRTB1 <-ggplot(data = RTB1, aes(`Repetitive transfer`,µs)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.3,0.7)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Parallel contact area 1")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.68,label.x = 50)
pRTB1
ggsave("Arduino_RTB1.png", pRTB1, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH RTB3 ####
pRTB3 <-ggplot(data = RTB3, aes(`Repetitive transfer`,µs)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.3,0.7)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Parallel contact area 2")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.68,label.x = 50)
pRTB3
ggsave("Arduino_RTB3.png", pRTB3, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH RTB6 ####
pRTB6 <-ggplot(data = RTB6, aes(`Repetitive transfer`,µs)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.3,0.7)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Perpendicular contact area 1")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.68,label.x = 50)
pRTB6
ggsave("Arduino_RTB6.png", pRTB6, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH RTB8 ####
pRTB8 <-ggplot(data = RTB8, aes(`Repetitive transfer`,µs)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.3,0.7)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Perpendicular contact area 2")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.68,label.x = 50)
pRTB8
ggsave("Arduino_RTB8.png", pRTB8, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### Combined grap RT red - FIGURE XXX ####
pCombinedRT_pending <- ggarrange(pRTB1+ rremove("ylab") + rremove("xlab"),
                               pRTB3+ rremove("ylab") + rremove("xlab"),
                               pRTB6+ rremove("ylab") + rremove("xlab"),
                               pRTB8+ rremove("ylab") + rremove("xlab"),
                               labels = NULL,
                               common.legend = TRUE, legend = "bottom",
                               align = "hv",
                               ncol = 2, nrow = 2,
                               font.label = list(size = 8, color = "black", family = NULL, position = "top"))

pCombinedRT <- annotate_figure(pCombinedRT_pending, left = textGrob("Static coefficient", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                             bottom = textGrob("Repetitive transfer", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedRT

ggsave("Arduino_combined graph RT_red jumpers.png", pCombinedRT, width = 7, height = 6, units = "in", dpi=600,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/RT")

#### Correlation between the four bands ####
RepetitiveTransferTot <- cbind(`Parallel contact area 1`=RTB1$µs,
                               `Parallel contact area 2`=RTB3$µs,
                               `Perpendicular contact area 1`=RTB6$µs,
                               `Perpendicular contact area 2`=RTB8$µs)

CorrelationRT <- pairs.panels(RepetitiveTransferTot[,1:4],
             stars = F, # If TRUE, adds significance level with stars
             pch=20, # points shape
             lm=T, # Plot the linear fit rather than the LOESS smoothed fits
             method = "pearson", # correlation method
             hist.col = "#6BAED6",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)
CorrelationRT # have to be saved manually

#### GRAPH RTB1y ####
meanRTB1yµs <- round(mean(RTB1y$µs),digits=2)
meanRTB3yµs <- round(mean(RTB3y$µs),digits=2)
meanRTB6yµs <- round(mean(RTB6y$µs),digits=2)
meanRTB8yµs <- round(mean(RTB8y$µs),digits=2)
sdRTB1yµs <- round(sd(RTB1y$µs),digits=2)
sdRTB3yµs <- round(sd(RTB3y$µs),digits=2)
sdRTB6yµs <- round(sd(RTB6y$µs),digits=2)
sdRTB8yµs <- round(sd(RTB8y$µs),digits=2)

pRTB1y <-ggplot(data = RTB1y, aes(`Repetitive transfer`,µs)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.3,0.7)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Parallel contact area 1")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.68,label.x = 50)
pRTB1y
ggsave("Arduino_RTB1y.png", pRTB1y, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH RTB3y ####
pRTB3y <-ggplot(data = RTB3y, aes(`Repetitive transfer`,µs)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.3,0.7)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Parallel contact area 2")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.68,label.x = 50)
pRTB3y
ggsave("Arduino_RTB3y.png", pRTB3y, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH RTB6y ####
pRTB6y <-ggplot(data = RTB6y, aes(`Repetitive transfer`,µs)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.3,0.7)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Perpendicular contact area 1")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.68,label.x = 50)
pRTB6y
ggsave("Arduino_RTB6y.png", pRTB6y, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH RTB8y ####
pRTB8y <-ggplot(data = RTB8y, aes(`Repetitive transfer`,µs)) +
  geom_line(size=0.3)+geom_point(size=0.1, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0.3,0.7)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("Perpendicular contact area 2")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)+
  stat_regline_equation(label.y = 0.68,label.x = 50)
pRTB8y
ggsave("Arduino_RTB8y.png", pRTB8y, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### Combined grap RT yellow - FIGURE XXX ####
CombinedRT_pending <- ggarrange(pRTB1y+ rremove("ylab") + rremove("xlab"),
                                pRTB3y+ rremove("ylab") + rremove("xlab"),
                                pRTB6y+ rremove("ylab") + rremove("xlab"),
                                pRTB8y+ rremove("ylab") + rremove("xlab"),
                                labels = NULL,
                                common.legend = TRUE, legend = "bottom",
                                align = "hv",
                                ncol = 2, nrow = 2,
                                font.label = list(size = 8, color = "black", family = NULL, position = "top"))

pCombinedRT <- annotate_figure(CombinedRT_pending, left = textGrob("Static coefficient", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                               bottom = textGrob("Repetitive transfer", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombinedRT

ggsave("Arduino_combined graph RT_yellow textile.png", pCombinedRT, width = 7, height = 6, units = "in", dpi=600,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/RT")


#### GRAPH B1G1y ####
pB1G1y <-ggplot(data = B1G1y, aes(Wash,µs)) +
  geom_line(size=0.3)+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA1")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
pB1G1y
ggsave("Arduino_B1G1y.png", pB1G1y, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B3G1y ####
pB3G1y <-ggplot(data = B3G1y, aes(Wash,µs)) +
  geom_line(size=0.3)+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA2")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
pB3G1y
ggsave("Arduino_B3G1y.png", pB3G1y, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B6G1y ####
pB6G1y <-ggplot(data = B6G1y, aes(Wash,µs)) +
  geom_line(size=0.3)+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA3")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
pB6G1y
ggsave("Arduino_B6G1y.png", pB6G1y, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### GRAPH B8G1y ####
pB8G1y <-ggplot(data = B8G1y, aes(Wash,µs)) +
  geom_line(size=0.3)+geom_point(size=0.4, colour= "darkred")+
  labs(x="Wash", y="Load")+ylim(0,1)+
  theme_bw(base_family = "Arial", base_size = 10) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+ggtitle("CA4")+
  geom_errorbar(aes(ymin=µs-`2Uµs`, ymax=µs+`2Uµs`), width=0.5)+
  geom_smooth(formula = y ~ x,method='lm', se=F,color="black", linetype="dashed", size=0.5)
pB8G1y
ggsave("Arduino_B8G1y.png", pB8G1y, width = 6, height = 4, units = "in", dpi=200,
       path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/")

#### Combined grap - FIGURE XXX ####
pCombined_pending <- ggarrange(pB1G1y+ rremove("ylab") + rremove("xlab"),
                               pB6G1y+ rremove("ylab") + rremove("xlab"),
                               pB3G1y+ rremove("ylab") + rremove("xlab"),
                               pB8G1y+ rremove("ylab") + rremove("xlab"),
                               labels = NULL,
                               common.legend = TRUE, legend = "bottom",
                               align = "hv",
                               ncol = 2, nrow = 2,
                               font.label = list(size = 8, color = "black", family = NULL, position = "top"))

pCombined <- annotate_figure(pCombined_pending, left = textGrob("Static coefficient", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                             bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)))
pCombined

ggsave("Arduino_combined graph_yellow textile.png", pCombined, width = 7, height = 6, units = "in", dpi=600,path = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino")

#############################################################
#####              TABLE _ table XXX                    #####
#############################################################
# add coder to the dataframe
B1G1$Garment <- c("G1")
B1G1$Band <- c("B1") 
B1G2$Garment <- c("G2")
B1G2$Band <- c("B1") 
B1G3$Garment <- c("G3")
B1G3$Band <- c("B1") 
B1G4$Garment <- c("G4")
B1G4$Band <- c("B1") 
B1G5$Garment <- c("G5")
B1G5$Band <- c("B1") 

B2G1$Garment <- c("G1")
B2G1$Band <- c("B2") 
B2G2$Garment <- c("G2")
B2G2$Band <- c("B2") 
B2G3$Garment <- c("G3")
B2G3$Band <- c("B2") 
B2G4$Garment <- c("G4")
B2G4$Band <- c("B2") 
B2G5$Garment <- c("G5")
B2G5$Band <- c("B2")

B3G1$Garment <- c("G1")
B3G1$Band <- c("B3") 
B3G2$Garment <- c("G2")
B3G2$Band <- c("B3") 
B3G3$Garment <- c("G3")
B3G3$Band <- c("B3") 
B3G4$Garment <- c("G4")
B3G4$Band <- c("B3") 
B3G5$Garment <- c("G5")
B3G5$Band <- c("B3") 

B4G1$Garment <- c("G1")
B4G1$Band <- c("B4") 
B4G2$Garment <- c("G2")
B4G2$Band <- c("B4") 
B4G3$Garment <- c("G3")
B4G3$Band <- c("B4") 
B4G4$Garment <- c("G4")
B4G4$Band <- c("B4") 
B4G5$Garment <- c("G5")
B4G5$Band <- c("B4") 

B5G1$Garment <- c("G1")
B5G1$Band <- c("B5") 
B5G2$Garment <- c("G2")
B5G2$Band <- c("B5") 
B5G3$Garment <- c("G3")
B5G3$Band <- c("B5") 
B5G4$Garment <- c("G4")
B5G4$Band <- c("B5") 
B5G5$Garment <- c("G5")
B5G5$Band <- c("B5") 

B6G1$Garment <- c("G1")
B6G1$Band <- c("B6") 
B6G2$Garment <- c("G2")
B6G2$Band <- c("B6") 
B6G3$Garment <- c("G3")
B6G3$Band <- c("B6") 
B6G4$Garment <- c("G4")
B6G4$Band <- c("B6") 
B6G5$Garment <- c("G5")
B6G5$Band <- c("B6") 

B7G1$Garment <- c("G1")
B7G1$Band <- c("B7") 
B7G2$Garment <- c("G2")
B7G2$Band <- c("B7") 
B7G3$Garment <- c("G3")
B7G3$Band <- c("B7") 
B7G4$Garment <- c("G4")
B7G4$Band <- c("B7") 
B7G5$Garment <- c("G5")
B7G5$Band <- c("B7") 

B8G1$Garment <- c("G1")
B8G1$Band <- c("B8") 
B8G2$Garment <- c("G2")
B8G2$Band <- c("B8") 
B8G3$Garment <- c("G3")
B8G3$Band <- c("B8") 
B8G4$Garment <- c("G4")
B8G4$Band <- c("B8") 
B8G5$Garment <- c("G5")
B8G5$Band <- c("B8") 

B9G1$Garment <- c("G1")
B9G1$Band <- c("B9") 
B9G2$Garment <- c("G2")
B9G2$Band <- c("B9") 
B9G3$Garment <- c("G3")
B9G3$Band <- c("B9") 
B9G4$Garment <- c("G4")
B9G4$Band <- c("B9") 
B9G5$Garment <- c("G5")
B9G5$Band <- c("B9") 

B10G1$Garment <- c("G1")
B10G1$Band <- c("B10") 
B10G2$Garment <- c("G2")
B10G2$Band <- c("B10") 
B10G3$Garment <- c("G3")
B10G3$Band <- c("B10") 
B10G4$Garment <- c("G4")
B10G4$Band <- c("B10") 
B10G5$Garment <- c("G5")
B10G5$Band <- c("B10") 

B1G1y$Garment <- c("G1y")
B1G1y$Band <- c("B1") 
B3G1y$Garment <- c("G1y")
B3G1y$Band <- c("B3") 
B6G1y$Garment <- c("G1y")
B6G1y$Band <- c("B6") 
B8G1y$Garment <- c("G1y")
B8G1y$Band <- c("B8") 

# Applied previous calulation to bands that are missing
# Combined data per garments
Garment1 <- rbind(B1G1,B2G1,B3G1,B4G1,B5G1,B6G1,B7G1,B8G1,B9G1,B10G1)
#Garment1[,1:2] = apply(Garment1[,1:2], 2, function(x) as.numeric(as.character(x)));
Garment2 <- rbind(B1G2,B2G2,B3G2,B4G2,B5G2,B6G2,B7G2,B8G2,B9G2,B10G2)
#Garment2[,1:2] = apply(Garment2[,1:2], 2, function(x) as.numeric(as.character(x)));
Garment3 <- rbind(B1G3,B2G3,B3G3,B4G3,B5G3,B6G3,B7G3,B8G3,B9G3,B10G3)
#Garment3[,1:2] = apply(Garment3[,1:2], 2, function(x) as.numeric(as.character(x)));
Garment4 <- rbind(B1G4,B2G4,B3G4,B4G4,B5G4,B6G4,B7G4,B8G4,B9G4,B10G4)
#Garment4[,1:2] = apply(Garment4[,1:2], 2, function(x) as.numeric(as.character(x)));
Garment5 <- rbind(B1G5,B2G5,B3G5,B4G5,B5G5,B6G5,B7G5,B8G5,B9G5,B10G5)
#Garment5[,1:2] = apply(Garment5[,1:2], 2, function(x) as.numeric(as.character(x)));
ParallelcontactareasG1 <- rbind(B1G1,B2G1,B3G1,B4G1,B5G1)
ParallelcontactareasG2 <- rbind(B1G2,B2G2,B3G2,B4G2,B5G2)
ParallelcontactareasG3 <- rbind(B1G3,B2G3,B3G3,B4G3,B5G3)
ParallelcontactareasG4 <- rbind(B1G4,B2G4,B3G4,B4G4,B5G4)
ParallelcontactareasG5 <- rbind(B1G5,B2G5,B3G5,B4G5,B5G5)

PerpendicularcontactareasG1 <- rbind(B6G1,B7G1,B8G1,B9G1,B10G1)
PerpendicularcontactareasG2 <- rbind(B6G2,B7G2,B8G2,B9G2,B10G2)
PerpendicularcontactareasG3 <- rbind(B6G3,B7G3,B8G3,B9G3,B10G3)
PerpendicularcontactareasG4 <- rbind(B6G4,B7G4,B8G4,B9G4,B10G4)
PerpendicularcontactareasG5 <- rbind(B6G5,B7G5,B8G5,B9G5,B10G5)

B1allG <-  rbind(B1G1,B1G2,B1G3,B1G4,B1G5)
B2allG <-  rbind(B2G1,B2G2,B2G3,B2G4,B2G5)
B3allG <-  rbind(B3G1,B3G2,B3G3,B3G4,B3G5)
B4allG <-  rbind(B4G1,B4G2,B4G3,B4G4,B4G5)
B5allG <-  rbind(B5G1,B5G2,B5G3,B5G4,B5G5)
B6allG <-  rbind(B6G1,B6G2,B6G3,B6G4,B6G5)
B7allG <-  rbind(B7G1,B7G2,B7G3,B7G4,B7G5)
B8allG <-  rbind(B8G1,B8G2,B8G3,B8G4,B8G5)
B9allG <-  rbind(B9G1,B9G2,B9G3,B9G4,B9G5)
B10allG <-  rbind(B10G1,B10G2,B10G3,B10G4,B10G5)

# Aggregate data per garment and per band
Garment1reduced<- aggregate(Garment1$µs, by=list(Garment1$Band), FUN=mean)
Garment1reduced<- Garment1reduced %>% bind_rows(slice(., 2)) %>% slice(-2)
Garment2reduced<- aggregate(Garment2$µs, by=list(Garment2$Band), FUN=mean)
Garment2reduced<- Garment2reduced %>% bind_rows(slice(., 2)) %>% slice(-2)
Garment3reduced<- aggregate(Garment3$µs, by=list(Garment3$Band), FUN=mean)
Garment3reduced<- Garment3reduced %>% bind_rows(slice(., 2)) %>% slice(-2)
Garment4reduced<- aggregate(Garment4$µs, by=list(Garment4$Band), FUN=mean)
Garment4reduced<- Garment4reduced %>% bind_rows(slice(., 2)) %>% slice(-2)
Garment5reduced<- aggregate(Garment5$µs, by=list(Garment5$Band), FUN=mean)
Garment5reduced<- Garment5reduced %>% bind_rows(slice(., 2)) %>% slice(-2)

Garment1reduced2<- aggregate(Garment1$µs, by=list(Garment1$Band), FUN=SD)
Garment1reduced2<- Garment1reduced2 %>% bind_rows(slice(., 2)) %>% slice(-2)
Garment2reduced2<- aggregate(Garment2$µs, by=list(Garment2$Band), FUN=SD)
Garment2reduced2<- Garment2reduced2 %>% bind_rows(slice(., 2)) %>% slice(-2)
Garment3reduced2<- aggregate(Garment3$µs, by=list(Garment3$Band), FUN=SD)
Garment3reduced2<- Garment3reduced2 %>% bind_rows(slice(., 2)) %>% slice(-2)
Garment4reduced2<- aggregate(Garment4$µs, by=list(Garment4$Band), FUN=SD)
Garment4reduced2<- Garment4reduced2 %>% bind_rows(slice(., 2)) %>% slice(-2)
Garment5reduced2<- aggregate(Garment5$µs, by=list(Garment5$Band), FUN=SD)
Garment5reduced2<- Garment5reduced2 %>% bind_rows(slice(., 2)) %>% slice(-2)

Garment1Parareduced<- aggregate(ParallelcontactareasG1$µs, by=list(ParallelcontactareasG1$Garment), FUN=mean)
Garment1Parareduced2<- aggregate(ParallelcontactareasG1$µs, by=list(ParallelcontactareasG1$Garment), FUN=SD)
Garment2Parareduced<- aggregate(ParallelcontactareasG2$µs, by=list(ParallelcontactareasG2$Garment), FUN=mean)
Garment2Parareduced2<- aggregate(ParallelcontactareasG2$µs, by=list(ParallelcontactareasG2$Garment), FUN=SD)
Garment3Parareduced<- aggregate(ParallelcontactareasG3$µs, by=list(ParallelcontactareasG3$Garment), FUN=mean)
Garment3Parareduced2<- aggregate(ParallelcontactareasG3$µs, by=list(ParallelcontactareasG3$Garment), FUN=SD)
Garment4Parareduced<- aggregate(ParallelcontactareasG4$µs, by=list(ParallelcontactareasG4$Garment), FUN=mean)
Garment4Parareduced2<- aggregate(ParallelcontactareasG4$µs, by=list(ParallelcontactareasG4$Garment), FUN=SD)
Garment5Parareduced<- aggregate(ParallelcontactareasG5$µs, by=list(ParallelcontactareasG5$Garment), FUN=mean)
Garment5Parareduced2<- aggregate(ParallelcontactareasG5$µs, by=list(ParallelcontactareasG5$Garment), FUN=SD)

Garment1Perpreduced<- aggregate(PerpendicularcontactareasG1$µs, by=list(PerpendicularcontactareasG1$Garment), FUN=mean)
Garment1Perpreduced2<- aggregate(PerpendicularcontactareasG1$µs, by=list(PerpendicularcontactareasG1$Garment), FUN=SD)
Garment2Perpreduced<- aggregate(PerpendicularcontactareasG2$µs, by=list(PerpendicularcontactareasG2$Garment), FUN=mean)
Garment2Perpreduced2<- aggregate(PerpendicularcontactareasG2$µs, by=list(PerpendicularcontactareasG2$Garment), FUN=SD)
Garment3Perpreduced<- aggregate(PerpendicularcontactareasG3$µs, by=list(PerpendicularcontactareasG3$Garment), FUN=mean)
Garment3Perpreduced2<- aggregate(PerpendicularcontactareasG3$µs, by=list(PerpendicularcontactareasG3$Garment), FUN=SD)
Garment4Perpreduced<- aggregate(PerpendicularcontactareasG4$µs, by=list(PerpendicularcontactareasG4$Garment), FUN=mean)
Garment4Perpreduced2<- aggregate(PerpendicularcontactareasG4$µs, by=list(PerpendicularcontactareasG4$Garment), FUN=SD)
Garment5Perpreduced<- aggregate(PerpendicularcontactareasG5$µs, by=list(PerpendicularcontactareasG5$Garment), FUN=mean)
Garment5Perpreduced2<- aggregate(PerpendicularcontactareasG5$µs, by=list(PerpendicularcontactareasG5$Garment), FUN=SD)

# combined mean and SD in a single column with the separator ±
Garment1combined <- data.frame(cbind(round(Garment1reduced$x, digit=2), round(Garment1reduced2$x, digit=2)))
Garment1combined$Tot <- paste(Garment1combined$X1, Garment1combined$X2, sep=" ± ")
Garment2combined <- data.frame(cbind(round(Garment2reduced$x, digit=2),round(Garment2reduced2$x, digit=2)))
Garment2combined$Tot <- paste(Garment2combined$X1, Garment2combined$X2, sep=" ± ")
Garment3combined <- data.frame(cbind(round(Garment3reduced$x, digit=2),round(Garment3reduced2$x, digit=2)))
Garment3combined$Tot <- paste(Garment3combined$X1, Garment3combined$X2, sep=" ± ")
Garment4combined <- data.frame(cbind(round(Garment4reduced$x, digit=2),round(Garment4reduced2$x, digit=2)))
Garment4combined$Tot <- paste(Garment4combined$X1, Garment4combined$X2, sep=" ± ")
Garment5combined <- data.frame(cbind(round(Garment5reduced$x, digit=2),round(Garment5reduced2$x, digit=2)))
Garment5combined$Tot <- paste(Garment5combined$X1, Garment5combined$X2, sep=" ± ")
TotalGarment <- data.frame(cbind(Garment1combined$Tot,Garment2combined$Tot,Garment3combined$Tot,Garment4combined$Tot,Garment5combined$Tot))
names(TotalGarment) <- c("G1","G2","G3","G4","G5")
write.table(TotalGarment, file = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/TotalGarment_static coefficient_red jumpers.csv", quote = F, sep = ",", row.names = F)

# combined mean and SD in a single column with the separator ±
Garment1Paracombined <- data.frame(cbind(round(Garment1Parareduced$x, digit=2),round(Garment1Parareduced2$x, digit=2)))
Garment1Paracombined$Tot <- paste(Garment1Paracombined$X1, Garment1Paracombined$X2, sep=" ± ")
Garment2Paracombined <- data.frame(cbind(round(Garment2Parareduced$x, digit=2),round(Garment2Parareduced2$x, digit=2)))
Garment2Paracombined$Tot <- paste(Garment2Paracombined$X1, Garment2Paracombined$X2, sep=" ± ")
Garment3Paracombined <- data.frame(cbind(round(Garment3Parareduced$x, digit=2),round(Garment3Parareduced2$x, digit=2)))
Garment3Paracombined$Tot <- paste(Garment3Paracombined$X1, Garment3Paracombined$X2, sep=" ± ")
Garment4Paracombined <- data.frame(cbind(round(Garment4Parareduced$x, digit=2),round(Garment4Parareduced2$x, digit=2)))
Garment4Paracombined$Tot <- paste(Garment4Paracombined$X1, Garment4Paracombined$X2, sep=" ± ")
Garment5Paracombined <- data.frame(cbind(round(Garment5Parareduced$x, digit=2),round(Garment5Parareduced2$x, digit=2)))
Garment5Paracombined$Tot <- paste(Garment5Paracombined$X1, Garment5Paracombined$X2, sep=" ± ")

Garment1Perpcombined <- data.frame(cbind(round(Garment1Perpreduced$x, digit=2),round(Garment1Perpreduced2$x, digit=2)))
Garment1Perpcombined$Tot <- paste(Garment1Perpcombined$X1, Garment1Perpcombined$X2, sep=" ± ")
Garment2Perpcombined <- data.frame(cbind(round(Garment2Perpreduced$x, digit=2),round(Garment2Perpreduced2$x, digit=2)))
Garment2Perpcombined$Tot <- paste(Garment2Perpcombined$X1, Garment2Perpcombined$X2, sep=" ± ")
Garment3Perpcombined <- data.frame(cbind(round(Garment3Perpreduced$x, digit=2),round(Garment3Perpreduced2$x, digit=2)))
Garment3Perpcombined$Tot <- paste(Garment3Perpcombined$X1, Garment3Perpcombined$X2, sep=" ± ")
Garment4Perpcombined <- data.frame(cbind(round(Garment4Perpreduced$x, digit=2),round(Garment4Perpreduced2$x, digit=2)))
Garment4Perpcombined$Tot <- paste(Garment4Perpcombined$X1, Garment4Perpcombined$X2, sep=" ± ")
Garment5Perpcombined <- data.frame(cbind(round(Garment5Perpreduced$x, digit=2),round(Garment5Perpreduced2$x, digit=2)))
Garment5Perpcombined$Tot <- paste(Garment5Perpcombined$X1, Garment5Perpcombined$X2, sep=" ± ")

TotalGarment_pending <- data.frame(cbind(Garment1Paracombined$Tot,Garment2Paracombined$Tot,Garment3Paracombined$Tot,Garment4Paracombined$Tot,Garment5Paracombined$Tot))
TotalGarment_pending2 <- data.frame(cbind(Garment1Perpcombined$Tot,Garment2Perpcombined$Tot,Garment3Perpcombined$Tot,Garment4Perpcombined$Tot,Garment5Perpcombined$Tot))
TotalGarment <- rbind(TotalGarment_pending,TotalGarment_pending2)
names(TotalGarment) <- c("G1","G2","G3","G4","G5")
write.table(TotalGarment, file = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/TotalGarmentorientation_static coefficient_red jumpers.csv", quote = F, sep = ",", row.names = F)

#### Table Chapter 6 ####
# Aggregate data per garment and per band
B1G1yreduced<- aggregate(B1G1y$µs, by=list(B1G1y$Band), FUN=mean)
B3G1yreduced<- aggregate(B3G1y$µs, by=list(B3G1y$Band), FUN=mean)
B6G1yreduced<- aggregate(B6G1y$µs, by=list(B6G1y$Band), FUN=mean)
B8G1yreduced<- aggregate(B8G1y$µs, by=list(B8G1y$Band), FUN=mean)

B1allGreduced<- aggregate(B1allG$µs, by=list(B1allG$Band), FUN=mean)
B2allGreduced<- aggregate(B2allG$µs, by=list(B2allG$Band), FUN=mean)
B3allGreduced<- aggregate(B3allG$µs, by=list(B3allG$Band), FUN=mean)
B4allGreduced<- aggregate(B4allG$µs, by=list(B4allG$Band), FUN=mean)
B5allGreduced<- aggregate(B5allG$µs, by=list(B5allG$Band), FUN=mean)
B6allGreduced<- aggregate(B6allG$µs, by=list(B6allG$Band), FUN=mean)
B7allGreduced<- aggregate(B7allG$µs, by=list(B7allG$Band), FUN=mean)
B8allGreduced<- aggregate(B8allG$µs, by=list(B8allG$Band), FUN=mean)
B9allGreduced<- aggregate(B9allG$µs, by=list(B9allG$Band), FUN=mean)
B10allGreduced<- aggregate(B10allG$µs, by=list(B10allG$Band), FUN=mean)


B1G1yreduced2<- aggregate(B1G1y$µs, by=list(B1G1y$Band), FUN=sd)
B3G1yreduced2<- aggregate(B3G1y$µs, by=list(B3G1y$Band), FUN=sd)
B6G1yreduced2<- aggregate(B6G1y$µs, by=list(B6G1y$Band), FUN=sd)
B8G1yreduced2<- aggregate(B8G1y$µs, by=list(B8G1y$Band), FUN=sd)
B1allGreduced2<- aggregate(B1allG$µs, by=list(B1allG$Band), FUN=sd)
B2allGreduced2<- aggregate(B2allG$µs, by=list(B2allG$Band), FUN=sd)
B3allGreduced2<- aggregate(B3allG$µs, by=list(B3allG$Band), FUN=sd)
B4allGreduced2<- aggregate(B4allG$µs, by=list(B4allG$Band), FUN=sd)
B5allGreduced2<- aggregate(B5allG$µs, by=list(B5allG$Band), FUN=sd)
B6allGreduced2<- aggregate(B6allG$µs, by=list(B6allG$Band), FUN=sd)
B7allGreduced2<- aggregate(B7allG$µs, by=list(B7allG$Band), FUN=sd)
B8allGreduced2<- aggregate(B8allG$µs, by=list(B8allG$Band), FUN=sd)
B9allGreduced2<- aggregate(B9allG$µs, by=list(B9allG$Band), FUN=sd)
B10allGreduced2<- aggregate(B10allG$µs, by=list(B10allG$Band), FUN=sd)

B1G1ycombined <- data.frame(cbind(round(B1G1yreduced$x, digit=2), round(B1G1yreduced2$x, digit=2)))
B1G1ycombined$Tot <- paste(B1G1ycombined$X1, B1G1ycombined$X2, sep=" ± ")
B3G1ycombined <- data.frame(cbind(round(B3G1yreduced$x, digit=2), round(B3G1yreduced2$x, digit=2)))
B3G1ycombined$Tot <- paste(B3G1ycombined$X1, B3G1ycombined$X2, sep=" ± ")
B6G1ycombined <- data.frame(cbind(round(B6G1yreduced$x, digit=2), round(B6G1yreduced2$x, digit=2)))
B6G1ycombined$Tot <- paste(B6G1ycombined$X1, B6G1ycombined$X2, sep=" ± ")
B8G1ycombined <- data.frame(cbind(round(B8G1yreduced$x, digit=2), round(B8G1yreduced2$x, digit=2)))
B8G1ycombined$Tot <- paste(B8G1ycombined$X1, B8G1ycombined$X2, sep=" ± ")

B1allGcombined <- data.frame(cbind(round(B1allGreduced$x, digit=2), round(B1allGreduced2$x, digit=2)))
B1allGcombined$Tot <- paste(B1allGcombined$X1, B1allGcombined$X2, sep=" ± ")
B2allGcombined <- data.frame(cbind(round(B2allGreduced$x, digit=2), round(B2allGreduced2$x, digit=2)))
B2allGcombined$Tot <- paste(B2allGcombined$X1, B2allGcombined$X2, sep=" ± ")
B3allGcombined <- data.frame(cbind(round(B3allGreduced$x, digit=2), round(B3allGreduced2$x, digit=2)))
B3allGcombined$Tot <- paste(B3allGcombined$X1, B3allGcombined$X2, sep=" ± ")
B4allGcombined <- data.frame(cbind(round(B4allGreduced$x, digit=2), round(B4allGreduced2$x, digit=2)))
B4allGcombined$Tot <- paste(B4allGcombined$X1, B4allGcombined$X2, sep=" ± ")
B5allGcombined <- data.frame(cbind(round(B5allGreduced$x, digit=2), round(B5allGreduced2$x, digit=2)))
B5allGcombined$Tot <- paste(B5allGcombined$X1, B5allGcombined$X2, sep=" ± ")
B6allGcombined <- data.frame(cbind(round(B6allGreduced$x, digit=2), round(B6allGreduced2$x, digit=2)))
B6allGcombined$Tot <- paste(B6allGcombined$X1, B6allGcombined$X2, sep=" ± ")
B7allGcombined <- data.frame(cbind(round(B7allGreduced$x, digit=2), round(B7allGreduced2$x, digit=2)))
B7allGcombined$Tot <- paste(B7allGcombined$X1, B7allGcombined$X2, sep=" ± ")
B8allGcombined <- data.frame(cbind(round(B8allGreduced$x, digit=2), round(B8allGreduced2$x, digit=2)))
B8allGcombined$Tot <- paste(B8allGcombined$X1, B8allGcombined$X2, sep=" ± ")
B9allGcombined <- data.frame(cbind(round(B9allGreduced$x, digit=2), round(B9allGreduced2$x, digit=2)))
B9allGcombined$Tot <- paste(B9allGcombined$X1, B9allGcombined$X2, sep=" ± ")
B10allGcombined <- data.frame(cbind(round(B10allGreduced$x, digit=2), round(B10allGreduced2$x, digit=2)))
B10allGcombined$Tot <- paste(B10allGcombined$X1, B10allGcombined$X2, sep=" ± ")

parayellow <- rbind(B1G1y,B3G1y)
parayellowreduced<- mean(parayellow$µs);parayellowreduced
parayellowreduced2<- sd(parayellow$µs);parayellowreduced2
perpyellow <- rbind(B6G1y,B8G1y)
perpyellowreduced<- mean(perpyellow$µs);perpyellowreduced
perpyellowreduced2<- sd(perpyellow$µs);perpyellowreduced2

parared <- rbind(ParallelcontactareasG1,ParallelcontactareasG2,ParallelcontactareasG3,ParallelcontactareasG4,ParallelcontactareasG5)
pararedreduced<- mean(parared$µs);pararedreduced
pararedreduced2<- sd(parared$µs);pararedreduced2
perpred <- rbind(PerpendicularcontactareasG1,PerpendicularcontactareasG2,PerpendicularcontactareasG3,PerpendicularcontactareasG4,PerpendicularcontactareasG5)
perpredreduced<- mean(perpred$µs);perpredreduced
perpredreduced2<- sd(perpred$µs);perpredreduced2

yellow <- data.frame(rbind(B1G1ycombined$Tot,B3G1ycombined$Tot,B6G1ycombined$Tot,B8G1ycombined$Tot))
red <- data.frame(rbind(B1allGcombined$Tot,B2allGcombined$Tot, B3allGcombined$Tot,B4allGcombined$Tot,B5allGcombined$Tot,
             B6allGcombined$Tot,B7allGcombined$Tot, B8allGcombined$Tot,B9allGcombined$Tot,B10allGcombined$Tot))
redreduced <- data.frame(rbind(B1allGcombined$Tot,B3allGcombined$Tot,B6allGcombined$Tot,B8allGcombined$Tot))

Bothgarment <- cbind(yellow,redreduced)
names(Bothgarment) <- c("Yellow","Red")
#write.table(Bothgarment, file = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/Staticcoeff- red+yellow.csv", quote = F, sep = ",", row.names = F)
#write.table(red, file = "C:/Users/2395804/OneDrive - University of Dundee/Desktop/PhD Results/Transfer project/Labwork analysis/Results/Arduino/Staticcoeff- red.csv", quote = F, sep = ",", row.names = F)
