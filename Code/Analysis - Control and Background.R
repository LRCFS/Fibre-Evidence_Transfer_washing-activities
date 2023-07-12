#########################################################
#####           Fibre transfer analysis             #####
#########################################################
# Assign a Coder to each wash number
coder_ids <- list(
  G1 = c("W000", "W001", "W002", "W003", "W004", "W005", "W006", "W007", "W009", "W011"),
  G5 = c("W000", "W001", "W002", "W003", "W004", "W005", "W006", "W007", "W008", "W009",
         "W010", "W011", "W012", "W013", "W014", "W015", "W016", "W017", "W018", "W019",
         "W020", "W021", "W022", "W023", "W024", "W025", "W026", "W027", "W028", "W029",
         "W030", "W031", "W032", "W033", "W034", "W035", "W036", "W037", "W038", "W039",
         "W040", "W041", "W042", "W043", "W044", "W045", "W046", "W047", "W048", "W049",
         "W050", "W051"),
  G12A = c("W000", "W001", "W002", "W003", "W004", "W005", "W006"),
  G12B = c("W000", "W001", "W002", "W003", "W004", "W005", "W006"),
  G12C = c("W000", "W001", "W002", "W003", "W004", "W005", "W006")
)
# Iterate over the coder_ids list and modify the respective data frames
lapply(names(coder_ids), function(coder) {
  lapply(coder_ids[[coder]], function(id) {
    df_name <- paste0(id, "_", coder, "_Dataset")
    if (exists(df_name) && !is.null(get(df_name))) {
      assign(df_name, get(df_name) %>% mutate(Coder = id), envir = .GlobalEnv)
    }
  })
})

# Assign a Coder to each garment number
coder_ids <- c("G1", "G5", "G12A", "G12B", "G12C")
# Loop through the coder IDs and dataset numbers
for (coder_id in coder_ids) {
  for (i in 0:51) {
    df_name <- paste0("W", sprintf("%03d", i), "_", coder_id, "_Dataset")
    if (exists(df_name)) {
      assign(df_name, get(df_name) %>% mutate(Coder2 = coder_id), envir = .GlobalEnv)
    }
  }
}

#########################################################
#####                    Controls                   #####
#########################################################
# Define a function to filter negative controls from a dataset
filter_negative <- function(dataset) {
  dataset %>% filter(grepl('negative', Sample))
}

# Select the negative controls from each dataset
datasets <- c(
  paste0("W", sprintf("%03d", c(0:7, 9, 11)), "_G1_Dataset"),
  paste0("W", sprintf("%03d", c(0:51)), "_G5_Dataset"),
  paste0("W", sprintf("%03d", 0:6), "_G12A_Dataset"),
  paste0("W", sprintf("%03d", 0:6), "_G12B_Dataset"),
  paste0("W", sprintf("%03d", 0:6), "_G12C_Dataset")
)
for (dataset in datasets) {
  if (exists(dataset)) {
    assign(paste0(dataset, "_negative"), filter_negative(get(dataset)))
  }
}
# Combined negative swatches
combined_negative <- bind_rows(mget(paste0(datasets, "_negative")))

# Select the positive controls from each datasets
# Define a function to filter positive controls from a dataset
filter_positive <- function(dataset) {
  dataset %>% filter(grepl('positive', Sample))
}
# Select the positive controls from each dataset
for (dataset in datasets) {
  assign(paste0(dataset, "_positive"), filter_positive(get(dataset)))
}
# Combined positive swatches
combined_positive <- bind_rows(mget(paste0(datasets, "_positive")))

# Calculate the changes in number of fibers on both controls
combined_negative$Diff <- combined_negative$`After transfer` - combined_negative$`Before transfer`
combined_positive$Diff2 <- combined_positive$`After transfer` - combined_positive$`Before transfer`

# Create a data frame with all the controls
Total_controls <- data.frame(
  Sample = combined_negative$Sample,
  Coder = combined_negative$Coder,
  Coder2 = combined_negative$Coder2,
  Diffneg = as.numeric(as.character(combined_negative$Diff)),
  Diffpos = as.numeric(as.character(combined_positive$Diff2))
)

# Categorize the controls
control_categories <- c(
  "Contamination on negative control",
  "Contamination on positive control",
  "Contamination on both controls",
  "Possible fibre(s) movement from + to -",
  "Possible fibre(s) movement from - to +",
  "Fibre loss on negative control only",
  "Fibre loss on positive controls only",
  "Fibre loss on both controls",
  "no changes"
)
  
# Create a new column for categorization
Total_controls$Coder3 <- control_categories[1]
for (i in 2:length(control_categories)) {
  Total_controls$Coder3[Total_controls$Diffneg > 0 & Total_controls$Diffpos == 0] <- control_categories[1]
  Total_controls$Coder3[Total_controls$Diffneg > 0 & Total_controls$Diffpos > 0] <- control_categories[3]
  Total_controls$Coder3[Total_controls$Diffneg > 0 & Total_controls$Diffpos < 0] <- control_categories[4]
  Total_controls$Coder3[Total_controls$Diffneg < 0 & Total_controls$Diffpos == 0] <- control_categories[6]
  Total_controls$Coder3[Total_controls$Diffneg < 0 & Total_controls$Diffpos > 0] <- control_categories[5]
  Total_controls$Coder3[Total_controls$Diffneg < 0 & Total_controls$Diffpos < 0] <- control_categories[8]
  Total_controls$Coder3[Total_controls$Diffneg == 0 & Total_controls$Diffpos < 0] <- control_categories[7]
  Total_controls$Coder3[Total_controls$Diffneg == 0 & Total_controls$Diffpos > 0] <- control_categories[2]
  Total_controls$Coder3[Total_controls$Diffneg == 0 & Total_controls$Diffpos  ==  0] <- control_categories[9]
}

# Separate controls into different data frames based on categories
Controls_contamination <- Total_controls[Total_controls$Coder3 %in% control_categories[1:3], ]
Controls_movement <- Total_controls[Total_controls$Coder3 %in% control_categories[4:5], ]
Controls_loss <- Total_controls[Total_controls$Coder3 %in% control_categories[6:8], ]

# plot the controls
Controls_contamination <-  aggregate(Controls_contamination$Sample,list(Controls_contamination$Coder3), FUN=length)
Plot_Controls <- ggplot(Controls_contamination, aes(x = Group.1, y = x)) + 
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) + 
  geom_text(aes(label = x), vjust = -0.5)+
  geom_hline(yintercept=0,linetype="dashed", color = "black")+
  labs(y= "Occurence", x="\nWash")+
  theme_bw(base_size = 12)+
  ylim(0,30)+
  theme(panel.grid.major.y = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.background = element_rect(color="grey", size=2),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
show(Plot_Controls)

Controls_loss <-  aggregate(Controls_loss$Sample,list(Controls_loss$Coder3), FUN=length)
Plot_Controls_2 <- ggplot(Controls_loss, aes(x = Group.1, y = x)) + 
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) +
  geom_text(aes(label = x), vjust = -0.5)+
  geom_hline(yintercept=0,linetype="dashed", color = "black")+
  labs(y= "Occurence", x="\nWash")+
  theme_bw(base_size = 12)+
  ylim(0,30)+
  theme(panel.grid.major.y = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.background = element_rect(color="grey", size=2),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
show(Plot_Controls_2)

Controls_movement <-  aggregate(Controls_movement$Sample,list(Controls_movement$Coder3), FUN=length)
Plot_Controls_3 <- ggplot(Controls_movement, aes(x = Group.1, y = x)) + 
  geom_bar(stat="identity", position=position_dodge(), width = 0.5) + 
  geom_text(aes(label = x), vjust = -0.5)+
  geom_hline(yintercept=0,linetype="dashed", color = "black")+
  labs(y= "Occurence", x="\nWash")+
  theme_bw(base_size = 12)+
  ylim(0,30)+
  theme(panel.grid.major.y = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.box.background = element_rect(color="grey", size=2),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
show(Plot_Controls_3)

# Combined results
pControls_pending <- ggarrange(Plot_Controls+ rremove("ylab") + rremove("xlab"),
                               Plot_Controls_2+ rremove("ylab") + rremove("xlab"),
                               Plot_Controls_3+ rremove("ylab") + rremove("xlab"),
                               labels = c("Possible contamination","Possible fibre loss", "Possible fibre movement"),
                               common.legend = TRUE, legend = "bottom",
                               align = "hv",
                               ncol = 1, nrow = 3,
                               font.label = list(size = 12, color = "black", family = "Arial", position = "top"),
                               hjust=-0.3,vjust=2)
pControls <- annotate_figure(pControls_pending, left = textGrob("Number of fibres", rot = 90, vjust = 0.5, hjust = 0.5, gp = gpar(cex =1)),
                             bottom = textGrob("Wash number", vjust = 0.5, hjust = 0.5,gp = gpar(cex = 1)));pControls
#ggsave("Controls.png", pControls, width = 6, height = 6, units = "in", dpi=150, path = "Results")

#########################################################
#####           ANALYSE OF THE BACKGROUND           #####
#########################################################

forFibreCount0_G1<- W000_G1_Dataset[!(W000_G1_Dataset$Sample=="MP_W000_G1_positive_B" | W000_G1_Dataset$Sample=="MP_W000_G1_negative_B"),]
forFibreCount1_G1<- W001_G1_Dataset[!(W001_G1_Dataset$Sample=="MP_W001_G1_positive_B" | W001_G1_Dataset$Sample=="MP_W001_G1_negative_B"),]
forFibreCount2_G1<- W002_G1_Dataset[!(W002_G1_Dataset$Sample=="MP_W002_G1_positive_B" | W002_G1_Dataset$Sample=="MP_W002_G1_negative_B"),]
forFibreCount3_G1<- W003_G1_Dataset[!(W003_G1_Dataset$Sample=="MP_W003_G1_positive_B" | W003_G1_Dataset$Sample=="MP_W003_G1_negative_B"),]
forFibreCount4_G1<- W004_G1_Dataset[!(W004_G1_Dataset$Sample=="MP_W004_G1_positive_B" | W004_G1_Dataset$Sample=="MP_W004_G1_negative_B"),]
forFibreCount5_G1<- W005_G1_Dataset[!(W005_G1_Dataset$Sample=="MP_W005_G1_positive_B" | W005_G1_Dataset$Sample=="MP_W005_G1_negative_B"),]
forFibreCount6_G1<- W006_G1_Dataset[!(W006_G1_Dataset$Sample=="MP_W006_G1_positive_B" | W006_G1_Dataset$Sample=="MP_W006_G1_negative_B"),]
forFibreCount7_G1<- W007_G1_Dataset[!(W007_G1_Dataset$Sample=="MP_W007_G1_positive_B" | W007_G1_Dataset$Sample=="MP_W007_G1_negative_B"),]
forFibreCount9_G1<- W009_G1_Dataset[!(W009_G1_Dataset$Sample=="MP_W009_G1_positive_B" | W009_G1_Dataset$Sample=="MP_W009_G1_negative_B"),]
forFibreCount11_G1<- W011_G1_Dataset[!(W011_G1_Dataset$Sample=="MP_W011_G1_positive_B" | W011_G1_Dataset$Sample=="MP_W011_G1_negative_B"),]
# forFibreCount13_G1<- W013_G1_Dataset[!(W013_G1_Dataset$Sample=="MP_W001_G1_positive_B" | W001_G1_Dataset$Sample=="MP_W001_G1_negative_B"),]
# forFibreCount15_G1<- W015_G1_Dataset[!(W015_G1_Dataset$Sample=="MP_W003_G1_positive_B" | W003_G1_Dataset$Sample=="MP_W003_G1_negative_B"),]

# Dataset with 5 garments
forFibreCount0_G5<- W000_G5_Dataset[!(W000_G5_Dataset$Sample=="W000_positive" | W000_G5_Dataset$Sample=="W000_negative"),]
forFibreCount1_G5<- W001_G5_Dataset[!(W001_G5_Dataset$Sample=="W001_positive" | W001_G5_Dataset$Sample=="W001_negative"),]
forFibreCount2_G5<- W002_G5_Dataset[!(W002_G5_Dataset$Sample=="W002_positive" | W002_G5_Dataset$Sample=="W002_negative"),]
forFibreCount3_G5<- W003_G5_Dataset[!(W003_G5_Dataset$Sample=="W003_positive" | W003_G5_Dataset$Sample=="W003_negative"),]
forFibreCount4_G5<- W004_G5_Dataset[!(W004_G5_Dataset$Sample=="W004_positive" | W004_G5_Dataset$Sample=="W004_negative"),]
forFibreCount5_G5<- W005_G5_Dataset[!(W005_G5_Dataset$Sample=="W005_positive" | W005_G5_Dataset$Sample=="W005_negative"),]
forFibreCount6_G5<- W006_G5_Dataset[!(W006_G5_Dataset$Sample=="W006_positive" | W006_G5_Dataset$Sample=="W006_negative"),]
forFibreCount7_G5<- W007_G5_Dataset[!(W007_G5_Dataset$Sample=="W007_positive" | W007_G5_Dataset$Sample=="W007_negative"),]
forFibreCount8_G5<- W008_G5_Dataset[!(W008_G5_Dataset$Sample=="W008_positive" | W008_G5_Dataset$Sample=="W008_negative"),]
forFibreCount9_G5<- W009_G5_Dataset[!(W009_G5_Dataset$Sample=="W009_positive" | W009_G5_Dataset$Sample=="W009_negative"),]
forFibreCount10_G5<- W010_G5_Dataset[!(W010_G5_Dataset$Sample=="W010_positive" | W010_G5_Dataset$Sample=="W010_negative"),]
forFibreCount11_G5<- W011_G5_Dataset[!(W011_G5_Dataset$Sample=="W011_positive" | W011_G5_Dataset$Sample=="W011_negative"),]
forFibreCount12_G5<- W012_G5_Dataset[!(W012_G5_Dataset$Sample=="W012_positive" | W012_G5_Dataset$Sample=="W012_negative"),]
forFibreCount13_G5<- W013_G5_Dataset[!(W013_G5_Dataset$Sample=="W013_positive" | W013_G5_Dataset$Sample=="W013_negative"),]
forFibreCount14_G5<- W014_G5_Dataset[!(W014_G5_Dataset$Sample=="W014_positive" | W014_G5_Dataset$Sample=="W014_negative"),]
forFibreCount15_G5<- W015_G5_Dataset[!(W015_G5_Dataset$Sample=="W015_positive" | W015_G5_Dataset$Sample=="W015_negative"),]
forFibreCount16_G5<- W016_G5_Dataset[!(W016_G5_Dataset$Sample=="W016_positive" | W016_G5_Dataset$Sample=="W016_negative"),]
forFibreCount17_G5<- W017_G5_Dataset[!(W017_G5_Dataset$Sample=="W017_positive" | W017_G5_Dataset$Sample=="W017_negative"),]
forFibreCount18_G5<- W018_G5_Dataset[!(W018_G5_Dataset$Sample=="W018_positive" | W018_G5_Dataset$Sample=="W018_negative"),]
forFibreCount19_G5<- W019_G5_Dataset[!(W019_G5_Dataset$Sample=="W019_positive" | W019_G5_Dataset$Sample=="W019_negative"),]
forFibreCount20_G5<- W020_G5_Dataset[!(W020_G5_Dataset$Sample=="W020_positive" | W020_G5_Dataset$Sample=="W020_negative"),]
forFibreCount21_G5<- W021_G5_Dataset[!(W021_G5_Dataset$Sample=="W021_positive" | W021_G5_Dataset$Sample=="W021_negative"),]
forFibreCount22_G5<- W022_G5_Dataset[!(W022_G5_Dataset$Sample=="W022_positive" | W022_G5_Dataset$Sample=="W022_negative"),]
forFibreCount23_G5<- W023_G5_Dataset[!(W023_G5_Dataset$Sample=="W023_positive" | W023_G5_Dataset$Sample=="W023_negative"),]
forFibreCount24_G5<- W024_G5_Dataset[!(W024_G5_Dataset$Sample=="W024_positive" | W024_G5_Dataset$Sample=="W024_negative"),]
forFibreCount25_G5<- W025_G5_Dataset[!(W025_G5_Dataset$Sample=="W025_positive" | W025_G5_Dataset$Sample=="W025_negative"),]
forFibreCount26_G5<- W026_G5_Dataset[!(W026_G5_Dataset$Sample=="W026_positive" | W026_G5_Dataset$Sample=="W026_negative"),]
forFibreCount27_G5<- W027_G5_Dataset[!(W027_G5_Dataset$Sample=="W027_positive" | W027_G5_Dataset$Sample=="W027_negative"),]
forFibreCount28_G5<- W028_G5_Dataset[!(W028_G5_Dataset$Sample=="W028_positive" | W028_G5_Dataset$Sample=="W028_negative"),]
forFibreCount29_G5<- W029_G5_Dataset[!(W029_G5_Dataset$Sample=="W029_positive" | W029_G5_Dataset$Sample=="W029_negative"),]
forFibreCount30_G5<- W030_G5_Dataset[!(W030_G5_Dataset$Sample=="W030_positive" | W030_G5_Dataset$Sample=="W030_negative"),]
forFibreCount31_G5<- W031_G5_Dataset[!(W031_G5_Dataset$Sample=="W031_positive" | W031_G5_Dataset$Sample=="W031_negative"),]
forFibreCount32_G5<- W032_G5_Dataset[!(W032_G5_Dataset$Sample=="W032_positive" | W032_G5_Dataset$Sample=="W032_negative"),]
forFibreCount33_G5<- W033_G5_Dataset[!(W033_G5_Dataset$Sample=="W033_positive" | W033_G5_Dataset$Sample=="W033_negative"),]
forFibreCount34_G5<- W034_G5_Dataset[!(W034_G5_Dataset$Sample=="W034_positive" | W034_G5_Dataset$Sample=="W034_negative"),]
forFibreCount35_G5<- W035_G5_Dataset[!(W035_G5_Dataset$Sample=="W035_positive" | W035_G5_Dataset$Sample=="W035_negative"),]
forFibreCount36_G5<- W036_G5_Dataset[!(W036_G5_Dataset$Sample=="W036_positive" | W036_G5_Dataset$Sample=="W036_negative"),]
forFibreCount37_G5<- W037_G5_Dataset[!(W037_G5_Dataset$Sample=="W037_positive" | W037_G5_Dataset$Sample=="W037_negative"),]
forFibreCount38_G5<- W038_G5_Dataset[!(W038_G5_Dataset$Sample=="W038_positive" | W038_G5_Dataset$Sample=="W038_negative"),]
forFibreCount39_G5<- W039_G5_Dataset[!(W039_G5_Dataset$Sample=="W039_positive" | W039_G5_Dataset$Sample=="W039_negative"),]
forFibreCount40_G5<- W040_G5_Dataset[!(W040_G5_Dataset$Sample=="W040_positive" | W040_G5_Dataset$Sample=="W040_negative"),]
forFibreCount41_G5<- W041_G5_Dataset[!(W041_G5_Dataset$Sample=="W041_positive" | W041_G5_Dataset$Sample=="W041_negative"),]
forFibreCount42_G5<- W042_G5_Dataset[!(W042_G5_Dataset$Sample=="W042_positive" | W042_G5_Dataset$Sample=="W042_negative"),]
forFibreCount43_G5<- W043_G5_Dataset[!(W043_G5_Dataset$Sample=="W043_positive" | W043_G5_Dataset$Sample=="W043_negative"),]
forFibreCount44_G5<- W044_G5_Dataset[!(W044_G5_Dataset$Sample=="W044_positive" | W044_G5_Dataset$Sample=="W044_negative"),]
forFibreCount45_G5<- W045_G5_Dataset[!(W045_G5_Dataset$Sample=="W045_positive" | W045_G5_Dataset$Sample=="W045_negative"),]
forFibreCount46_G5<- W046_G5_Dataset[!(W046_G5_Dataset$Sample=="W046_positive" | W046_G5_Dataset$Sample=="W046_negative"),]
forFibreCount47_G5<- W047_G5_Dataset[!(W047_G5_Dataset$Sample=="W047_positive" | W047_G5_Dataset$Sample=="W047_negative"),]
forFibreCount48_G5<- W048_G5_Dataset[!(W048_G5_Dataset$Sample=="W048_positive" | W048_G5_Dataset$Sample=="W048_negative"),]
forFibreCount49_G5<- W049_G5_Dataset[!(W049_G5_Dataset$Sample=="W049_positive" | W049_G5_Dataset$Sample=="W049_negative"),]
forFibreCount50_G5<- W050_G5_Dataset[!(W050_G5_Dataset$Sample=="W050_positive" | W050_G5_Dataset$Sample=="W050_negative"),]
forFibreCount51_G5<- W051_G5_Dataset[!(W051_G5_Dataset$Sample=="W051_positive" | W051_G5_Dataset$Sample=="W051_negative"),]

# Dataset with 12 garments
forFibreCount0_G12A<- W000_G12A_Dataset[!(W000_G12A_Dataset$Sample=="MP_W000_G4A_positive_B" | W000_G12A_Dataset$Sample=="MP_W000_G4A_negative_B"),]
forFibreCount1_G12A<- W001_G12A_Dataset[!(W001_G12A_Dataset$Sample=="MP_W001_G4A_positive_B" | W001_G12A_Dataset$Sample=="MP_W001_G4A_negative_B"),]
forFibreCount2_G12A<- W002_G12A_Dataset[!(W002_G12A_Dataset$Sample=="MP_W002_G4A_positive_B" | W002_G12A_Dataset$Sample=="MP_W002_G4A_negative_B"),]
forFibreCount3_G12A<- W003_G12A_Dataset[!(W003_G12A_Dataset$Sample=="MP_W003_G4A_positive_B" | W003_G12A_Dataset$Sample=="MP_W003_G4A_negative_B"),]
forFibreCount4_G12A<- W004_G12A_Dataset[!(W004_G12A_Dataset$Sample=="MP_W004_G4A_positive_B" | W004_G12A_Dataset$Sample=="MP_W004_G4A_negative_B"),]
forFibreCount5_G12A<- W005_G12A_Dataset[!(W005_G12A_Dataset$Sample=="MP_W005_G4A_positive_B" | W005_G12A_Dataset$Sample=="MP_W005_G4A_negative_B"),]
forFibreCount0_G12B<- W000_G12B_Dataset[!(W000_G12B_Dataset$Sample=="MP_W000_G4A_positive_B" | W000_G12B_Dataset$Sample=="MP_W000_G4B_negative_B"),]
forFibreCount1_G12B<- W001_G12B_Dataset[!(W001_G12B_Dataset$Sample=="MP_W001_G4A_positive_B" | W001_G12B_Dataset$Sample=="MP_W001_G4B_negative_B"),]
forFibreCount2_G12B<- W002_G12B_Dataset[!(W002_G12B_Dataset$Sample=="MP_W002_G4A_positive_B" | W002_G12B_Dataset$Sample=="MP_W002_G4B_negative_B"),]
forFibreCount3_G12B<- W003_G12B_Dataset[!(W003_G12B_Dataset$Sample=="MP_W003_G4A_positive_B" | W003_G12B_Dataset$Sample=="MP_W003_G4B_negative_B"),]
forFibreCount4_G12B<- W004_G12B_Dataset[!(W004_G12B_Dataset$Sample=="MP_W004_G4A_positive_B" | W004_G12B_Dataset$Sample=="MP_W004_G4B_negative_B"),]
forFibreCount5_G12B<- W005_G12B_Dataset[!(W005_G12B_Dataset$Sample=="MP_W005_G4A_positive_B" | W005_G12B_Dataset$Sample=="MP_W005_G4B_negative_B"),]
forFibreCount0_G12C<- W000_G12C_Dataset[!(W000_G12C_Dataset$Sample=="MP_W000_G4C_positive_B" | W000_G12C_Dataset$Sample=="MP_W000_G4C_negative_B"),]
forFibreCount1_G12C<- W001_G12C_Dataset[!(W001_G12C_Dataset$Sample=="MP_W001_G4C_positive_B" | W001_G12C_Dataset$Sample=="MP_W001_G4C_negative_B"),]
forFibreCount2_G12C<- W002_G12C_Dataset[!(W002_G12C_Dataset$Sample=="MP_W002_G4C_positive_B" | W002_G12C_Dataset$Sample=="MP_W002_G4C_negative_B"),]
forFibreCount3_G12C<- W003_G12C_Dataset[!(W003_G12C_Dataset$Sample=="MP_W003_G4C_positive_B" | W003_G12C_Dataset$Sample=="MP_W003_G4C_negative_B"),]
forFibreCount4_G12C<- W004_G12C_Dataset[!(W004_G12C_Dataset$Sample=="MP_W004_G4C_positive_B" | W004_G12C_Dataset$Sample=="MP_W004_G4C_negative_B"),]
forFibreCount5_G12C<- W005_G12C_Dataset[!(W005_G12C_Dataset$Sample=="MP_W005_G4C_positive_B" | W005_G12C_Dataset$Sample=="MP_W005_G4C_negative_B"),]

#### select the column Before only in each dataframe ####
# Dataset with 1 garments
BackgroundW000_G1 <- forFibreCount0_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW001_G1 <- forFibreCount1_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW002_G1 <- forFibreCount2_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW003_G1 <- forFibreCount3_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW004_G1 <- forFibreCount4_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW005_G1 <- forFibreCount5_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW006_G1 <- forFibreCount6_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW007_G1 <- forFibreCount7_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW009_G1 <- forFibreCount9_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW011_G1 <- forFibreCount11_G1 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)

# Dataset with 5 garments
BackgroundW000_G5 <- forFibreCount0_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW001_G5 <- forFibreCount1_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW002_G5 <- forFibreCount2_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW003_G5 <- forFibreCount3_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW004_G5 <- forFibreCount4_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW005_G5 <- forFibreCount5_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW006_G5 <- forFibreCount6_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW007_G5 <- forFibreCount7_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW008_G5 <- forFibreCount8_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW009_G5 <- forFibreCount9_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW010_G5 <- forFibreCount10_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW011_G5 <- forFibreCount11_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW012_G5 <- forFibreCount12_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW013_G5 <- forFibreCount13_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW014_G5 <- forFibreCount14_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW015_G5 <- forFibreCount15_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW016_G5 <- forFibreCount16_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW017_G5 <- forFibreCount17_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW018_G5 <- forFibreCount18_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW019_G5 <- forFibreCount19_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW020_G5 <- forFibreCount20_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW021_G5 <- forFibreCount21_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW022_G5 <- forFibreCount22_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW023_G5 <- forFibreCount23_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW024_G5 <- forFibreCount24_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW025_G5 <- forFibreCount25_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW026_G5 <- forFibreCount26_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW027_G5 <- forFibreCount27_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW028_G5 <- forFibreCount28_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW029_G5 <- forFibreCount29_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW030_G5 <- forFibreCount30_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW031_G5 <- forFibreCount31_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW032_G5 <- forFibreCount32_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW033_G5 <- forFibreCount33_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW034_G5 <- forFibreCount34_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW035_G5 <- forFibreCount35_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW036_G5 <- forFibreCount36_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW037_G5 <- forFibreCount37_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW038_G5 <- forFibreCount38_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW039_G5 <- forFibreCount39_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW040_G5 <- forFibreCount40_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW041_G5 <- forFibreCount41_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW042_G5 <- forFibreCount42_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW043_G5 <- forFibreCount43_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW044_G5 <- forFibreCount44_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW045_G5 <- forFibreCount45_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW046_G5 <- forFibreCount46_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW047_G5 <- forFibreCount47_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW048_G5 <- forFibreCount48_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW049_G5 <- forFibreCount49_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW050_G5 <- forFibreCount50_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW051_G5 <- forFibreCount51_G5 %>%
  dplyr::select(Coder,Coder2,`Before transfer`)

# Dataset with 12 garments
BackgroundW000_G12A <- forFibreCount0_G12A %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW001_G12A <- forFibreCount1_G12A %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW002_G12A <- forFibreCount2_G12A %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW003_G12A <- forFibreCount3_G12A %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW004_G12A <- forFibreCount4_G12A %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW005_G12A <- forFibreCount5_G12A %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW000_G12B <- forFibreCount0_G12B %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW001_G12B <- forFibreCount1_G12B %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW002_G12B <- forFibreCount2_G12B %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW003_G12B <- forFibreCount3_G12B %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW004_G12B <- forFibreCount4_G12B %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW005_G12B <- forFibreCount5_G12B %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW000_G12C <- forFibreCount0_G12C %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW001_G12C <- forFibreCount1_G12C %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW002_G12C <- forFibreCount2_G12C %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW003_G12C <- forFibreCount3_G12C %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW004_G12C <- forFibreCount4_G12C %>%
  dplyr::select(Coder,Coder2,`Before transfer`)
BackgroundW005_G12C <- forFibreCount5_G12C %>%
  dplyr::select(Coder,Coder2,`Before transfer`)

#### Combine all the dataframe ####
BackgroundFibreCount <- rbind(BackgroundW000_G1, BackgroundW001_G1, BackgroundW002_G1,BackgroundW003_G1,BackgroundW004_G1,
                              BackgroundW005_G1,BackgroundW006_G1,BackgroundW007_G1,BackgroundW009_G1,BackgroundW011_G1,
                              BackgroundW000_G5, BackgroundW001_G5, BackgroundW002_G5, BackgroundW003_G5, BackgroundW004_G5, BackgroundW005_G5, BackgroundW006_G5,
                              BackgroundW007_G5, BackgroundW008_G5, BackgroundW009_G5, BackgroundW010_G5, BackgroundW011_G5, BackgroundW012_G5,BackgroundW013_G5,
                              BackgroundW014_G5, BackgroundW015_G5, BackgroundW016_G5,BackgroundW017_G5,BackgroundW018_G5,BackgroundW019_G5,BackgroundW020_G5,
                              BackgroundW021_G5,BackgroundW022_G5,BackgroundW023_G5,BackgroundW024_G5,BackgroundW025_G5, BackgroundW026_G5, BackgroundW027_G5,
                              BackgroundW028_G5, BackgroundW029_G5, BackgroundW030_G5,BackgroundW031_G5,BackgroundW032_G5,BackgroundW033_G5,BackgroundW034_G5,
                              BackgroundW035_G5,BackgroundW036_G5,BackgroundW037_G5,BackgroundW038_G5,BackgroundW039_G5,BackgroundW040_G5,
                              BackgroundW041_G5,BackgroundW042_G5,BackgroundW043_G5,BackgroundW044_G5,BackgroundW045_G5,BackgroundW046_G5,BackgroundW047_G5,
                              BackgroundW048_G5,BackgroundW049_G5,BackgroundW050_G5,BackgroundW051_G5,
                              BackgroundW000_G12A, BackgroundW001_G12A, BackgroundW002_G12A,BackgroundW003_G12A,BackgroundW004_G12A,
                              BackgroundW005_G12A,
                              BackgroundW000_G12B, BackgroundW001_G12B, BackgroundW002_G12B,BackgroundW003_G12B,BackgroundW004_G12B,
                              BackgroundW005_G12B,
                              BackgroundW000_G12C, BackgroundW001_G12C, BackgroundW002_G12C,BackgroundW003_G12C,BackgroundW004_G12C,
                              BackgroundW005_G12C)
names(BackgroundFibreCount) <- c("wash","garment", "value")
# write.table(TransferFibreCount_G1, file = "Fibre Count - Transfer.csv", quote = F, sep = ",", row.names = F)

# Count the number of fibres found on the background images
BackgroundFibreCountfibres <- aggregate(BackgroundFibreCount$wash,list(BackgroundFibreCount$value), FUN=length) # W011: NA