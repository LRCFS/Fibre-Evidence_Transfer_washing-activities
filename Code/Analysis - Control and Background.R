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
  G12A = c("W000", "W001", "W002", "W003", "W004", "W005", "W006","W007", "W008", "W009","W010", "W011", "W012", "W013"),
  G12B = c("W000", "W001", "W002", "W003", "W004", "W005", "W006","W007", "W008", "W009","W010", "W011", "W012", "W013"),
  G12C = c("W000", "W001", "W002", "W003", "W004", "W005", "W006","W007", "W008", "W009","W010", "W011", "W012", "W013")
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
# Removing the controls
# Dataset with 1 garment
# Create a vector of dataset names
dataset_names <- c("W000_G1_Dataset", "W001_G1_Dataset", "W002_G1_Dataset",
                   "W003_G1_Dataset", "W004_G1_Dataset", "W005_G1_Dataset",
                   "W006_G1_Dataset", "W007_G1_Dataset", "W009_G1_Dataset",
                   "W011_G1_Dataset")

# Filter the datasets and create variables
filtered_datasets <- lapply(mget(dataset_names, envir = .GlobalEnv), function(dataset) {
  if (!is.null(dataset)) {
    dataset[!(grepl("W\\d{3}_G1_(positive|negative)_B", dataset$Sample)), ]
  } else {
    NULL
  }
})

# Assign variables to the filtered datasets with desired names
for (i in 1:length(filtered_datasets)) {
  if (!is.null(filtered_datasets[[i]])) {
    if (i == 9) {
      assign("forFibreCount9_G1", filtered_datasets[[i]])
    } else if (i == 10) {
      assign("forFibreCount11_G1", filtered_datasets[[i]])
    } else if (i == 8) {
      assign("forFibreCount7_G1", filtered_datasets[[i]])
    } else {
      assign(paste0("forFibreCount", i - 1, "_G1"), filtered_datasets[[i]])
    }
  } else {
    print(paste0("forFibreCount", i - 1, "_G1 not created."))
  }
}

# Dataset with 5 garments
dataset_names <- c(
  "W000_G5_Dataset", "W001_G5_Dataset", "W002_G5_Dataset", "W003_G5_Dataset",
  "W004_G5_Dataset", "W005_G5_Dataset", "W006_G5_Dataset", "W007_G5_Dataset",
  "W008_G5_Dataset", "W009_G5_Dataset", "W010_G5_Dataset", "W011_G5_Dataset",
  "W012_G5_Dataset", "W013_G5_Dataset", "W014_G5_Dataset", "W015_G5_Dataset",
  "W016_G5_Dataset", "W017_G5_Dataset", "W018_G5_Dataset", "W019_G5_Dataset",
  "W020_G5_Dataset", "W021_G5_Dataset", "W022_G5_Dataset", "W023_G5_Dataset",
  "W024_G5_Dataset", "W025_G5_Dataset", "W026_G5_Dataset", "W027_G5_Dataset",
  "W028_G5_Dataset", "W029_G5_Dataset", "W030_G5_Dataset", "W031_G5_Dataset",
  "W032_G5_Dataset", "W033_G5_Dataset", "W034_G5_Dataset", "W035_G5_Dataset",
  "W036_G5_Dataset", "W037_G5_Dataset", "W038_G5_Dataset", "W039_G5_Dataset",
  "W040_G5_Dataset", "W041_G5_Dataset", "W042_G5_Dataset", "W043_G5_Dataset",
  "W044_G5_Dataset", "W045_G5_Dataset", "W046_G5_Dataset", "W047_G5_Dataset",
  "W048_G5_Dataset", "W049_G5_Dataset", "W050_G5_Dataset", "W051_G5_Dataset"
)

# Loop through the dataset names
for (i in 1:length(dataset_names)) {
  # Create a variable with the current dataset
  current_dataset <- get(dataset_names[i])
  
  # Filter the dataset based on Sample column
  assign(paste0("forFibreCount", i - 1, "_G5"), current_dataset[!(current_dataset$Sample %in% c(paste0("W", sprintf("%03d", i - 1), "_positive"), paste0("W", sprintf("%03d", i - 1), "_negative"))),])
}


# Dataset with 12 garments
# Create a vector of dataset names
dataset_names <- c("W000_G12A_Dataset", "W001_G12A_Dataset", "W002_G12A_Dataset",
                   "W003_G12A_Dataset", "W004_G12A_Dataset", "W005_G12A_Dataset",
                   "W006_G12A_Dataset","W007_G12A_Dataset","W008_G12A_Dataset",
                   "W009_G12A_Dataset","W010_G12A_Dataset","W011_G12A_Dataset",
                   "W012_G12A_Dataset","W013_G12A_Dataset")

# Loop through the dataset names
for (i in 1:length(dataset_names)) {
  # Create a variable with the current dataset
  current_dataset <- get(dataset_names[i])
  
  # Filter the dataset based on Sample column
  assign(paste0("forFibreCount", i - 1, "_G12A"), current_dataset[!(grepl(paste0("W00", i - 1), current_dataset$Sample) & grepl("_positive_B|_negative_B", current_dataset$Sample)),])
}
dataset_names <- c("W000_G12B_Dataset", "W001_G12B_Dataset", "W002_G12B_Dataset",
                   "W003_G12B_Dataset", "W004_G12B_Dataset", "W005_G12B_Dataset",
                   "W006_G12B_Dataset","W007_G12B_Dataset","W008_G12B_Dataset",
                   "W009_G12B_Dataset","W010_G12B_Dataset","W011_G12B_Dataset",
                   "W012_G12B_Dataset","W013_G12B_Dataset")

# Loop through the dataset names
for (i in 1:length(dataset_names)) {
  # Create a variable with the current dataset
  current_dataset <- get(dataset_names[i])
  
  # Filter the dataset based on Sample column
  assign(paste0("forFibreCount", i - 1, "_G12B"), current_dataset[!(grepl(paste0("W00", i - 1), current_dataset$Sample) & grepl("_positive_B|_negative_B", current_dataset$Sample)),])
}
dataset_names <- c("W000_G12C_Dataset", "W001_G12C_Dataset", "W002_G12C_Dataset",
                   "W003_G12C_Dataset", "W004_G12C_Dataset", "W005_G12C_Dataset",
                   "W006_G12C_Dataset","W007_G12C_Dataset","W008_G12C_Dataset",
                   "W009_G12C_Dataset","W010_G12C_Dataset","W011_G12C_Dataset",
                   "W012_G12C_Dataset","W013_G12C_Dataset")

# Loop through the dataset names
for (i in 1:length(dataset_names)) {
  # Create a variable with the current dataset
  current_dataset <- get(dataset_names[i])
  
  # Filter the dataset based on Sample column
  assign(paste0("forFibreCount", i - 1, "_G12C"), current_dataset[!(grepl(paste0("W00", i - 1), current_dataset$Sample) & grepl("_positive_B|_negative_B", current_dataset$Sample)),])
}

#### select the column Before transfer only in each dataframe ####
# Create a vector of dataset names
data_list <- list(forFibreCount0_G1,forFibreCount1_G1,forFibreCount2_G1,forFibreCount3_G1,forFibreCount4_G1,
                     forFibreCount5_G1,forFibreCount6_G1,forFibreCount7_G1,forFibreCount9_G1,forFibreCount11_G1,
                     
                     forFibreCount0_G5,forFibreCount1_G5,forFibreCount2_G5,forFibreCount3_G5,forFibreCount4_G5,
                     forFibreCount5_G5,forFibreCount6_G5,forFibreCount7_G5,forFibreCount8_G5,forFibreCount9_G5,
                     forFibreCount10_G5,forFibreCount11_G5,forFibreCount12_G5,forFibreCount13_G5,forFibreCount14_G5,
                     forFibreCount15_G5,forFibreCount16_G5,forFibreCount17_G5,forFibreCount18_G5,forFibreCount19_G5,
                     forFibreCount20_G5,forFibreCount21_G5,forFibreCount22_G5,forFibreCount23_G5,forFibreCount24_G5,
                     forFibreCount25_G5,forFibreCount26_G5,forFibreCount27_G5,forFibreCount28_G5,forFibreCount29_G5,
                     forFibreCount30_G5,forFibreCount31_G5,forFibreCount32_G5,forFibreCount33_G5,forFibreCount34_G5,
                     forFibreCount35_G5,forFibreCount36_G5,forFibreCount37_G5,forFibreCount38_G5,forFibreCount39_G5,
                     forFibreCount40_G5,forFibreCount41_G5,forFibreCount42_G5,forFibreCount43_G5,forFibreCount44_G5,
                     forFibreCount45_G5,forFibreCount46_G5,forFibreCount47_G5,forFibreCount48_G5,forFibreCount49_G5,
                     forFibreCount50_G5,forFibreCount51_G5,
                  
                  forFibreCount0_G12A,forFibreCount1_G12A,forFibreCount2_G12A,forFibreCount3_G12A,forFibreCount4_G12A,
                  forFibreCount5_G12A,forFibreCount6_G12A,forFibreCount7_G12A,forFibreCount8_G12A,forFibreCount9_G12A,
                  forFibreCount10_G12A,forFibreCount11_G12A,forFibreCount12_G12A,forFibreCount13_G12A,
                  
                  forFibreCount0_G12B,forFibreCount1_G12B,forFibreCount2_G12B,forFibreCount3_G12B,forFibreCount4_G12B,
                  forFibreCount5_G12B,forFibreCount6_G12B,forFibreCount7_G12B,forFibreCount8_G12B,forFibreCount9_G12B,
                  forFibreCount10_G12B,forFibreCount11_G12B,forFibreCount12_G12B,forFibreCount13_G12B,
                  
                  forFibreCount0_G12C,forFibreCount1_G12C,forFibreCount2_G12C,forFibreCount3_G12C,forFibreCount4_G12C,
                  forFibreCount5_G12C,forFibreCount6_G12C,forFibreCount7_G12C,forFibreCount8_G12C,forFibreCount9_G12C,
                  forFibreCount10_G12C,forFibreCount11_G12C,forFibreCount12_G12C,forFibreCount13_G12C)

# apply the select function to each data frame to extract the columns:
#"Coder, Coder2, and Before transfer"
# The result is a new list called Background that contains the modified data frames
Background <- lapply(data_list, function(data) {
  data %>%
    dplyr::select(Coder, Coder2, `Before transfer`)
})
# convert the list Background back to a single data frame
combined_data_Background <- bind_rows(Background)

# Count the number of fibres found on the background images
combined_data_Background <- aggregate(combined_data_Background$Coder,list(combined_data_Background$`Before transfer`), FUN=length) 
