#############################################################
#####                     To read                       #####
#############################################################
# This R script is to generate the figures related to:
# the secondary transfer of fibres following washing

#----------------------------------------------------------------------------------#
####              Secondary transfer of fibres following washing               #####
#----------------------------------------------------------------------------------#

# Split the column based on "_"
ST_Dataset <- separate(ST_Dataset, Sample, into = c("Garment", "Side", "Row","Column", "Condition"), sep = "_")

# substract background
ST_Dataset$value <-  ST_Dataset$`After wash` -  ST_Dataset$`Before wash`

# Select important column
ST_Dataset <- ST_Dataset %>% dplyr::select(Garment, Side,Row,Column,`Before wash`,`After wash`,value)

# Create a list of data frames
STG_list <- lapply(1:15, function(i) ST_Dataset %>% filter(grepl(paste0('\\bG', i, '\\b'), Garment)))

# Assign the list elements to individual data frames
list2env(setNames(STG_list, paste0("STG", 1:15)), .GlobalEnv)


####_________________Boxplot_________________####
#Assign a Coder to each wash
for (i in 1:15) {
  assign(paste0("STG", i), transform(get(paste0("STG", i)), Coder = paste0("S", sprintf("%03d", i))))
}

# # Combined Dataset
# TotalData <- rbind(STG1,STG2,STG3,STG4,STG5,STG6,STG7,STG8,STG9,STG10,STG11,STG12,STG13,STG14,STG15)

# Define a list of STG datasets
STG_list <- list(STG1,STG2,STG3,STG4,STG5,STG6,STG7,STG8,STG9,STG10,STG11,STG12,STG13,STG14,STG15)

# Iterate through the list and aggregate the values
for (i in 1:length(STG_list)) {
  SumFibreCount <- aggregate(STG_list[[i]]$value, list(STG_list[[i]]$Coder), FUN = sum)
  names(SumFibreCount) <- c("Garment", "value")
  assign(paste0("SumFibreCountSTG", i), SumFibreCount)
}

# To plot data
SumFibreTotal <- rbind(SumFibreCountSTG1, SumFibreCountSTG2, SumFibreCountSTG3, SumFibreCountSTG4, SumFibreCountSTG5, SumFibreCountSTG6,
                       SumFibreCountSTG7, SumFibreCountSTG8, SumFibreCountSTG9, SumFibreCountSTG10, SumFibreCountSTG11, SumFibreCountSTG12,
                       SumFibreCountSTG13, SumFibreCountSTG14, SumFibreCountSTG15)

#### Create a graph of total fibres by wash/garment ####
STplot <- ggplot(SumFibreTotal, aes(x= Garment, y=value, group=1)) +
  geom_line(colour='#d1133f')+ 
  geom_point(colour='#d1133f')+
  ylim(0,1000)+
  labs(x="Secondary transfer wash", y="Number of Fibre secondary transferred") +
  theme_bw(base_family = "Arial", base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  geom_text(aes(label = round(value, 1)),
            vjust = -1.05, hjust = -0.25,
            show.legend = FALSE)
STplot
ggsave("Secondary transfer_Fibre Count total.png", STplot, width = 7, height = 5, units = "in", dpi=150, path = "Results/")

