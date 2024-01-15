###########################################################################
#
# Exploring the influence of washing activities on the transfer and persistence of fibres in forensic science
#
# Leverhulme Research Centre for Forensic Science

# Virginie Galais, Stephanie Wilson, Niamh Nic Daéid, Hervé Ménard

# Website: https://github.com/LRCFS/
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

###########################################################################
# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                library requirement                #####
#############################################################
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(extrafont)
library(RColorBrewer)
library(ggpubr)
library(gridExtra)
library(grid)
library(devtools)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(corrplot)
library(matrixStats)
library(corrplot)
library(MASS)
library(psych)


#############################################################
#####                      Function                     #####
#############################################################

source("Functions/SearchAndReplace.R")
source("Functions/posthocTGH.R")
source("Functions/ReturnEveryNthLabel.R")
source("Functions/get_formula.R")

#############################################################
#####                Folder & Files                     #####
#############################################################

# where the generated figures are saved, create folder if not existing
# dir.create(file.path(Results.dir),recursive = TRUE) # will create folder if not already there.
Results.dir <- "Results/"

#############################################################
#####                       Codes                       #####
#############################################################
# This codes can be run subsequently as each create necessary outputs for the next codes.  
source("Code/1 - Data upload.R")
# source("Code/Analysis - Transfer.R")
