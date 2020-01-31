

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(countrycode)
library(RColorBrewer)

# Directories
dir_capture <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines"

# Load data
data <- readRDS(file.path(dir_capture, "gaines_data_for_eez_analysis.Rds"))


# Setup
################################################################################

# Subset to 2012 data
data1 <- data %>% 
  filter(year==2012 & rcp=="RCP26" & scenario%in%c("Range Shift Only")) %>% 
  arrange(eez, species)

# Export
saveRDS(data1, "data/fisheries/Free_etal_2012_catch.Rds")
