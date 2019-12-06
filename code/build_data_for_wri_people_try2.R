
# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/blue_paper/bluepaper2/data/"

# Read template
temp <- readxl::read_excel(file.path(datadir, "Flourish_Map_Template.xlsx"))

# Read data
wc1_orig <- read.csv(file.path(datadir, "capture_fisheries_outcomes_2100totoday_under_full_adaptation.csv"), as.is=T)
wc2_orig <- read.csv(file.path(datadir, "capture_fisheries_outcomes_full_adaptation_compared_to_bau.csv"), as.is=T)
wc_msy_orig <- read.csv(file.path(datadir, "capture_fisheries_msy_change_by_rcp_country.csv"), as.is=T)
aq_b_orig <- read.csv(file.path(datadir, "aquauculture_bivalve_underages.csv"), as.is=T)
aq_f_orig <- read.csv(file.path(datadir, "aquauculture_finfish_underages.csv"), as.is=T)


# Bubble charts
#############################

# Format WC1 data
wc1 <- wc1_orig %>% 
  select(rcp, c_pdiff, p_pdiff, msy_pdiff, msy_tot1) %>% 
  rename(x=c_pdiff, y= p_pdiff, color=msy_pdiff, size=msy_tot1) %>% 
  mutate(size=size/1e6)
wc1_60 <- filter(wc1, rcp=="RCP 6.0")
wc1_85 <- filter(wc1, rcp=="RCP 8.5")

# Format WC2 data
wc2 <- wc2_orig %>% 
  select(rcp, c_tot_rel, p_tot_rel, msy_pdiff, msy_tot1) %>% 
  rename(x=c_tot_rel, y=p_tot_rel, color=msy_pdiff, size=msy_tot1) %>% 
  mutate(size=size/1e6)
wc2_60 <- filter(wc2, rcp=="RCP 6.0")
wc2_85 <- filter(wc2, rcp=="RCP 8.5")

# Export
sheets <- list(wc1_60, wc1_85, wc1_60, wc1_85)
openxlsx::write.xlsx(sheets, file = file.path(datadir, "bubble_chart_data_for_wri.xlsx"))


# Maps
#############################

#
mdata <- temp %>% 
  left_join(select(aq_b_orig, iso3, b_diff_mt), by=c("3-letter ISO code"="iso3")) %>% 
  left_join(select(aq_f_orig, iso3, f_diff_mt), by=c("3-letter ISO code"="iso3")) %>% 
  mutate("Bivalve production"=b_diff_mt, 
         "Finfish production"=f_diff_mt)

write.csv(mdata, file=file.path(datadir, "mariculture_map_data_for_wri.csv"))


