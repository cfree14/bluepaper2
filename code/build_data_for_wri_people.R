
# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(tidyverse)


# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/blue_paper/bluepaper2/data/"

# Read data
wc1_orig <- read.csv(file.path(datadir, "capture_fisheries_outcomes_2100totoday_under_full_adaptation.csv"), as.is=T)
wc2_orig <- read.csv(file.path(datadir, "capture_fisheries_outcomes_full_adaptation_compared_to_bau.csv"), as.is=T)
wc_msy_orig <- read.csv(file.path(datadir, "capture_fisheries_msy_change_by_rcp_country.csv"), as.is=T)
aq_b_orig <- read.csv(file.path(datadir, "aquauculture_bivalve_underages.csv"), as.is=T)
aq_f_orig <- read.csv(file.path(datadir, "aquauculture_finfish_underages.csv"), as.is=T)

# Merge data
data <- wc1_orig %>%
  select(sovereign_iso3, sovereign, country, rcp, msy_pdiff) %>%
  spread(key="rcp", value="msy_pdiff") %>%
  rename(msy_pdiff_rcp60="RCP 6.0", msy_pdiff_rcp85="RCP 8.5") %>%
  left_join(select(aq_b_orig, iso3, b_diff_mt), by=c("sovereign_iso3"="iso3")) %>%
  left_join(select(aq_f_orig, iso3, f_diff_mt), by=c("sovereign_iso3"="iso3")) %>%
  rename(aq_bivalve_underage_mt=b_diff_mt, aq_finfish_underage_mt=f_diff_mt)

# Export data
write.csv(data, file=file.path(datadir, "merged_bp2_wc_and_aq_data_for_wri.csv"), row.names=F)



