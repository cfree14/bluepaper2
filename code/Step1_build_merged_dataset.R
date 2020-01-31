
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
dir_tourism <- "data/tourism"
dir_capture <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines"
dir_mariculture <- "/Users/cfree/Dropbox/Chris/UCSB/projects/aquacast/output/processed"
dir_plots <- "figures"

# Read data
tr_old_orig <- read.csv(file.path(dir_tourism, "tourism_data_old.csv"), as.is=T)
tr_new_orig <- read.csv(file.path(dir_tourism, "TourismProjection_2100.csv"), as.is=T) 
baq_orig <- read.csv(file.path(dir_mariculture, "bivalve_mariculture_potential_by_eez_rcp.csv"), as.is=T)
faq_orig <- read.csv(file.path(dir_mariculture, "finfish_mariculture_potential_by_eez_rcp_feed_scenario.csv"), as.is=T)
wc_orig <- readRDS(file.path(dir_capture, "gaines_territory_level_results_approach1.Rds"))


# Merge data
################################################################################

# Format tourism data
tr_old <- tr_old_orig %>% 
  # Rename/reduce columns
  rename(country=Country) %>% 
  select(country, value_usd_t, value_perc_change_rcp45, value_perc_change_rcp85) %>% 
  # Calculate value today and in future in USD
  mutate(value_usd_today=as.numeric(gsub(",", "", value_usd_t))*1000,
         value_usd_2100_rcp45=(1+value_perc_change_rcp45/100)*value_usd_today,
         value_usd_2100_rcp85=(1+value_perc_change_rcp45/100)*value_usd_today) %>% 
  # Reshape data
  select(country, value_usd_today, value_usd_2100_rcp45, value_usd_2100_rcp85) %>% 
  gather(key="rcp", value="value_usd_2100", 3:ncol(.)) %>% 
  mutate(rcp=recode(rcp, 
                    "value_usd_2100_rcp45"="RCP 4.5",
                    "value_usd_2100_rcp85"="RCP 8.5"),
         value_usd_diff=value_usd_2100-value_usd_today) %>% 
  # Rearrange columns
  select(country, rcp, everything()) %>% 
  # Add ISO code
  mutate(iso3=countrycode(country, "country.name", "iso3c"),
         iso3=ifelse(country=="Micronesia", "FSM", iso3),
         iso3=ifelse(country=="Virgin Islands of the United States", "VIR", iso3),
         iso3=ifelse(country=="Hawaii", "USA", iso3)) %>% 
  filter(!is.na(iso3)) %>% 
  ungroup()

# Format tourism data
tr_new <- tr_new_orig %>% 
  select(country, scenario, Total, Tourismvalue2100) %>% 
  rename(value_usd_today=Total, value_usd_2100=Tourismvalue2100, rcp=scenario) %>% 
  mutate(rcp=paste("RCP", rcp),
         rcp=recode(rcp, "RCP 6"="RCP 6.0")) %>% 
  mutate(value_usd_2100=pmax(0, value_usd_2100),
         value_usd_diff=value_usd_2100-value_usd_today) %>% 
  # Add ISO code
  mutate(iso3=countrycode(country, "country.name", "iso3c"),
         iso3=ifelse(country=="Micronesia", "FSM", iso3),
         iso3=ifelse(country=="Virgin Islands of the United States", "VIR", iso3)) %>% 
  # Filter
  filter(rcp %in% c("RCP 4.5", "RCP 8.5"))

# Format finfish mariculture data
faq <- faq_orig %>% 
  # Sum production and profits by SOVEREIGN NATION
  group_by(feed_scen, dev_pattern, rcp, year, sov1_name) %>% 
  summarize(value_usd_2100=sum(profits_usd, na.rm=T),
            production_mt_2100=sum(prod_mt, na.rm=T)) %>% 
  # Reduce to scenarios of interest
  mutate(scenario=ifelse(feed_scen=="Progressive reform" & dev_pattern=="Equal development", "Progressive reform", 
                         ifelse(feed_scen=="Base case" & dev_pattern=="Equal development", "Base case", "Other"))) %>% 
  filter(scenario%in%c("Progressive reform", "Base case") & year==2100) %>% 
  # Rename columns
  rename(country=sov1_name) %>% 
  # Add ISO code
  mutate(iso3=countrycode(country, "country.name", "iso3c"),
         iso3=ifelse(country=="Micronesia", "FSM", iso3),
         iso3=ifelse(country=="Comores", "COM", iso3)) %>% 
  filter(!is.na(iso3)) %>% 
  ungroup()

# Seperate reform and base
faq_base <- faq %>% 
  filter(scenario=="Base case")
faq_reform <- faq %>%
  filter(scenario=="Progressive reform")

# Format bivalve mariculture data
baq <- baq_orig %>% 
  # Sum production and profits by SOVEREIGN NATION
  group_by(rcp, year, sov1_name) %>% 
  summarize(value_usd_2100=sum(profits_usd, na.rm=T),
            production_mt_2100=sum(prod_mt, na.rm=T)) %>% 
  # Reduce to year of interest
  filter(year==2100) %>% 
  # Rename columns
  rename(country=sov1_name) %>% 
  # Add ISO code
  mutate(iso3=countrycode(country, "country.name", "iso3c")) %>% 
  filter(!is.na(iso3)) %>% 
  ungroup()

# Format capture data
wc <- wc_orig %>% 
  group_by(sovereign, rcp, scenario) %>% 
  summarize(production_mt_today=sum(c1, na.rm=T),
            production_mt_2100=sum(c2, na.rm=T),
            value_usd_today=sum(p1, na.rm=T),
            value_usd_2100=sum(p2, na.rm=T)) %>% 
  # Reduce to full adaptation
  filter(scenario=="Full Adaptation") %>% 
  # Format columns
  ungroup() %>% 
  mutate(rcp=recode(rcp, 
                    "RCP26"="RCP 2.6",
                    "RCP45"="RCP 4.5",
                    "RCP60"="RCP 6.0",
                    "RCP85"="RCP 8.5"),
         value_usd_diff=value_usd_2100-value_usd_today,
         production_mt_diff=production_mt_2100-production_mt_today) %>% 
  # Filter to RCPs of interest
  filter(rcp %in% c("RCP 4.5", "RCP 8.5")) %>% 
  # Rename columns
  rename(country=sovereign) %>% 
  # Add ISO code
  mutate(iso3=countrycode(country, "country.name", "iso3c"),
         iso3=ifelse(country=="Micronesia", "FSM", iso3)) %>% 
  filter(!is.na(iso3)) %>% 
  ungroup()


# Build data
# rcp, country, wc_value_usd_change, baq_value_usd_change, faq_value_usd_change, tour_value_usd_change, net_value_used_change
data <- wc %>% 
  # Format capture fisheries
  select(rcp, iso3, value_usd_diff, production_mt_diff) %>% 
  rename(wc_value_usd_change=value_usd_diff, 
         wc_prod_mt_change=production_mt_diff) %>% 
  # Add finfish mariculture (base case)
  full_join(select(faq_base, rcp, iso3, value_usd_2100, production_mt_2100), by=c("rcp", "iso3")) %>% 
  rename(faq_base_value_usd_change=value_usd_2100, 
         faq_base_prod_mt_change=production_mt_2100) %>% 
  # Add finfish mariculture (reform)
  full_join(select(faq_reform, rcp, iso3, value_usd_2100, production_mt_2100), by=c("rcp", "iso3")) %>% 
  rename(faq_reform_value_usd_change=value_usd_2100, 
         faq_reform_prod_mt_change=production_mt_2100) %>% 
  # Add bivalve aquaculture
  full_join(select(baq, rcp, iso3, value_usd_2100, production_mt_2100), by=c("rcp", "iso3")) %>% 
  rename(baq_value_usd_change=value_usd_2100, 
         baq_prod_mt_change=production_mt_2100) %>% 
  # Add tourism
  full_join(select(tr_new, rcp, iso3, value_usd_diff), by=c("rcp", "iso3")) %>% 
  rename(tour_value_usd_change=value_usd_diff) %>% 
  # Replace all NAs with 0s
  mutate(wc_value_usd_change=ifelse(is.na(wc_value_usd_change), 0, wc_value_usd_change),
         wc_prod_mt_change=ifelse(is.na(wc_prod_mt_change), 0, wc_prod_mt_change)) %>% 
  mutate(faq_base_value_usd_change=ifelse(is.na(faq_base_value_usd_change), 0, faq_base_value_usd_change),
         faq_base_prod_mt_change=ifelse(is.na(faq_base_prod_mt_change), 0, faq_base_prod_mt_change)) %>% 
  mutate(faq_reform_value_usd_change=ifelse(is.na(faq_reform_value_usd_change), 0, faq_reform_value_usd_change),
         faq_reform_prod_mt_change=ifelse(is.na(faq_reform_prod_mt_change), 0, faq_reform_prod_mt_change)) %>% 
  mutate(baq_value_usd_change=ifelse(is.na(baq_value_usd_change), 0, baq_value_usd_change),
         baq_prod_mt_change=ifelse(is.na(baq_prod_mt_change), 0, baq_prod_mt_change)) %>% 
  mutate(tour_value_usd_change=ifelse(is.na(tour_value_usd_change), 0, tour_value_usd_change)) %>% 
  # Calculate net change
  mutate(base_net_value_usd_change=rowSums(.[c("wc_value_usd_change", "baq_value_usd_change", "faq_base_value_usd_change", "tour_value_usd_change")], na.rm=T),
         base_net_prod_mt_change=rowSums(.[c("wc_prod_mt_change", "baq_prod_mt_change", "faq_base_prod_mt_change")], na.rm=T),
         reform_net_value_usd_change=rowSums(.[c("wc_value_usd_change", "baq_value_usd_change", "faq_reform_value_usd_change", "tour_value_usd_change")], na.rm=T),
         reform_net_prod_mt_change=rowSums(.[c("wc_prod_mt_change", "baq_prod_mt_change", "faq_reform_prod_mt_change")], na.rm=T)) %>% 
  # Classify net change
  mutate(base_value_type=ifelse(base_net_value_usd_change<0, "Net loser", 
                            ifelse(wc_value_usd_change>=0 & baq_value_usd_change>=0 & faq_base_value_usd_change>=0 & tour_value_usd_change>=0, "Net winner", "Net offsetter")),
         base_prod_type=ifelse(base_net_prod_mt_change<0, "Net loser", 
                          ifelse(wc_prod_mt_change>=0 & baq_prod_mt_change>=0 & faq_base_prod_mt_change>=0, "Net winner", "Net offsetter")),
         reform_value_type=ifelse(reform_net_value_usd_change<0, "Net loser", 
                                ifelse(wc_value_usd_change>=0 & baq_value_usd_change>=0 & faq_reform_value_usd_change>=0 & tour_value_usd_change>=0, "Net winner", "Net offsetter")),
         reform_prod_type=ifelse(reform_net_prod_mt_change<0, "Net loser", 
                               ifelse(wc_prod_mt_change>=0 & baq_prod_mt_change>=0 & faq_reform_prod_mt_change>=0, "Net winner", "Net offsetter"))) %>% 
  # Arrange columns
  select(rcp, iso3, 
         wc_value_usd_change, baq_value_usd_change, faq_base_value_usd_change, faq_reform_value_usd_change, tour_value_usd_change, 
         base_net_value_usd_change, base_value_type, reform_net_value_usd_change, reform_value_type,
         wc_prod_mt_change, baq_prod_mt_change, faq_base_prod_mt_change, faq_reform_prod_mt_change,
         base_net_prod_mt_change, base_prod_type, reform_net_prod_mt_change, reform_prod_type, everything())


# Types
table(data$rcp, data$reform_value_type)
table(data$rcp, data$reform_prod_type)
table(data$rcp, data$base_value_type)
table(data$rcp, data$base_prod_type)
n_distinct(data$iso3)


# Plot profit type
#########################

# Reshape data for plotting
data_plot <- data %>% 
  select(rcp, iso3, base_prod_type, reform_prod_type, base_value_type, reform_value_type) %>% 
  gather(key="scenario", value="value_type", 3:ncol(.)) %>%
  mutate(metric=ifelse(grepl("prod", scenario), "Production", "Profits"), 
         scenario=ifelse(grepl("base", scenario), "Base case", "Progressive reform")) %>% 
  select(rcp, scenario, metric, iso3, value_type) %>% 
  arrange(rcp, scenario, metric, iso3) %>% 
  mutate(scenario_label=recode(scenario, 
                               "Base case"="Business-as-usual\nmariculture feed practices",
                               "Progressive reform"="Progressive reforms in\nmariculture feed practices"))
  
# Get world
world <- rnaturalearth::ne_countries(scale="small", type="countries", returnclass="sf") %>% 
  mutate(iso3=countrycode(name_long, "country.name", "iso3c"))

# Add results to world
data_sf <- world %>% 
  left_join(data_plot, by=c("iso3"="iso3")) %>% 
  filter(!is.na(rcp))

# Plot
g1 <- ggplot() +
  geom_sf(world, mapping=aes(), lwd=0.2, fill="grey70") +
  geom_sf(filter(data_sf, metric=="Profits"), mapping=aes(fill=value_type), lwd=0.2) +
  facet_grid(rcp ~ scenario_label) +
  # labs(title="Net impact of climate change and adaptation\non profits from the ocean ecosystem economy") +
  scale_fill_discrete(name="", na.value = "grey30") +
  theme_bw() +
  theme(legend.position = "bottom")
g1

# Export figure
ggsave(g1, filename=file.path(dir_plots, "figure_map_country_type_profits.png"), 
       width=6.5, height=4, units="in", dpi=600)

# Plot
g2 <- ggplot() +
  geom_sf(world, mapping=aes(), lwd=0.2, fill="grey70") +
  geom_sf(filter(data_sf, metric=="Production"), mapping=aes(fill=value_type), lwd=0.2) +
  facet_grid(rcp ~ scenario_label) +
  # labs(title="Net impact of climate change and adaptation\non food from the ocean ecosystem economy") +
  scale_fill_discrete(name="", na.value = "grey30") +
  theme_bw() +
  theme(legend.position = "bottom")
g2

# Export figure
ggsave(g2, filename=file.path(dir_plots, "figure_map_country_type_food.png"), 
       width=6.5, height=4, units="in", dpi=600)



  
  
  
  










