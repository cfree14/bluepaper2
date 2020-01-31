
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
tr_orig <- read.csv(file.path(dir_tourism, "TourismProjection_2100_run4.csv"), as.is=T) 
baq_orig <- read.csv(file.path(dir_mariculture, "bivalve_mariculture_potential_by_eez_rcp.csv"), as.is=T)
faq_orig <- read.csv(file.path(dir_mariculture, "finfish_mariculture_potential_by_eez_rcp_feed_scenario.csv"), as.is=T)
wc_orig <- readRDS(file.path(dir_capture, "gaines_territory_level_results_approach1.Rds"))

# Get world
world <- rnaturalearth::ne_countries(scale="small", type="countries", returnclass="sf") %>% 
  mutate(iso3=countrycode(name_long, "country.name", "iso3c"))

# Step 1. Format tourism data
################################################################################

# Format tourism data
tr <- tr_orig %>% 
  select(country, scenario, Total, Tourismvalue2100) %>% 
  rename(value_usd_today=Total, value_usd_2100=Tourismvalue2100, rcp=scenario) %>% 
  mutate(rcp=paste("RCP", rcp),
         rcp=recode(rcp, "RCP 6"="RCP 6.0")) %>% 
  mutate(value_usd_diff=value_usd_2100-value_usd_today) %>% 
  # Add ISO code
  mutate(iso3=countrycode(country, "country.name", "iso3c"),
         iso3=ifelse(country=="Micronesia", "FSM", iso3),
         iso3=ifelse(country=="Virgin Islands of the United States", "VIR", iso3))


# Step 2. Format fisheries data
################################################################################

# FAO data
###################################

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")

# Inspect
table(fao_orig$area_type)
table(fao_orig$units)
table(fao$isscaap)

# Build 2012 catch data
bad_isscaaps <- c("Brown seaweeds", "Corals", "Green seaweeds", "Horseshoe crabs and other arachnoids", "Miscellaneous aquatic plants", "Red seaweeds", " Sponges", "Turtles")
fao <- fao_orig %>%
  filter(area_type=="marine" & units=="t" & !isscaap%in%c(bad_isscaaps) & year==2012) %>% 
  group_by(country, iso3) %>% 
  summarize(prod_mt=sum(quantity, na.rm=T)) %>% 
  ungroup()

# Gaines projections
###################################

# Countries in Gaines data
cntry_key <- wc_orig %>% 
  ungroup() %>% 
  select(sovereign, sovereign_iso3, country) %>% 
  unique() %>%  
  filter(!sovereign%in%c("High Seas", "Disputed") & sovereign_iso3!="Joint") %>% 
  mutate(country_use=recode(country,
                        "Alaska"="United States",
                        "Hawaii"="United States",
                        "Micronesia"="Federated States of Micronesia",
                        "Puerto Rico of the United States"="Puerto Rico"),
         country_use_iso3=countrycode(country_use, "country.name", "iso3c")) %>% 
  # Use the sovereign nation as the jurisdictional unit for the remaining territories without ISOs
  mutate(country_use=ifelse(is.na(country_use_iso3), sovereign, country_use)) %>% 
  # Run ISO3 lookup one last time and now it should be perfect
  mutate(country_use_iso3=countrycode(country_use, "country.name", "iso3c"))

# Free et al. catch data
catch12_orig <- readRDS("data/fisheries/Free_etal_2012_catch.Rds")

# Calculate total catch by correct jurisdictional unit
catch12 <- catch12_orig %>% 
  # Add "country use"
  filter(!sovereign%in%c("High Seas", "Disputed") & sovereign_iso3!="Joint") %>% 
  left_join(select(cntry_key, country, country_use, country_use_iso3), by="country") %>% 
  # Calculate overall 2012 catch
  group_by(country_use, country_use_iso3) %>% 
  summarise(catch_mt_gaines=sum(harvest_eez, na.rm=T),
            profits_usd_gaines=sum(profit_eez, na.rm=T)) %>% 
  # Remove missing country
  filter(!is.na(country_use)) %>% 
  # Add FAO catch
  left_join(select(fao, iso3, prod_mt), by=c("country_use_iso3"="iso3")) %>% 
  rename(catch_mt_fao=prod_mt) %>% 
  # Add scalars
  mutate(gaines_prop=catch_mt_gaines / catch_mt_fao,
         gaines_scalar=1/gaines_prop,
         gaines_prop_cap=pmin(2, gaines_prop)) %>% 
  ungroup()

# Plot quickly
hist(catch12$gaines_prop, xlim=c(0,10), breaks=seq(0,6000,0.05), col="grey80")
abline(v=1)

# Plot as map
scalar_sf <- world %>% 
  left_join(select(catch12, country_use_iso3, gaines_prop, gaines_prop_cap), by=c("iso3"="country_use_iso3"))
g <- ggplot(scalar_sf) +
  geom_sf(aes(fill=gaines_prop_cap), lwd=0.05) +
  scale_fill_gradientn(name="Proportion of reported catch\nincluded in Free et al. (2019)", 
                       colors=RColorBrewer::brewer.pal(9, "RdBu"),
                       limits=c(0,2),
                       breaks=seq(0,2,0.5),
                       labels=c("0.0", "0.5", "1.0", "1.5", ">2.0")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(dir_plots, "figure_proportion_of_fao_catch_in_free_etal.png"), 
       width=6.5, height=4, units="in", dpi=600)


# Format capture data
wc <- wc_orig %>% 
  # Add "country use"
  filter(!sovereign%in%c("High Seas", "Disputed") & sovereign_iso3!="Joint") %>% 
  left_join(select(cntry_key, country, country_use, country_use_iso3), by="country") %>% 
  # Calculate total profits/production by "country"
  group_by(sovereign, sovereign_iso3, country_use, country_use_iso3, rcp, scenario) %>% 
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
  # Add catch scalar
  left_join(select(catch12, country_use_iso3, gaines_prop, gaines_scalar), by="country_use_iso3") %>% 
  # Make assumption that jurisdictions with 0, NA, or Inf catch scalars should be unscaled (1)
  mutate(scalar_use=ifelse(is.na(gaines_scalar) | gaines_scalar==0 | is.infinite(gaines_scalar), 1, gaines_scalar)) %>% 
  # Scale catch/profit loss
  mutate(production_mt_diff_scaled=production_mt_diff*scalar_use,
         value_usd_diff_scaled=value_usd_diff*scalar_use)
  
# Inspect
freeR::complete(wc)
  


# Step 3. Format mariculture data
################################################################################

# Read current production
maq_curr <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/aquacast/data/feed_params/processed/FAO_2013_2017_maq_prod_averages_by_country.csv")

# Mariculture demand caps
baq_cap <- 53.5 * 10^6
faq_cap <- 21.6 * 10^6

# Finfish formatting
###########################################

# Build AQ country key
faq_cntry_key <- faq_orig %>% 
  # Unique EEZs
  select(sov1_name, ter1_name, eez_name, eez_type) %>% 
  unique() %>% 
  # Omit disputed/point EEZs
  filter(eez_type=="200NM") %>% 
  # Do territories have ISO3s
  mutate(ter1_name_use=recode(ter1_name,
                              "Alaska"="United States",
                              "Hawaii"="United States",
                              "Micronesia"="Federated States of Micronesia",
                              "Comores"="Comoros",
                              "Puerto Rico of the United States"="Puerto Rico"),
         ter1_iso_use=countrycode(ter1_name_use, "country.name", "iso3c")) %>% 
  # Use the sovereign nation as the jurisdictional unit for the remaining territories without ISOs
  mutate(ter1_name_use=ifelse(is.na(ter1_iso_use), sov1_name, ter1_name_use)) %>% 
  # Run ISO3 lookup one last time and now it should be perfect
  mutate(ter1_iso_use=countrycode(ter1_name_use, "country.name", "iso3c"))

# Format finfish mariculture data
faq <- faq_orig %>% 
  # Add "country use"
  filter(eez_type=="200NM") %>% 
  left_join(select(faq_cntry_key, eez_name, ter1_name_use, ter1_iso_use), by="eez_name") %>% 
  # Sum production and profits by TER1 USE
  group_by(feed_scen, dev_pattern, rcp, year, ter1_name_use, ter1_iso_use) %>% 
  summarize(value_usd_2100=sum(profits_usd, na.rm=T),
            production_mt_2100=sum(prod_mt, na.rm=T)) %>% 
  # Reduce to scenarios of interest
  mutate(scenario=paste(feed_scen, dev_pattern, sep="/"),
         scenario=recode(scenario, 
                         "Business-as-usual/Current development"="Business-as-usual",
                         "Progressive reform/Equal development"="Progressive reform",
                         "Progressive reform/Need-based development"="Need-based progressive reform")) %>% 
  filter(scenario %in% c("Business-as-usual", "Progressive reform", "Need-based progressive reform")) %>% 
  # Reduce to terminal year
  filter(year==2100) %>% 
  # Add current production
  left_join(maq_curr %>% filter(major_group=="Pisces") %>% select(iso3, prod_mt, profit_usd), by=c("ter1_iso_use"="iso3")) %>% 
  rename(production_mt_2012=prod_mt, value_usd_2012=profit_usd) %>% 
  mutate(production_mt_2012=ifelse(is.na(production_mt_2012), 0, production_mt_2012),
         production_mt_diff=production_mt_2100-production_mt_2012, 
         value_usd_2012=ifelse(is.na(value_usd_2012), 0, value_usd_2012),
         value_usd_diff=value_usd_2100-value_usd_2012) %>% 
  ungroup() %>% 
  # Scale production to match demand cap by scenario
  group_by(scenario) %>% 
  mutate(prod_mt_total=sum(production_mt_2100),
         prop_used=faq_cap/prod_mt_total,
         production_mt_diff_scaled=production_mt_diff*prop_used,
         value_usd_diff_scaled=value_usd_diff*prop_used) %>% 
  ungroup()

# Format FAQ for merge
# rcp, iso3, prod/profit diff by each scenario
faq_for_merge <- faq %>% 
  select(rcp, ter1_iso_use, scenario, production_mt_diff_scaled, value_usd_diff_scaled) %>% 
  # Reshape
  gather(key="metric", value="value", 4:5) %>% 
  mutate(variable=paste(scenario, metric, sep="-")) %>% 
  select(rcp, ter1_iso_use, variable, value) %>% 
  spread(key="variable", value="value") %>% 
  # Rename columns
  rename(iso3="ter1_iso_use",
         faq_bau_prod_mt_change="Business-as-usual-production_mt_diff_scaled",            
         faq_bau_value_usd_change="Business-as-usual-value_usd_diff_scaled",
         faq_need_prod_mt_change="Need-based progressive reform-production_mt_diff_scaled",
         faq_need_value_usd_change="Need-based progressive reform-value_usd_diff_scaled",    
         faq_reform_prod_mt_change="Progressive reform-production_mt_diff_scaled",
         faq_reform_value_usd_change="Progressive reform-value_usd_diff_scaled") %>% 
  # Arrange column order
  select(rcp, iso3, 
         faq_bau_prod_mt_change, faq_bau_value_usd_change,
         faq_reform_prod_mt_change, faq_reform_value_usd_change,
         faq_need_prod_mt_change, faq_need_value_usd_change)




# Bivalve formatting
###########################################

# Build AQ country key
baq_cntry_key <- baq_orig %>% 
  # Unique EEZs
  select(sov1_name, ter1_name, eez_name, eez_type) %>% 
  unique() %>% 
  # Omit disputed/point EEZs
  filter(eez_type=="200NM") %>% 
  # Do territories have ISO3s
  mutate(ter1_name_use=recode(ter1_name,
                              "Alaska"="United States",
                              "Hawaii"="United States",
                              "Micronesia"="Federated States of Micronesia",
                              "Comores"="Comoros",
                              "Puerto Rico of the United States"="Puerto Rico"),
         ter1_iso_use=countrycode(ter1_name_use, "country.name", "iso3c")) %>% 
  # Use the sovereign nation as the jurisdictional unit for the remaining territories without ISOs
  mutate(ter1_name_use=ifelse(is.na(ter1_iso_use), sov1_name, ter1_name_use)) %>% 
  # Run ISO3 lookup one last time and now it should be perfect
  mutate(ter1_iso_use=countrycode(ter1_name_use, "country.name", "iso3c"))

# Format bivalve mariculture data
baq <- baq_orig %>% 
  # Add "country use"
  filter(eez_type=="200NM") %>% 
  left_join(select(baq_cntry_key, eez_name, ter1_name_use, ter1_iso_use), by="eez_name") %>% 
  # Sum production and profits by TER1 USE
  group_by(rcp, year, ter1_name_use, ter1_iso_use) %>% 
  summarize(value_usd_2100=sum(profits_usd, na.rm=T),
            production_mt_2100=sum(prod_mt, na.rm=T)) %>% 
  # Reduce to terminal year
  filter(year==2100) %>% 
  # Add current production
  left_join(maq_curr %>% filter(major_group=="Mollusca") %>% select(iso3, prod_mt, profit_usd), by=c("ter1_iso_use"="iso3")) %>% 
  rename(production_mt_2012=prod_mt, value_usd_2012=profit_usd) %>% 
  mutate(production_mt_2012=ifelse(is.na(production_mt_2012), 0, production_mt_2012),
         production_mt_diff=production_mt_2100-production_mt_2012, 
         value_usd_2012=ifelse(is.na(value_usd_2012), 0, value_usd_2012),
         value_usd_diff=value_usd_2100-value_usd_2012) %>% 
  ungroup() %>% 
  # Scale production to match demand cap by scenario
  mutate(prod_mt_total=sum(production_mt_2100),
         prop_used=baq_cap/prod_mt_total,
         production_mt_diff_scaled=production_mt_diff*prop_used,
         value_usd_diff_scaled=value_usd_diff*prop_used)


# Step 4. Merge data
################################################################################

# Build data
# rcp, country, wc_value_usd_change, baq_value_usd_change, faq_value_usd_change, tour_value_usd_change, net_value_used_change
data <- wc %>% 
  # Format capture fisheries
  select(rcp, country_use_iso3, value_usd_diff_scaled, production_mt_diff_scaled, value_usd_diff_scaled) %>% 
  rename(iso3=country_use_iso3,
         wc_value_usd_change=value_usd_diff_scaled, 
         wc_prod_mt_change=production_mt_diff_scaled) %>% 
  # Add finfish mariculture (base case)
  full_join(faq_for_merge, by=c("rcp", "iso3")) %>% 
  # Add bivalve aquaculture
  full_join(select(baq, rcp, ter1_iso_use, value_usd_diff_scaled, production_mt_diff_scaled), by=c("rcp", "iso3"="ter1_iso_use")) %>% 
  rename(baq_value_usd_change=value_usd_diff_scaled, 
         baq_prod_mt_change=production_mt_diff_scaled) %>% 
  # Add tourism
  full_join(select(tr, rcp, iso3, value_usd_diff), by=c("rcp", "iso3")) %>% 
  rename(tour_value_usd_change=value_usd_diff) %>% 
  # Replace all NAs with 0s
  mutate(wc_value_usd_change=ifelse(is.na(wc_value_usd_change), 0, wc_value_usd_change),
         wc_prod_mt_change=ifelse(is.na(wc_prod_mt_change), 0, wc_prod_mt_change)) %>% 
  mutate(faq_bau_value_usd_change=ifelse(is.na(faq_bau_value_usd_change), 0, faq_bau_value_usd_change),
         faq_bau_prod_mt_change=ifelse(is.na(faq_bau_prod_mt_change), 0, faq_bau_prod_mt_change)) %>% 
  mutate(faq_reform_value_usd_change=ifelse(is.na(faq_reform_value_usd_change), 0, faq_reform_value_usd_change),
         faq_reform_prod_mt_change=ifelse(is.na(faq_reform_prod_mt_change), 0, faq_reform_prod_mt_change)) %>% 
  mutate(faq_need_value_usd_change=ifelse(is.na(faq_need_value_usd_change), 0, faq_need_value_usd_change),
         faq_need_prod_mt_change=ifelse(is.na(faq_need_prod_mt_change), 0, faq_need_prod_mt_change)) %>% 
  mutate(baq_value_usd_change=ifelse(is.na(baq_value_usd_change), 0, baq_value_usd_change),
         baq_prod_mt_change=ifelse(is.na(baq_prod_mt_change), 0, baq_prod_mt_change)) %>% 
  mutate(tour_value_usd_change=ifelse(is.na(tour_value_usd_change), 0, tour_value_usd_change)) %>% 
  # Calculate net change
  mutate(bau_net_value_usd_change=rowSums(.[c("wc_value_usd_change", "baq_value_usd_change", "faq_bau_value_usd_change", "tour_value_usd_change")], na.rm=T),
         bau_net_prod_mt_change=rowSums(.[c("wc_prod_mt_change", "baq_prod_mt_change", "faq_bau_prod_mt_change")], na.rm=T),
         reform_net_value_usd_change=rowSums(.[c("wc_value_usd_change", "baq_value_usd_change", "faq_reform_value_usd_change", "tour_value_usd_change")], na.rm=T),
         reform_net_prod_mt_change=rowSums(.[c("wc_prod_mt_change", "baq_prod_mt_change", "faq_reform_prod_mt_change")], na.rm=T),
         need_net_value_usd_change=rowSums(.[c("wc_value_usd_change", "baq_value_usd_change", "faq_need_value_usd_change", "tour_value_usd_change")], na.rm=T),
         need_net_prod_mt_change=rowSums(.[c("wc_prod_mt_change", "baq_prod_mt_change", "faq_need_prod_mt_change")], na.rm=T)) %>% 
  # Classify net change
  mutate(bau_value_type=ifelse(bau_net_value_usd_change<0, "Net loser", 
                            ifelse(wc_value_usd_change>=0 & baq_value_usd_change>=0 & faq_bau_value_usd_change>=0 & tour_value_usd_change>=0, "Net winner", "Net offsetter")),
         bau_prod_type=ifelse(bau_net_prod_mt_change<0, "Net loser", 
                          ifelse(wc_prod_mt_change>=0 & baq_prod_mt_change>=0 & faq_bau_prod_mt_change>=0, "Net winner", "Net offsetter")),
         reform_value_type=ifelse(reform_net_value_usd_change<0, "Net loser", 
                                ifelse(wc_value_usd_change>=0 & baq_value_usd_change>=0 & faq_reform_value_usd_change>=0 & tour_value_usd_change>=0, "Net winner", "Net offsetter")),
         reform_prod_type=ifelse(reform_net_prod_mt_change<0, "Net loser", 
                               ifelse(wc_prod_mt_change>=0 & baq_prod_mt_change>=0 & faq_reform_prod_mt_change>=0, "Net winner", "Net offsetter")),
         need_value_type=ifelse(need_net_value_usd_change<0, "Net loser", 
                                  ifelse(wc_value_usd_change>=0 & baq_value_usd_change>=0 & faq_need_value_usd_change>=0 & tour_value_usd_change>=0, "Net winner", "Net offsetter")),
         need_prod_type=ifelse(need_net_prod_mt_change<0, "Net loser", 
                                 ifelse(wc_prod_mt_change>=0 & baq_prod_mt_change>=0 & faq_need_prod_mt_change>=0, "Net winner", "Net offsetter"))) %>% 
  # Arrange columns
  select(rcp, iso3, 
         wc_value_usd_change, baq_value_usd_change, faq_bau_value_usd_change, faq_reform_value_usd_change, tour_value_usd_change, 
         bau_net_value_usd_change, bau_value_type, reform_net_value_usd_change, reform_value_type,
         wc_prod_mt_change, baq_prod_mt_change, faq_bau_prod_mt_change, faq_reform_prod_mt_change,
         bau_net_prod_mt_change, bau_prod_type, reform_net_prod_mt_change, reform_prod_type, everything())


# Types
table(data$rcp, data$bau_value_type)
table(data$rcp, data$reform_value_type)
table(data$rcp, data$need_value_type)

table(data$rcp, data$bau_prod_type)
table(data$rcp, data$reform_prod_type)
table(data$rcp, data$need_prod_type)

n_distinct(data$iso3)


# Plot profit type
#########################

# Reshape data for plotting
data_plot <- data %>% 
  select(rcp, iso3, bau_prod_type, reform_prod_type, need_prod_type, bau_value_type, reform_value_type, need_value_type) %>% 
  gather(key="scenario", value="value_type", 3:ncol(.)) %>%
  mutate(metric=ifelse(grepl("prod", scenario), "Production", "Profits"), 
         scenario=ifelse(grepl("bau", scenario), "Business-as-usual", 
                         ifelse(grepl("reform", scenario), "Progressive reforms", "Need-based progressive reforms"))) %>% 
  select(rcp, scenario, metric, iso3, value_type) %>% 
  arrange(rcp, scenario, metric, iso3) %>% 
  mutate(scenario_label=recode(scenario, 
                               "Business-as-usual"="Business-as-usual",
                               "Progressive reforms"="Progressive reforms",
                               "Need-based progressive reforms"="Need-based progressive reforms"), 
         scenario_label=factor(scenario_label, levels=c("Business-as-usual",
                                                        "Progressive reforms",
                                                        "Need-based progressive reforms"))) %>% 
  mutate(value_type=recode(value_type, "Net winner"="All gains", "Net offsetter"="Net offsets", "Net loser"="Net losses"), 
         value_type=factor(value_type, levels=c("Net losses", "Net offsets", "All gains")))

# Add results to world
data_sf <- world %>% 
  left_join(data_plot, by=c("iso3"="iso3")) %>% 
  filter(!is.na(rcp))

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot
g1 <- ggplot() +
  geom_sf(world, mapping=aes(), lwd=0.2, fill="grey70") +
  geom_sf(filter(data_sf, metric=="Profits"), mapping=aes(fill=value_type), lwd=0.2) +
  facet_grid(rcp ~ scenario_label) +
  # labs(title="Net impact of climate change and adaptation\non profits from the ocean ecosystem economy") +
  scale_fill_discrete(name="", na.value = "grey30") +
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g1

# Export figure
ggsave(g1, filename=file.path(dir_plots, "figure_map_country_type_profits.png"), 
       width=6.5, height=5, units="in", dpi=600)

# Plot
g2 <- ggplot() +
  geom_sf(world, mapping=aes(), lwd=0.2, fill="grey70") +
  geom_sf(filter(data_sf, metric=="Production"), mapping=aes(fill=value_type), lwd=0.2) +
  facet_grid(rcp ~ scenario_label) +
  # labs(title="Net impact of climate change and adaptation\non food from the ocean ecosystem economy") +
  scale_fill_discrete(name="", na.value = "grey30") +
  theme_bw() + my_theme +
  theme(legend.position = "bottom")
g2

# Export figure
ggsave(g2, filename=file.path(dir_plots, "figure_map_country_type_food.png"), 
       width=6.5, height=5, units="in", dpi=600)



  
  
  
  










