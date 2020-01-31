
# Clear
rm(list = ls())

# Setup
#####################################################################################

# Packages
library(tidyverse)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/results"
plotdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/blue_paper/bluepaper2/figures"
outdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/blue_paper/bluepaper2/data/"


# Read data
load(file=file.path(datadir, "BP2_aq_data_scen2.Rdata"))
load(file=file.path(datadir, "BP2_aq_data_scen3c.Rdata")) # overwrites "fdata" from above


# Calculate current country-level AQ production
#####################################################################################

# Load AQ data
aq_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/processed/1950_2018_FAO_aq_production.Rdata")

# Build dataset
aq <- aq_orig %>% 
  # Summarize 2016 bivalve/finfish production by country
  filter(year==2016 & type2=="marine and coastal") %>% 
  group_by(country, iso3, group) %>% 
  summarize(prod_mt=sum(quantity_mt, na.rm=T)) %>% 
  filter(group %in% c("Molluscs", "Finfish") & iso3!="" & !is.na(iso3)) %>% 
  spread(key="group", value="prod_mt", fill=0) %>% 
  rename(f_mt_curr=Finfish, b_mt_curr=Molluscs) %>% 
  ungroup()


# Plot data
#####################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8), 
                  strip.text=element_text(size=10),
                  plot.title=element_text(size=10),
                  legend.position = "bottom",
                  # panel.grid.major = element_blank(),
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# World layer
world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf")
world_df <- world %>% as.data.frame() %>% select(-geometry)

# Format bivalve data
bdata1 <- bdata %>% 
  left_join(select(aq, -country), by="iso3") %>% 
  rename(b_mt_pot=tot_mt_yr) %>% 
  mutate(b_mt_curr=ifelse(is.na(b_mt_curr), 0, b_mt_curr),
         b_diff_mt=b_mt_pot-b_mt_curr,
         b_diff_mmt=b_diff_mt/1e6) %>% 
  filter(b_diff_mmt>0)

# Format bivalve data
fdata1 <- fdata %>% 
  left_join(select(aq, -country), by="iso3") %>% 
  rename(f_mt_pot=tot_mt_yr) %>% 
  mutate(f_mt_curr=ifelse(is.na(f_mt_curr), 0, f_mt_curr),
         f_diff_mt=f_mt_pot-f_mt_curr,
         f_diff_mmt=f_diff_mt/1e6) %>% 
  filter(f_diff_mmt>0)

# Export
write.csv(bdata1, file.path(outdir, "aquauculture_bivalve_underages.csv"), row.names=F)
write.csv(fdata1, file.path(outdir, "aquauculture_finfish_underages.csv"), row.names=F)

# Add to world
world_f <- world %>% 
  left_join(fdata1, by=c("gu_a3"="iso3"))

# Add to world
world_b <- world %>% 
  left_join(bdata1, by=c("gu_a3"="iso3"))

# Plot bivalve underages
g1 <- ggplot(world_b) +
  geom_sf(aes(fill=b_diff_mmt), lwd=0.1, col="grey30") +
  scale_fill_gradientn(name="Bivalve production\nunderages (millions mt)",
                       colors = RColorBrewer::brewer.pal(9, "Blues"),
                        na.value="grey90", limits=c(0,120)) +
  guides(fill = guide_colorbar(ticks.colour = "black",
                               frame.colour = "black")) +
  theme_bw() + my_theme
g1

# Plot finfish underages
g2 <- ggplot(world_f) +
  geom_sf(aes(fill=f_diff_mmt), lwd=0.1, col="grey30") +
  scale_fill_gradientn(name="Finfish production\nunderages (millions mt)",
                       colors = RColorBrewer::brewer.pal(9, "Greens"),
                       na.value="grey90", limits=c(0,65)) +
  guides(fill = guide_colorbar(ticks.colour = "black",
                               frame.colour = "black")) +
  theme_bw() + my_theme
g2

# Merge together
g <- plot_grid(g1, g2, nrow=2)
g

# Export
ggsave(file.path(plotdir, "Fig2_country_level_aq_potential2.pdf"), width=6.5, height=7.5, units="in", dpi=600)

