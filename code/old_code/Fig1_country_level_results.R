


# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(cowplot)

# Read data
plotdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/blue_paper/bluepaper2/figures"
eezdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/eezs/processed"
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/rshiny/data/"
outdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/blue_paper/bluepaper2/data/"

# Load data
data1 <- readRDS(file.path(datadir, "gaines_territory_level_results_approach1.Rds"))
data2 <- readRDS(file.path(datadir, "gaines_territory_level_results_approach2.Rds"))
tmsy <- readRDS(file.path(datadir, "gaines_territory_level_msy_time_series_by_type.Rds"))

# Load EEZ shapefile
eezs <- readRDS(file.path(eezdir, "world_eezs_v8.Rds"))

# World layer
world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf")


# Stats for report
################################################################################

stats2 <- data2 %>%
  filter(scenario=="Full Adaptation") %>% 
  group_by(rcp) %>% 
  summarize(perc_c_pos=sum(c_tot_rel>0)/n(),
            perc_p_pos=sum(p_tot_rel>0)/n(),
            perc_cp_pos=sum(c_tot_rel>0 & p_tot_rel>0)/n())

stats1 <- data1 %>%
  filter(scenario=="Full Adaptation") %>% 
  group_by(rcp) %>% 
  summarize(perc_cp_pos=sum(c_pdiff>0 & p_pdiff>0)/n())


# MSY maps
################################################################################

# Calculate percent change in mean MSY from beginning to end
yrs1 <- 2012:2021; length(yrs1)
yrs2 <- 2091:2100; length(yrs2)
msy <- tmsy %>% 
  ungroup() %>% 
  # Sum MSY by EEZ and year
  group_by(rcp, sovereign, sovereign_iso3, country, eez_id, eez, year) %>%
  summarize(msy=sum(msy_mt)) %>% 
  # Average over periods
  group_by(rcp, sovereign, sovereign_iso3, country, eez_id, eez) %>% 
  summarize(msy_avg1=mean(msy[year%in%yrs1]),
            msy_avg2=mean(msy[year%in%yrs2]),
            msy_perc=(msy_avg2-msy_avg1)/msy_avg1*100,
            msy_perc=pmin(300, msy_perc)) %>% 
  # Remove outrageous values
  filter(msy_perc <= 100 & msy_perc>=-100) %>% 
  filter(rcp %in% c("RCP 6.0", "RCP 8.5"))

# Plot distribution of MSY change percentages
hist(msy$msy_perc, breaks=seq(-100,100,10))

# Export
write.csv(msy, file.path(outdir, "capture_fisheries_msy_change_by_rcp_country.csv"), row.names=F)

# Add data to EEZ shapefile
eezs1 <- eezs %>%
  left_join(msy, by="eez") %>% 
  filter(!is.na(rcp)) %>% 
  mutate(rcp=plyr::revalue(rcp, c("RCP26"="RCP 2.6",
                                  "RCP45"="RCP 4.5",
                                  "RCP60"="RCP 6.0",
                                  "RCP85"="RCP 8.5")))

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10), 
                  strip.text=element_text(size=10),
                  plot.title=element_text(size=12),
                  # panel.grid.major = element_blank(), 
                  panel.grid.major = element_line(colour = 'transparent'),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot all
g0 <- ggplot(eezs1) +
  geom_sf(aes(fill=msy_perc), lwd=0.05, col="white") +
  geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
  geom_hline(aes(yintercept = 0), linetype="dotted", color="grey30", size=0.1) +
  facet_wrap(~ rcp, nrow=1) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,50),
                       name="Percent change in MSY\n(2100 relative to today)") +
  ylab(" \n \n \n ") +
  theme_bw() + my_theme + theme(legend.position = "bottom")


# Approach 1
################################################################################

# Legend titles
bbmsy_avg_title1 <- "Mean B/BMSY\nof stocks in 2100"
bbmsy_prop_title1 <- "Proportion of stocks\nwith B/BMSY > 1.0 in 2100"


# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  legend.text=element_text(size=7),
                  legend.title=element_text(size=9), 
                  strip.text=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Format
data1format <- data1 %>% 
  filter(scenario=="Full Adaptation") %>% 
  mutate(c_pdiff=ifelse(abs(c_pdiff)>300, sign(c_pdiff)*300, c_pdiff),
         # Cap percent difference in profits [-100%, 300%]
         p_pdiff=ifelse(p_pdiff < -100, -100, p_pdiff), 
         p_pdiff=ifelse(p_pdiff > 300, 300, p_pdiff),
         # Calculate percent difference in MSY: cap at [-100%, 100%]
         msy_pdiff=(msy_tot2-msy_tot1)/msy_tot1*100,
         msy_pdiff=ifelse(abs(msy_pdiff)>100, sign(msy_pdiff)*100, msy_pdiff)) %>% 
  filter(rcp %in% c("RCP 6.0", "RCP 8.5"))

# Export
write.csv(data1format, file.path(outdir, "capture_fisheries_outcomes_2100totoday_under_full_adaptation.csv"), row.names=F)


# Plot data
g1 <- ggplot(data1format, aes(x=c_pdiff, y=p_pdiff, size=msy_tot1/1e6, fill=msy_pdiff)) +
  geom_hline(yintercept=0, lty=2, col="grey30") +
  geom_vline(xintercept=0, lty=2, col="grey30") +
  geom_point(pch=21) +
  facet_wrap(~ rcp, nrow=1) +
  xlab("% difference in catch\n(2100 relative to today)") + 
  ylab("% difference in profits\n(2100 relative to today)") + 
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,25),
                       labels=seq(-100,100,25),
                       name="Percent change in MSY\n(2100 relative to today)", guide="none") +
  scale_size_continuous(name="MSY (millions mt)") +
  theme_bw() + my_theme + theme(legend.position=c(0.9, 0.25))
g1

# Approach 2
################################################################################

# Format
data2format <- data2 %>% 
  filter(scenario=="Full Adaptation") %>% 
  mutate(c_tot_rel=ifelse(abs(c_tot_rel)>300, sign(c_tot_rel)*300, c_tot_rel),
         p_tot_rel=ifelse(abs(p_tot_rel)>300, sign(p_tot_rel)*300, p_tot_rel), 
         msy_pdiff=(msy_tot2-msy_tot1)/msy_tot1*100,
         msy_pdiff=ifelse(abs(msy_pdiff)>100, sign(msy_pdiff)*100, msy_pdiff)) %>% 
  filter(rcp %in% c("RCP 6.0", "RCP 8.5"))

# Plot data
g2 <- ggplot(data2format, aes(x=c_tot_rel, y=p_tot_rel, size=msy_tot1/1e6, fill=msy_pdiff)) +
  geom_hline(yintercept=0, lty=2, col="grey30") +
  geom_vline(xintercept=0, lty=2, col="grey30") +
  geom_point(pch=21) +
  facet_wrap(~ rcp, nrow=1) +
  xlab("% difference in cumulative catch\n(adaptation relative to business-as-usual)") + 
  ylab("% difference in cumulative profits\n(adaptation relative to business-as-usual)") + 
  ylim(0,300) +
  scale_fill_gradientn(colours=brewer.pal(9, "RdBu"), 
                       limits=c(-100,100),
                       breaks=seq(-100,100,25),
                       labels=seq(-100,100,25),
                       name="Percent change in MSY\n(2100 relative to today)") +
  scale_size_continuous(name="MSY (millions mt)") +
  theme_bw() + my_theme + theme(legend.position="none")
g2

# Export
write.csv(data2format, file.path(outdir, "capture_fisheries_outcomes_full_adaptation_compared_to_bau.csv"), row.names=F)


# Combine plots
################################################################################


g <- plot_grid(g0, g1, g2, nrow=3, rel_heights=c(0.3, 0.7/2, 0.7/2), labels=c("A", "B", "C"))
g


# Export
ggsave(file.path(plotdir, "Fig1_country_level_results.pdf"), width=6.5, height=9, units="in", dpi=600)


