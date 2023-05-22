# Load Libraries and Data Files ------------------------------------------------
# Create data plots for FASTR synthesis Report 
# Using correct biovolume data (revised Feb 2022)
# 2/15/2022

library(tidyverse)
library(lubridate)
library(RColorBrewer)

# Set working directory
setwd("./phyto_code/")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Load biovolume density data at group and genus level
load("RData/phyto.sum.RData")
load("RData/phyto_gen_EMP.RData")
load("RData/phyto.gen.RData")
load("RData/phyto.gen.NMDS.RData")
load("RData/phyto.grp.gen.BV.RData")
load("RData/phyto.grp.BV.RData")
load("RData/phyto.grp.BM.RData")
load("RData/phyto.grp.LCEFA.RData")
load("RData/phyto.grp.sum.error.RData")
load("RData/FlowDesignation.RData")
load("RData/phyto.grp.gen.BV.RA.tot.Rdata")
load("RData/phyto.types")

test <- phyto.grp.BV %>%
  group_by(Year, StationCode, Region, ActionPhase, DateTime) %>%
  summarize(Total.BV = sum(BV.um3.per.L, na.rm = TRUE)) %>%
  ungroup

plot <- ggplot(test, aes(x = Year, fill = ActionPhase)) +
  geom_histogram()

plot

plot2 <- ggplot(test, aes(x = Year, fill = Region)) +
  geom_histogram()

plot2

plot3 <- ggplot(test, aes(x = Year, fill = StationCode)) +
  geom_histogram()

plot3

# Create Biovolume-only data frame at genus level
phyto.gen.BV <- phyto.gen %>% select(Year:Region,Genus:ActionPhase,BV.um3.per.L)

# Create custom color palette
brewer.pal(n=8, name = "Set1")

# Set folder name to output graphs into
output <- "plots"

# Format Flow Designation data frame for graphing
FlowDesignation <- pivot_longer(FlowDesignation, 
                                cols = PreFlowStart:PostFlowEnd,
                                names_to = "Phase",
                                values_to = "Date")

FlowDesignation$Date <- as_datetime(FlowDesignation$Date,tz = "US/Pacific")

# Pie Chart for Overall Taxa ---------------------------------------------------
# Plot general relative abundance for all data.
phyto.RA <- phyto.grp.BV %>%
  group_by(Group) %>%
  summarize(Mean.BV.per.L = mean(BV.um3.per.L)) %>%
  mutate(MeanRelAbund = Mean.BV.per.L/sum(Mean.BV.per.L)) %>%
  ungroup()

# Set group display order
phyto.RA <- phyto.RA %>% 
  arrange(desc(MeanRelAbund)) %>%
  mutate(Group = factor(Group, levels = Group))

#type.order <- c("Aulacoseira","Cyclotella","Ulnaria","Other")
#phyto.dia.BV$Type <- factor(as.character(phyto.dia.BV$Type), levels =  type.order)

phyto.BV.RA.pie <- ggplot(phyto.RA, aes(x = "", y = MeanRelAbund, fill = Group)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void()

phyto.BV.RA.pie 

ggsave(path = output,
       filename = "phyto_BV_RA_pie.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=3.5,
       width=3, 
       dpi="print")

# Relative Abundance Plots -----------------------------------------------------
# Make individual bar plots to export to Illustrator
groups <- unique(phyto.grp.gen.BV.RA.tot$Group) 

for (group in groups) {
  df_temp <- phyto.grp.gen.BV.RA.tot %>%
    filter(Group == group)
  
  RA.plot <- ggplot(df_temp, 
                    aes(x = Group, 
                        y = MeanRelAbund, 
                        fill = Type)) +
    geom_bar(data = df_temp, 
             position = "stack",  
             width = 1, 
             stat = "summary", 
             fun = "mean") +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Group", 
         y = "Relative Abundance (%)") 
  
  RA.plot 
  
  ggsave(path = output,
         filename = paste0("phyto_RA_",group,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3.5,
         width=3, 
         dpi="print")
  
  rm(df_temp)
  
}

# Box plots for Total Phytoplankton Biovolume ----------------------------------

# Plot Upstream and Downstream Total BV by ActionPhase for each year
fig1 <- ggplot(data=phyto.sum, aes(y= log10(Total.BV.per.L), 
                                       x = Region,
                                       color = ActionPhase)) +
  geom_boxplot() 

fig1 +
  labs(title = "Total Phytoplankton Abundance by Year",
       y = bquote(Log[10]~'Biovolume Density'~(um^3~L^-1)), 
       x = "Sampling Region",
       color = "Pulse Period") +
  facet_wrap(Year ~ ., ncol = 2)  

ggsave(path = output,
       filename = "fig1_log_phyto_biovolume_by_year_and_AP.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

# Plot Total BV by Flow Pulse Type at Each Station

fig2 <- ggplot(data=phyto.sum, aes(y= log10(Total.BV.per.L), 
                                       x = FlowPulseCategory,
                                       color = ActionPhase)) +
  geom_boxplot()

fig2 +
  labs(title = "Total Phytoplankton Abundance by Flow Pulse Type",
       y = bquote(Log[10]~'Biovolume Density'~(um^3~L^-1)), 
       x = "Flow Pulse Category") +
  facet_wrap(StationCode ~ ., ncol = 2)

ggsave(path = output,
       filename = "fig2_log_phyto_biovolume_by_site_and_pulse_type.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=8,
       width=6.5, 
       dpi="print")

# Bar Charts with Error Bars --------------------------------------------------- 

fig3 <- ggplot(phyto.sum, aes(x = Year, y = log10(Total.BV.per.L), fill = ActionPhase)) +
  geom_jitter(width = 0.1,
              size = 2,
              pch = 21, 
              color = "black") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  #scale_fill_brewer(palette = "Dark2") +
  labs(x = "Year", 
       y = bquote(Log[10]~'Biovolume Density'~(um^3~L^-1)), 
       title = "Yearly Abundance of Phytoplankton During Flow Pulses") 

fig3 + 
  facet_wrap(StationCode ~ ., ncol = 2, dir = "h") 

# fig3 <- ggplot(phyto.grp.sum.error, aes(x=Year, 
#                                         y=Total.BV.per.L, 
#                                         ymin=Total.BV.per.L-se, 
#                                         ymax=Total.BV.per.L+se, 
#                                         fill=ActionPhase)) +
#    geom_bar(position=position_dodge(), 
#             aes(y=Total.BV.per.L), 
#             stat="identity", 
#             color = "black") +
#    geom_errorbar(position=position_dodge(width=0.9), 
#                  color="black", 
#                  width = 0.2) 
# 
# fig3 +
#   ylab(bquote('Biovolume Density'~(um^3~L^-1))) + 
#   xlab("Month") + 
#   labs(title = "Phytoplankton Abundance During Flow Actions") +
#   facet_wrap(StationCode ~ ., ncol = 2, dir = "v") 

ggsave(path = output,
       filename = "fig3_phyto_BV_jitter.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=7.5,
       width=6, 
       dpi="print")

# Stacked Bar Charts for Biovolume ---------------------------------------------

# Upstream/Downstream
fig4 <- ggplot(phyto.grp.BV, aes(x = ActionPhase, 
                              y = BV.um3.per.L, 
                              fill = Group)) + 
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) 

fig4 + 
  labs(x = NULL, 
       y = bquote('Average Biovolume Density'~(um^3~L^-1)), 
       title = paste0("Abundance of Phytoplankton Groups During Flow Pulses")) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#FFFF33",
                               "#A65628",
                               "#F781BF")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  facet_grid(Region~Year) # dir = v makes order of station go "north to south"

ggsave(path = output,
       filename = paste0("fig4_phyto_group_biovolume_by_year.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")

## Create biovolume plots for each year
years <- unique(phyto.grp.BV$Year) 
years <- sort(years, decreasing = F, na.last = T)

for (year in years) {
  df_temp <- phyto.grp.BV %>%
    filter(Year == year)
  
  biovolume.plot <- ggplot(phyto.grp.BV, 
                           aes(x = ActionPhase, 
                               y = BV.um3.per.L, 
                               fill = Group)) +
    geom_bar(data = subset(phyto.grp.BV, Year == year), 
             position = "stack",  
             width = 0.6, 
             stat = "summary", 
             fun = "mean") +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Pulse Period", 
         y = bquote('Average Biovolume Density'~(um^3~L^-1)), 
         title = paste0("Abundance of Phytoplankton Groups During Flow Pulses - ",year)) 
  
  biovolume.plot +
    scale_fill_manual(values = c("#E41A1C",
                                 "#377EB8",
                                 "#4DAF4A",
                                 "#984EA3",
                                 "#FF7F00",
                                 "#FFFF33",
                                 "#A65628",
                                 "#F781BF")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
    facet_wrap(StationCode ~ ., ncol = 5, dir = "h")
  
  ggsave(path = output,
         filename = paste0("phyto_avg_BV_by_station_",year,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3.5,
         width=7, 
         dpi="print")
  
  rm(df_temp)
  
}

# Recreate Jared's Plots from 2016 ---------------------------------------------

for (year in years) {
  df_temp <- phyto.grp.BV %>% filter(Year == year)
  
  biovolume.plot <- ggplot(phyto.grp.BV, aes(x = StationCode, y = BV.um3.per.L, fill = Group)) +
    geom_bar(data = subset(phyto.grp.BV, Year == year), 
             position = "stack",  
             width = 0.6, 
             stat = "summary", 
             fun = "mean") +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Station", 
         y = "Biovolume Density (um^3 per mL)", 
         title = paste0("Abundance During Flow Pulses - ",year)) 
  
  biovolume.plot +
    scale_fill_manual(values = c("#E41A1C",
                                 "#377EB8",
                                 "#4DAF4A",
                                 "#984EA3",
                                 "#FF7F00",
                                 "#FFFF33",
                                 "#A65628",
                                 "#F781BF")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
    facet_wrap(ActionPhase ~ ., ncol = 1, dir = "h")
  
  ggsave(path = output,
         filename = paste0("phyto_avg_BV_by_AP_",year,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3.5,
         width=5.5, 
         dpi="print")
  
  rm(df_temp)
  
}

# Relative Abundance Bar Plots -------------------------------------------------

# Summarize Total Biovolumes to look at relative contributions by Year

phyto.grp.BV.RA.plot <- ggplot(phyto.grp.BV.RA, aes(x = ActionPhase, y = MeanRelAbund, fill = Group)) +
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Station", 
       y = "Relative Biovolume Density (%)", 
       title = "Relative Abundance of Phytoplankton During Flow Pulses") 

phyto.grp.BV.RA.plot +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#FFFF33",
                               "#A65628",
                               "#F781BF")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
  facet_grid(Region ~ Year)

ggsave(path = output,
       filename = "phyto_BV_relative_abund.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=3.5,
       width=5.5, 
       dpi="print")

## Make individual abundance jitter plots for each year biovolume 
years <- unique(phyto.grp.gen.BV$Year) 
years <- sort(years, decreasing = F, na.last = T)

phyto.grp.gen.BV <- left_join(phyto.grp.gen.BV, phyto.types)

for (year in years) {

  df_temp <- phyto.grp.gen.BV %>% 
    filter(Year==year) %>%
    filter(Group == "Diatoms")
  
  jitter.plot <- ggplot(df_temp, aes(x = StationCode, y = BV.um3.per.L, color = Type, shape = Type)) +
    geom_jitter(width = 0.1, size = 2) +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Station", 
         y = "Biovolume (um^3 per L)", 
         title = paste0("Diatom Biovolume During Flow Pulses - ",year)) 
    #ylim(c(0,2e10))
  
  jitter.plot +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
    facet_wrap(ActionPhase ~ ., ncol = 1, dir = "h")
  
  ggsave(path = output,
         filename = paste0("jitter_Diatom_BV_by_station_and_Region_",year,".pdf"), 
         device = "pdf",
         scale=1.0, 
         units="in",
         height=3.5,
         width=6.5, 
         dpi="print")
  
  rm(df_temp)
  
}

## Make individual abundance jitter plots for each year LCEFA 
years <- unique(phyto.grp.LCEFA$Year) 
years <- sort(years, decreasing = F, na.last = T)

for (year in years) {
  
  df_temp <- phyto.grp.LCEFA %>% 
    filter(Year==year)
  
  LCEFA.plot <- ggplot(df_temp, aes(x = StationCode, y = LCEFA.per.L, fill = Group, shape = Group)) +
    geom_bar(data = subset(phyto.grp.LCEFA, Year == year), 
             position = "stack",  
             width = 0.6, 
             stat = "summary", 
             fun = "mean") +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Station", 
         y = "LCEFA (ug per L)", 
         title = paste0("Est. Mass of LCEFA During Flow Pulses - ",year)) 
  #ylim(c(0,2e10))
  
  LCEFA.plot +
    scale_fill_brewer(palette = "Dark2") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
    facet_wrap(ActionPhase ~ ., ncol = 1, dir = "h")
  
  ggsave(path = output,
         filename = paste0("barplot_LCEFA_by_station_and_Region_",year,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3.5,
         width=6.5, 
         dpi="print")
  
  rm(df_temp)
  
}

# Compare Cyclotella Abundance Upstream and Downstream -------------------------

cyc.plot <- ggplot(phyto.gen.BV, 
                   aes(x = Region, y = log10(BV.um3.per.L+1))) +
  geom_jitter(data = subset(phyto.gen.BV, Genus == "Cyclotella"),
              width = 0.1) +
  geom_boxplot(data = subset(phyto.gen.BV, Genus == "Cyclotella"),
              width = 0.1,
              outlier.shape = NA)

cyc.plot +
  labs(title = "Yearly Abundance of Cyclotella",
       y = bquote(Log[10]~'Biovolume Density'~(um^3~L^-1)), 
       x = NULL) +
  facet_wrap(Year ~ ., ncol = 3)

ggsave(path = output,
       filename = "cyclotella_abundance_plot.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=3.5,
       width=6.5, 
       dpi="print")

# NMDS Plots -------------------------------------------------------------------

# Create NMDS plots for each year by Region
NMDS.Region.plot <- ggplot(phyto.gen.NMDS, aes(x = NMDS1, y = NMDS2, color = Region)) +
  geom_point(size = 3) +
  stat_ellipse() + 
  labs(title = "Phytoplankton Community Composition") +
  labs(color = "Region") +
  theme_bw()

NMDS.Region.plot +
  facet_wrap(Year ~ ., ncol = 3, dir = "h")

ggsave(path = output,
       filename = paste0("phyto_NMDS_biovolume_by_region.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=3,
       width=6.5, 
       dpi="print")

# Create biovolume NMDS plots for each year by Region

# Plot NMDS in ggplot2

NMDS.AP.plot <- ggplot(phyto.gen.NMDS, aes(x = NMDS1, y = NMDS2, color = ActionPhase)) +
  geom_point(size = 3) +
  stat_ellipse() + 
  labs(title = "Phytoplankton Community Composition") +
  labs(color = "Region") +
  theme_bw()

NMDS.AP.plot +
  facet_wrap(Year ~ ., ncol = 3, dir = "h")

ggsave(path = output,
       filename = paste0("phyto_NMDS_biovolume_by_AP.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=3,
       width=6.5, 
       dpi="print")

# LCEFA Stacked Bar Plots ------------------------------------------------------

# Create stacked bar chart for yearly LCEFA abundance by Action Phase
# and Upstream/Downstream
fig12 <- ggplot(phyto.grp.LCEFA, aes(x = ActionPhase, 
                              y = LCEFA.per.L, 
                              fill = Group)) + 
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) 

fig12 + 
  labs(x = NULL, 
       y = bquote('Average LCEFA'~(?g~L^-1)), 
       title = paste0("Estimated Mass of LCEFA for Phytoplankton Groups During Flow Pulses")) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#A65628")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  facet_grid(Region~Year) # dir = v makes order of station go "north to south"

ggsave(path = output,
       filename = paste0("fig12_phyto_group_LCEFA_by_year.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")

# Summarize data to get raw numbers for paper ----------------------------------

## Summarize by group
phyto.LCEFA.RA <- phyto.LCEFA.grp %>% 
  group_by(Year, Region, ActionPhase, Group) %>%
  summarize(LCEFA.Total = mean(LCEFA, na.rm = TRUE)) %>%
  ungroup

phyto.LCEFA.RA <- phyto.LCEFA.RA %>%
  group_by(Year, Region, ActionPhase) %>%
  mutate(LCEFA.RA = LCEFA.Total / sum(LCEFA.Total)) %>%
  ungroup

phyto.BV.RA <- phyto.grp %>% 
  group_by(Year, Region, ActionPhase, Group) %>%
  summarize(BV.Total = mean(BV.Density, na.rm = TRUE)) %>%
  ungroup

phyto.BV.RA <- phyto.BV.RA %>%
  group_by(Year, Region, ActionPhase) %>%
  mutate(BV.RA = BV.Total / sum(BV.Total)) %>%
  ungroup

