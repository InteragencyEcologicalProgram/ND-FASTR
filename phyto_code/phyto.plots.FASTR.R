## Create data plots for FASTR synthesis Report 
## Using correct biovolume data (revised Feb 2022)
## 2/15/2022

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("RColorBrewer");packageVersion("RColorBrewer")

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

## Load biovolume density data at group and genus level
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

## Create Biovolume-only data frame at genus level
phyto.gen.BV <- phyto.gen %>% select(Year:Region,Genus:ActionPhase,BV.um3.per.L)

## Create custom palette
brewer.pal(n=8, name = "Set1")

# Set folder name to output graphs into
output <- "plots"

## Combine EMP and FASTR datasets for Aulacosiera graphs
phyto_gen_FASTR <- phyto.gen.BV %>% select(Year:Genus,BV.um3.per.L)
phyto_gen_EMP <- phyto_gen_EMP %>% select(Year:Genus,BV.um3.per.L)

# Add column identifying study
phyto_gen_EMP <- phyto_gen_EMP %>% mutate(Study = "EMP")
phyto_gen_FASTR <- phyto_gen_FASTR %>% mutate(Study = "FASTR")

# Combine data frames
phyto_combined <- bind_rows(phyto_gen_FASTR, phyto_gen_EMP)

# Rename FASTR Regions
phyto_combined$Region <- gsub("Downstream", "NDFA.Downstream", phyto_combined$Region)
phyto_combined$Region <- gsub("Upstream", "NDFA.Upstream", phyto_combined$Region)

# Remove bay stations and EZs and filter EMP data for FASTR years only
phyto_combined <- phyto_combined %>%
  filter(!Region %in% c("San.Pablo.Bay","Grizzly.and.Suisun.Bay","Entrapment.Zone")) %>%
  filter(Year >= 2014 & Year <= 2019) 

## Add Flow Designation to combined data frame
# Remove Water Year Type and Flow Pulse Type data
FlowDesignation <- FlowDesignation %>% select(Year:PostFlowEnd)

phyto_combined <- inner_join(phyto_combined,FlowDesignation, by = "Year")   
phyto_combined <- phyto_combined %>%
  mutate(ActionPhase = ifelse(DateTime > PreFlowStart & DateTime < PreFlowEnd, "Before", NA)) %>%
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PreFlowEnd & DateTime < PostFlowStart, "During")) %>% ## added >= to avoid removing samples that fall on PreFlowEnd date
  mutate(ActionPhase = replace(ActionPhase, DateTime >= PostFlowStart & DateTime < PostFlowEnd, "After")) %>% ## added >= to avoid removing samples that fall on PostFlowStart date
  filter(!is.na(ActionPhase)) %>%
  select(-c(PreFlowStart:PostFlowEnd))

# Order the new ActionPhase so that it plots in the order Pre < During < Post
phase.order <- c("Before","During","After")
phyto_combined$ActionPhase <- factor(as.character(phyto_combined$ActionPhase), levels = phase.order)

# Set year as factor for graphing
phyto_combined$Year <- as.factor(phyto_combined$Year)
  
# Re-order stations from roughly North to South
regions <- c("NDFA.Upstream", "NDFA.Downstream", "Northern.Interior.Delta",
             "Confluence", "Central.Delta", "Southern.Interior.Delta")
phyto_combined$Region <- factor(as.character(phyto_combined$Region), levels = regions)

# Add zeros to non-detects
temp <- pivot_wider(phyto_combined, 
                    names_from = Genus,
                    values_from = BV.um3.per.L,
                    values_fill = 0)

phyto_combined <- pivot_longer(temp,
                             cols = Chlorella:last_col(),
                             names_to = "Genus",
                             values_to = "BV.um3.per.L")

rm(temp)

# Plot biovolume of Aulacoseira yearly at EMP and FASTR sites
Aul.BV.year <- ggplot(data = subset(phyto_combined, Genus == "Aulacoseira"), 
                      aes(x = Year, y = BV.um3.per.L)) +
  geom_jitter(width = 0.1,
              size = 2,
              pch = 21, 
              color = "black",
              fill = "darkgreen") +
  labs(x = "Year",
       y = "Biovolume (um^3 per L)",
       title = "Abundance of Aulacoseira Diatoms 2014 - 2019") +
  scale_fill_brewer(palette = "Dark2")

Aul.BV.year +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
  facet_wrap(Region ~ ., ncol = 3) 

ggsave(path = output,
       filename = paste0("Aulacoseira_abundance_FASTR_EMP_yearly.pdf"), 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=3.5,
       width=6.5, 
       dpi="print")

# Plot biovolume of Aulacoseira monthly in 2016 only at EMP and FASTR sites
Aul.BV.2016 <- ggplot(data = subset(phyto_combined, Genus == "Aulacoseira" & Year == 2016), 
                      aes(x = ActionPhase, y = BV.um3.per.L)) +
  geom_jitter(width = 0.1, 
              size = 2,
              pch = 21,
              color = "black",
              fill = "darkgreen") +
  labs(x = "Pulse Period",
       y = "Biovolume (um^3 per L)",
       title = "Abundance of Aulacoseira Diatoms During Pulse Periods - 2016")

Aul.BV.2016 +
  facet_wrap(Region ~ ., ncol = 3)

ggsave(path = output,
       filename = paste0("Aulacoseira_abundance_FASTR_EMP_2016.pdf"), 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=3.5,
       width=6.5, 
       dpi="print")

`## Format Flow Designation data frame for graphing
FlowDesignation <- pivot_longer(FlowDesignation, 
                                cols = PreFlowStart:PostFlowEnd,
                                names_to = "Phase",
                                values_to = "Date")

FlowDesignation$Date <- as_datetime(FlowDesignation$Date,tz = "US/Pacific")


## Create pie chart showing overall distribution of taxa
## Plot general relative abundance for all data
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

## Create relative abundance plots as individual bar plots to export to Illustrator
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




## Create box plots 
# Plot Upstream and Downstream Total BV by ActionPhase for each year
fig1 <- ggplot(data=phyto.sum, aes(y= log10(Total.BV.per.L), 
                                       x = Region,
                                       color = ActionPhase)) +
  geom_boxplot() 

fig1 +
  labs(title = "Total Phytoplankton Biovolume by Year",
       y = bquote(Log[10]~'Total Phyto Biovolume'~(um^3~L^-1)), 
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
  labs(title = "Total Phytoplankton Biovolume by Flow Pulse Type",
       y = "Log10 Total Phyto Biovolume (um^3 per L)", 
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

## Make bar charts with error bars for each station 
fig3 <- ggplot(phyto.grp.sum.error, aes(x=Year, 
                                        y=Total.BV.per.L, 
                                        ymin=Total.BV.per.L-se, 
                                        ymax=Total.BV.per.L+se, 
                                        fill=ActionPhase)) +
  geom_bar(position=position_dodge(), 
           aes(y=Total.BV.per.L), 
           stat="identity", 
           color = "black") +
  geom_errorbar(position=position_dodge(width=0.9), 
                color="black", 
                width = 0.2) 

fig3 +
  ylab("Biovolume (um^3 per L)") + 
  xlab("Month") + 
  labs(title = "Phytoplankton Biovolume During Flow Actions") +
  facet_wrap(StationCode ~ ., ncol = 2, dir = "v") 

ggsave(path = output,
       filename = "fig3_phyto_BV_w_errorbars.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=7.5,
       width=6, 
       dpi="print")

## Create stacked bar chart for yearly biovolume by Action Phase
## and Upstream/Downstream
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
       y = bquote('Average Biovolume'~(um^3~L^-1)), 
       title = paste0("Biovolume of Phytoplankton Groups During Flow Pulses")) + 
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
         y = bquote('Average Biovolume'~(um^3~L^-1)), 
         title = paste0("Biovolume of Phytoplankton Groups During Flow Pulses - ",year)) 
  
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

## Create biovolume plots similar to Jared's

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
         y = "Biovolume (um^3 per mL)", 
         title = paste0("Biovolume During Flow Pulses - ",year)) 
  
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

## Create relative abundance plots 
## Summarize Total Biovolumes to look at relative contributions by Year

phyto.grp.BV.RA.plot <- ggplot(phyto.grp.BV.RA, aes(x = ActionPhase, y = MeanRelAbund, fill = Group)) +
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Station", 
       y = "Relative Biovolume (%)", 
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

## Make individual abundance jitter plots for each year
years <- unique(phyto.grp.gen.BV$Year) 
years <- sort(years, decreasing = F, na.last = T)

phyto.grp.gen.BV <- left_join(phyto.grp.gen.BV, phyto.types)

for (year in years) {

  df_temp <- phyto.grp.gen.BV %>% 
    filter(Year==year) %>%
    filter(Group == "Dinoflagellates")
  
  jitter.plot <- ggplot(df_temp, aes(x = StationCode, y = BV.um3.per.L, color = Type, shape = Type)) +
    geom_point(width = 0.1, size = 2) +
    theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = "Station", 
         y = "Biovolume (um^3 per L)", 
         title = paste0("Dinoflagellate Biovolume During Flow Pulses - ",year)) 
    #ylim(c(0,2e10))
  
  jitter.plot +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.7)) +
    facet_wrap(ActionPhase ~ ., ncol = 1, dir = "h")
  
  ggsave(path = output,
         filename = paste0("jitter_Dinoflagellates_BV_by_station_and_Region_",year,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=3.5,
         width=6.5, 
         dpi="print")
  
  rm(df_temp)
  
}

## Compare Cyclotella upstream and downstream
cyc.plot <- ggplot(phyto.gen.BV, 
                   aes(x = Region, y = log10(BV.um3.per.L+1))) +
  geom_jitter(data = subset(phyto.gen.BV, Genus == "Cyclotella"),
              width = 0.1) +
  geom_boxplot(data = subset(phyto.gen.BV, Genus == "Cyclotella"),
              width = 0.1,
              outlier.shape = NA)

cyc.plot +
  facet_wrap(Year ~ ., ncol = 3)

ggsave(path = output,
       filename = paste0("cyclotella_abundance_plot.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=3.5,
       width=6.5, 
       dpi="print")

# Time-series plots of Aulacoseira sp. at each station
## Aulacoseira was found in 2012 and 2016 blooms
Aul <- ggplot(data = phyto.gen.BV, aes(x = DateTime, y = BV.um3.per.L, color = Genus)) +
  geom_point(data = subset(phyto.gen.BV, Genus == "Aulacoseira" & Year == 2016)) +
  geom_point(data = subset(phyto.gen.BV, Genus == "Eucapsis" & Year == 2016)) +
  annotate("rect", fill = "gray", alpha = 0.5,
           xmin = as.POSIXct("2016-07-12 17:00:00"), xmax = as.POSIXct("2016-08-01 17:00:00"),
           ymin = -Inf, ymax = Inf)

Aul + 
  labs (x = "Date", y = "Biovolume (um^3 per mL)", title = "Biovolume of Aulacoseira diatoms - 2016") +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(StationCode ~ ., ncol = 5) 

ggsave(path = output,
       filename = "aulacoseira_biovolume_by_station_2016.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=7.5, 
       dpi="print")

#######################################
####           NMDS Plots          ####
#######################################

## Create biovolume NMDS plots for each year by Region
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

## Create biovolume NMDS plots for each year by Region

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


## Create stacked bar chart for yearly LCEFA abundance by Action Phase
## and Upstream/Downstream
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

### Summarize data to get raw numbers for paper
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

