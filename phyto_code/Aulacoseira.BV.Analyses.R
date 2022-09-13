## Script just to look at Aulacoseira

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")
library("vegan");packageVersion("vegan")

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Set folder name to output graphs into
output <- "plots"

# Import combined EMP & AEU data files 
load("RData/df_phyto.RData")

# Filter out non-Aulacoseira taxa and years outside of FASTR study
df_Aul <- df_phyto %>% 
  filter(Genus == "Aulacoseira") %>%
  filter(Year %in% c(2014,2015,2016,2017,2018,2019))

ggplot(df_Aul, aes(x = Year, y = GALD)) +
  geom_jitter(width = 0.1)

ggplot(df_Aul, aes(x = Year, y = BV.Avg)) +
  geom_jitter(width = 0.1)

## Plot EMP and FASTR samples by year, facet by study
ggplot(df_Aul, aes(x = Year, y = BV.um3.per.L)) +
  geom_jitter(width = 0.1,
              size = 2,
              pch = 21, 
              color = "black",
              fill = "darkgreen") +
  labs(x = "Year",
       y = "Biovolume (um^3 per L)") +
  facet_wrap(Study ~ ., ncol = 1)

ggsave(path = output,
       filename = "Aulacoseira_abundance_FASTR_EMP_yearly_all.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=4, 
       dpi="print")

## Plot EMP and FASTR samples by month, facet by study
ggplot(df_Aul, aes(x = Month, y = BV.um3.per.L)) +
  geom_jitter(data = subset(df_Aul, Year == "2016"),
              width = 0.1,
              size = 2,
              pch = 21, 
              color = "black",
              fill = "darkgreen") +
  labs(x = "Year",
       y = "Biovolume (um^3 per L)") +
  facet_wrap(Study ~ ., ncol = 1)

ggsave(path = output,
       filename = "Aulacoseira_abundance_FASTR_EMP_month_2016.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=4, 
       dpi="print")


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
