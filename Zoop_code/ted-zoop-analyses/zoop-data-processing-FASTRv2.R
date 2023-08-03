# Processing FASTR zooplankton data from Jesse ---------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(vegan)
library(RColorBrewer)
library(here)

# Set visual theme in ggplot ---------------------------------------------------
theme_set(theme_bw())

# Set output location
output = here("Zoop_code","ted-zoop-analyses","plots")

# Import processed data --------------------------------------------------------
df_zoop <- read_csv(file = here("Zoop_code",
                                "ted-zoop-analyses",
                                "zoop_NDFA_v2.csv"), 
                    show_col_types = FALSE)

# Remove unnecessary phylogenetic groups ---------------------------------------
# Some of these throw errors when imported and we won't need them --------------
df_zoop <- df_zoop %>% 
  select(!(c("Superorder","Suborder","Infraorder","Superfamily",
             "Subclass","Infraclass","Subphylum")))

# No problems shown 
problems(df_zoop)

# Remove empty rows
df_zoop <- df_zoop %>% filter_all(any_vars(!is.na(.))) # Zero empty rows

# Clean up imported data -------------------------------------------------------

# DateTime and Time columns are incomplete, so we'll remove them because
# we're not really doing any time-based analyses anyway
df_zoop <- df_zoop %>% select(!c("DateTime","Time"))

# Covert Date column to date format using lubridate
df_zoop$Date <- as_date(df_zoop$Date)

sum(is.na(df_zoop$Date)) # No entries don't have a date

## Order month in calendar order rather than (default) alphabetical
df_zoop <- df_zoop %>% mutate(Month = month(Date, abbr = TRUE, label = TRUE))

# Remove samples with incorrect CPUE due to bad flowmeter values
df_zoop <- df_zoop %>%  filter(Date!="2016-07-07" | StationCode!="RVB")
df_zoop <- df_zoop %>% filter(Date!="2016-01-06" | StationCode!="STTD")

# Remove flowmeter data 
df_zoop <- df_zoop %>% select(!c(VolMeso:OrganismID))

# Remove other extraneous columns 
df_zoop <- df_zoop %>% select(!c("FlowMeterSpeed","StartMeter","EndMeter"))
df_zoop <- df_zoop %>% select(!c("...1","Count","CarbonWeight"))

# Remove Microzooplankton, Macrozooplankton, Harpacticoid copepods (all not targeted by gear)

df_zoop <- df_zoop %>% 
  filter(Classification != "Microzooplankton_Nauplii")
df_zoop <- df_zoop %>% 
  filter(Classification != "Macrozooplankton")
df_zoop <- df_zoop %>% 
  filter(Classification != "Harpacticoids")

# List of stations used for FASTR
stations <- c("RMB","RCS","RD22","I80","LIS","STTD","BL5","LIB","RYI","RVB")

# Check station IDs on imported data
table(df_zoop$StationCode) # 11 stations

# Remove Sherwood stations
df_zoop <- df_zoop %>% 
  filter(StationCode != "SHR")

# Confirm station IDs
unique(df_zoop$StationCode)

# Re-order stations from North to South
df_zoop$StationCode <- factor(as.character(df_zoop$StationCode), levels = stations)

# Import Stations listed as Upstream and Downstream
df_region <- read_csv(here("Zoop_code",
                        "ted-zoop-analyses",
                        "CSVs",
                        "upstream_downstream_stations.csv"),
                   show_col_types = FALSE)
                   
df_region <- df_region %>% 
  rename("Region" = "UpDown") %>%
  rename("StationCode" = "Site")

df_region$Region <- factor(df_region$Region, levels = c("Upstream","Downstream"))

df_zoop <- left_join(df_zoop, df_region, by = "StationCode")

rm(df_region)

# Reorder columns for clarity
df_zoop <- df_zoop %>%
  relocate(SamplePeriod, .after = Date) %>%
  relocate(Region, .after = StationCode) %>% 
  relocate(WYType, .after = Year) %>% 
  relocate(FlowPulseType, .after = WYType) %>% 
  relocate(NetFlowDays, .after = FlowPulseType) %>% 
  relocate(Taxlife, .after = TaxonName)

# Order the new ActionPhase so that it plots in the order Pre < During < Post
phase.order <- c("Before","During","After")
df_zoop$SamplePeriod <- factor(as.character(df_zoop$SamplePeriod), 
                                levels = phase.order)
  
# Analyze CPUE Data for Outliers -----------------------------------------------

# Identify outliers to remove
histogram(log10(df_zoop$CPUEZoop))

# Identify 0.1% and 99.9% percentiles of the data
quartiles <- quantile(df_zoop$CPUEZoop, probs=c(0.01, 0.99), na.rm = TRUE)

# List upper cutoff (99%)
cutoff <- quartiles[2]

# Filter dataset to show all taxa above this 99.9% cutoff
df_outliers <- df_zoop %>% filter(CPUEZoop > cutoff)

list(df_zoop) # No major outliers to remove

# Summarize data by year
table(df_zoop$Year) # Data is sparse prior to 2014

# Remove samples prior to 2014
df_zoop <- df_zoop %>% filter(Year >= 2014) 

# Summarize data ---------------------------------------------------------------

# Examine all samples for high-level diversity
table(df_zoop$Phylum)
table(df_zoop$Class) # 4 (Branchiopoda, Hexanauplia, Insecta, Ostracoda)
table(df_zoop$Order) # 3 (Calanoida, Cyclopoida, Diplostraca)

# Examine lower-level diversity 

# Family Level
length(unique(df_zoop$Family))# 17 families

sort(table(df_zoop$Family), decreasing = TRUE)
# 3 low-abundance families with 10 or fewer detections
# Acartiidae, Temoridae, Ergasilidae

# Genus Level
length(unique(df_zoop$Genus)) # 45 Genera

sort(table(df_zoop$Genus), decreasing = TRUE)

# Look at which taxa do not have a genus-level classification
df_no_genus <- df_zoop %>% filter(is.na(Genus))

sort(table(df_no_genus$TaxonName), decreasing = TRUE)
sort(table(df_no_genus$Taxlife), decreasing = TRUE)

# Taxon level

table(is.na(df_zoop$TaxonName)) # no missing taxon identifiers

sort(table(df_zoop$TaxonName), decreasing = TRUE)

# Summarize by taxon for NMDS plots
df_zoop_gen <- df_zoop %>%
  group_by(Year, Month, Date, Region,SamplePeriod, WYType, FlowPulseType, StationCode, Family, Genus, TaxonName) %>%
  summarize(across(CPUEZoop:BPUE, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

# Add zeros to genus data frame
temp <- df_zoop_gen %>% select(Year:StationCode,TaxonName:CPUEZoop)

temp <- pivot_wider(temp, 
                    names_from = "TaxonName", 
                    values_from = "CPUEZoop",
                    values_fill = 0)

df_zoop_gen <- pivot_longer(temp,
                            cols = `Sinocalanus doerrii`:last_col(),
                            names_to = "TaxonName",
                            values_to = "CPUEZoop")


# Calculate relative abundance of CPUE by taxon
df_zoop_RA <- df_zoop_gen %>%
  group_by(Year, Month, Date, Region, WYType, FlowPulseType, StationCode) %>%
  mutate(MeanRelAbund = CPUEZoop/sum(CPUEZoop)) %>%
  ungroup

# Highlight most abundant genera
df_zoop_RA <- df_zoop_RA %>%
  mutate(Type = case_when(MeanRelAbund > 0.05 ~ TaxonName,
                          TRUE ~ 'Other'))

length(unique(df_zoop_RA$Type)) # 32 remaining taxa not lumped together

# lump together all "other" taxa
df_zoop_RA_tot <- df_zoop_RA %>%
  group_by(Year, Month, Date, Region, WYType, FlowPulseType, StationCode, Type) %>%
  summarize(MeanRelAbund = sum(MeanRelAbund)) %>%
  ungroup()

## Create cross-walk table for what taxa are lumped into the Other category
df_zoop_types <- df_zoop_RA_tot %>% select(Type)

# Calculate NMDS axes ----------------------------------------------------------

## Generate NMDS data with metaMDS by each year separately

years <- unique(df_zoop_gen$Year) 
years <- sort(years, decreasing = F, na.last = T)

## Create blank data frame to fill stresses in
stresses <- data.frame(Year = years, stress = NA)
ls_dfs <- list()

for (i in 1:length(years)) {
  genw <- pivot_wider(df_zoop_gen, 
                      names_from = "TaxonName", 
                      values_from = "CPUEZoop",
                      values_fill = 0)
  
  genw <- genw %>% filter(Year == years[i])
  
  #look at number of observations per station
  #table(genw$StationCode)
  
  # Calculate the nMDS using vegan 
  # A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
  # < 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.
  zoop_NMDS <- metaMDS(
    comm = genw[c(9:70)],
    distance = "bray",
    k = 3,
    trymax = 150
    #trace = F,
    #autotransform = F
  )
  
  stresses$stress[which(stresses$Year == years[i])] <- zoop_NMDS$stress
  
  #look at Shepard plot which shows scatter around the regression between the interpoint distances 
  #in the final configuration (i.e., the distances between each pair of communities) against their 
  #original dissimilarities.
  stressplot(zoop_NMDS)
  
  # Using the scores function from vegan to extract the site scores and convert to a data.frame
  df_data_scores <- as_tibble(scores(zoop_NMDS, display = "sites"))
  
  # Combine metadata with NMDS data scores to plot in ggplot
  meta <- genw %>% select(Year:StationCode)
  meta <- cbind(meta, df_data_scores)
  
  # Read in years as a character otherwise it shows up as a number and gets displayed as a gradient
  meta$Year <- as.character(meta$Year)
  
  ls_dfs[[i]] <- meta
  
}

df_zoop_gen_NMDS <- do.call(rbind, ls_dfs)

# Create NMDS Plots ------------------------------------------------------------

# Create NMDS plots for each year by Region
p_zoop_NMDS_Region <- ggplot(df_zoop_gen_NMDS, aes(x = NMDS1, 
                                                   y = NMDS2, 
                                                   color = Region)) +
  geom_point(size = 3) +
  stat_ellipse() + 
  labs(title = "Zooplankton Community Composition") +
  labs(color = "Region") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")

p_zoop_NMDS_Region +
  facet_wrap(Year ~ ., ncol = 3, dir = "h") +
  labs(x = NULL,
       y = NULL,
       title = "NDFA - Zooplankton Community Comparison (2014-2019)",
       color = "Region") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave(path = output,
       filename = "p_zoop_NMDS_region.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=6.5, 
       dpi="print")

# Create NMDS plots for each year by Sample Period
p_zoop_NMDS_SamplePeriod <- ggplot(df_zoop_gen_NMDS, aes(x = NMDS1, 
                                                   y = NMDS2, 
                                                   color = SamplePeriod)) +
  geom_point(size = 3) +
  stat_ellipse() + 
  labs(title = "Zooplankton Community Composition") +
  labs(color = "Sample Period") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")

p_zoop_NMDS_SamplePeriod +
  facet_wrap(Year~., ncol = 3, dir = "h") +
  labs(x = NULL,
       y = NULL,
       title = "NDFA - Zooplankton Community Comparison (2014-2019)",
       color = "Sample Period") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave(path = output,
       filename = "p_zoop_NMDS_sampleperiod.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=6.5, 
       dpi="print")

p_zoop_NMDS_SamplePeriod +
  facet_wrap(vars(Year, Region), ncol = 4, dir = "h") +
  labs(x = NULL,
       y = NULL,
       title = "NDFA - Zooplankton Community Comparison (2014-2019)",
       color = "Sample Period") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

ggsave(path = output,
       filename = "p_zoop_NMDS_sampleperiod_Region.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=6.5, 
       dpi="print")

# Calculate PERMANOVA comparisons for zoop communities by Region and Year with sample period as an interaction


genw2 <- pivot_wider(df_zoop_gen, 
                     names_from = "TaxonName", 
                     values_from = "CPUEZoop",
                     values_fill = 0)


adon.results <- adonis2(genw2[c(9:70)] ~ genw2$Region*genw2$SamplePeriod + genw2$Year*genw2$SamplePeriod+genw2$Region*genw2$Year, 
                        strata = genw2$StationCode,
                        method = "bray",
                        perm = 999)
#write.csv(adon.results, file = "adon.results.csv")

dm_zoop_gen <- vegdist(genw2[c(9:70)], method = "bray")

bd <- betadisper(dm_zoop_gen, genw2$Region)

anova(bd)
permutest(bd)

# Calculate ANOSIM comparisons for zoop communities by Region
anosim_r <- data.frame(Year = years, 
                       R_value = NA,
                       p_value = NA)

for (year in years) {
  
  df_temp <- genw2 %>% filter(Year==year)
  
  test <- anosim(df_temp[c(9:70)], 
                 df_temp$Region, 
                 permutations = 10000, 
                 distance = "bray")
  
  anosim_r$R_value[which(anosim_r$Year == year)] <- test$statistic
  
  anosim_r$p_value[which(anosim_r$Year == year)] <- test$signif
  
}

# Calculate ANOSIM comparisons for zoop communities by Sample Period
anosim_S <- data.frame(Year = years, 
                       R_value = NA,
                       p_value = NA)

for (year in years) {
  
  df_tempS <- genw2 %>% filter(Year==year)
  
  testS <- anosim(df_tempS[c(9:70)], 
                 df_tempS$SamplePeriod, 
                 permutations = 10000, 
                 distance = "bray")
  
  anosim_S$R_value[which(anosim_S$Year == year)] <- testS$statistic
  
  anosim_S$p_value[which(anosim_S$Year == year)] <- testS$signif
  
}


####recreating Ted's plots####

# Upstream/Downstream
fig4 <- ggplot(df_zoop, aes(x = SamplePeriod, 
                                 y = CPUEZoop, 
                                 fill = Order)) + 
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0)) 

fig4 + 
  labs(x = NULL, 
       y = bquote('Average Zooplankton CPUE ' ~ ('organisms *'~L^-1)), 
       title = paste0("Abundance of Zooplankton Orders During Flow Pulses")) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8",
                               "#4DAF4A",
                               "#984EA3",
                               "#FF7F00",
                               "#FFFF33",
                               "#A65628",
                               "#F781BF"),
                    labels=c("Calanoida", "Cyclopoida", "Cladocera")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  facet_grid(Region~Year) # dir = v makes order of station go "north to south"

ggsave(path = output,
       filename = paste0("fig4_zoop_order_by_year.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")


#end