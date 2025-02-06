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
                                "zoop_NDFA_v2.1.csv"), 
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

# Remove Sherwood and Rominger Bridge stations
df_zoop <- df_zoop %>% 
  filter(StationCode != "SHR") %>% 
  filter(StationCode!="RMB")

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

df_region$Region <- factor(df_region$Region, levels = c("Upstream", "Downstream"))

df_zoop <- left_join(df_zoop, df_region, by = "StationCode")



# Reorder columns for clarity
df_zoop <- df_zoop %>%
  relocate(SamplePeriod, .after = Date) %>%
  relocate(Region, .after = StationCode) %>% 
  relocate(WYType, .after = Year) %>% 
  relocate(FlowPulseType, .after = WYType) %>% 
  relocate(NetFlowDays, .after = FlowPulseType) %>% 
  relocate(Taxlife, .after = TaxonName)

# Change calanoid copepodids to order in TaxonName because of interannual inconsistency in these ID's

df_zoop$LifeStage <- replace_na(df_zoop$LifeStage,"NA") 
df_zoop$TaxonName <- 
  ifelse(
    df_zoop$Order == "Calanoida" & df_zoop$LifeStage == "NA", "Calanoida", df_zoop$TaxonName )

df_zoop$TaxonName <- 
    ifelse(
    df_zoop$Order == "Calanoida" & df_zoop$LifeStage == "copepodid", "Calanoida", df_zoop$TaxonName )
    

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
  group_by(Year, Month, Date, Region,SamplePeriod, WYType, FlowPulseType, StationCode, TaxonName) %>%
  summarize(CPUEZoop = sum(CPUEZoop)) %>%
  ungroup

# Add zeros to genus data frame
temp <- df_zoop_gen %>% select(Year:StationCode,TaxonName:CPUEZoop)

temp <- pivot_wider(temp, 
                    names_from = "TaxonName", 
                    values_from = "CPUEZoop",
                    values_fill = 0)

df_zoop_gen <- pivot_longer(temp,
                            cols = `Calanoida`:last_col(),
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
    comm = genw[c(9:61)],
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
# Create a ggplot2 theme for the NMDS plots
theme_update(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.border = element_rect(fill = NA, colour = "black"),
  strip.background = element_rect(fill = "gray", colour = "black"),
  legend.position = "bottom",
  legend.key = element_rect(fill = "white", colour = NA)
)
# Create NMDS plots for each year by Region
p_zoop_NMDS_Region <- ggplot(df_zoop_gen_NMDS, aes(x = NMDS1, 
                                                   y = NMDS2, 
                                                   fill = Region)) +
  geom_point(size = 3,
             pch = 21,
             color = "black") +
  stat_ellipse(aes(color = Region)) + 
  labs(color = "Region",
       x = NULL,
       y = NULL)

p_zoop_NMDS_Region +
  facet_wrap(Year ~ ., ncol = 3, dir = "h") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")


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
                                                   fill = SamplePeriod)) +
  geom_point(size = 3,
             pch = 21,
             color = "black") +
  stat_ellipse(aes(color = SamplePeriod)) + 
  labs(color = "Flow Pulse Period",
       fill = "Flow Pulse Period",
       x = NULL,
       y = NULL)

p_zoop_NMDS_SamplePeriod +
  facet_wrap(Year ~ ., ncol = 3, dir = "h") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

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
       title = NULL,
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

<<<<<<< HEAD
adon.results <- adonis2(genw2[c(9:61)] ~ genw2$Region*genw2$SamplePeriod + genw2$Year*genw2$SamplePeriod+genw2$Region*genw2$Year, 
                         strata = genw2$StationCode,
                         method = "bray",
                         perm = 999)
write.csv(adon.results, file = "adon.results.csv")

dm_zoop_gen <- vegdist(genw2[c(9:61)], method = "bray")
=======
adon.results <- adonis2(genw2[c(9:70)] ~ genw2$Region*genw2$SamplePeriod + genw2$Year*genw2$SamplePeriod+genw2$Region*genw2$Year, 
                         strata = genw2$StationCode,
                         method = "bray",
                         perm = 999)
#write.csv(adon.results, file = "adon.results.csv")

dm_zoop_gen <- vegdist(genw2[c(9:70)], method = "bray")
>>>>>>> ee35623b151d8b4475bea871837d7940bf579ff9

bd <- betadisper(dm_zoop_gen, genw2$Region)

anova(bd)
permutest(bd)
#this resulted in significant p values for beta diversity below, so the assumptions of homogeneity of variances for permanova are not met

#try log transformation and re-run
df_zoop_gen3 <- df_zoop_gen

df_zoop_gen3$CPUEZoop <- log10(df_zoop_gen3$CPUEZoop+1)

genw3 <- pivot_wider(df_zoop_gen3, 
                     names_from = "TaxonName", 
                     values_from = "CPUEZoop",
                     values_fill = 0)

<<<<<<< HEAD
adon.results3 <- adonis2(genw3[c(9:61)] ~ genw3$Region*genw3$SamplePeriod + genw3$Year*genw3$SamplePeriod+genw3$Region*genw3$Year, 
=======
adon.results3 <- adonis2(genw3[c(9:70)] ~ genw3$Region*genw3$SamplePeriod + genw3$Year*genw3$SamplePeriod+genw3$Region*genw3$Year, 
>>>>>>> ee35623b151d8b4475bea871837d7940bf579ff9
                        strata = genw3$StationCode,
                        method = "bray",
                        perm = 999)
#write.csv(adon.results, file = "adon.results.csv")

<<<<<<< HEAD
dm_zoop_gen3 <- vegdist(genw3[c(9:61)], method = "bray")
=======
dm_zoop_gen3 <- vegdist(genw3[c(9:70)], method = "bray")
>>>>>>> ee35623b151d8b4475bea871837d7940bf579ff9

bd3 <- betadisper(dm_zoop_gen3, genw3$Region)

anova(bd3)
permutest(bd3)
#still significant p values for beta diversity with the log-transformed data 

# Calculate ANOSIM comparisons for zoop communities by Region
anosim_r <- data.frame(Year = years, 
                       R_value = NA,
                       p_value = NA)

for (year in years) {
  
  df_temp <- genw2 %>% filter(Year==year)
  
  test <- anosim(df_temp[c(9:61)], 
                 df_temp$Region, 
                 permutations = 10000, 
                 distance = "bray")
  
  anosim_r$R_value[which(anosim_r$Year == year)] <- test$statistic
  
  anosim_r$p_value[which(anosim_r$Year == year)] <- test$signif
  
}
anosim_r

# Calculate ANOSIM comparisons for zoop communities by Sample Period
anosim_S <- data.frame(Year = years, 
                       R_value = NA,
                       p_value = NA)

for (year in years) {
  
  df_tempS <- genw2 %>% filter(Year==year)
  
  testS <- anosim(df_tempS[c(9:61)], 
                 df_tempS$SamplePeriod, 
                 permutations = 10000, 
                 distance = "bray")
  
  anosim_S$R_value[which(anosim_S$Year == year)] <- testS$statistic
  
  anosim_S$p_value[which(anosim_S$Year == year)] <- testS$signif
  
}
anosim_S

####recreating Ted's plots####
#write.csv(df_zoop, file = "df_zoop.csv")
datfig <- df_zoop %>% group_by(Year,Order,Region,SamplePeriod) %>% 
  summarise(avg=mean(CPUEZoop))
#write.csv(datfig, file = "C:/Users/jadams/Documents/DES docs and forms/NDFA/Manuscript/ND-FASTR/Zoop_code/Jesse-zoop-analyses/datfig.csv")

#table for fig4
df_zoop_sampavg <- df_zoop %>% group_by(Date,StationCode,Year,SamplePeriod,Region,Order) %>% 
  summarise(tot=sum(CPUEZoop))

#some aggregating to look at total zoop by order
df_zoop_ordertot <- df_zoop %>% group_by(Date,StationCode,Year,Region,Order,SamplePeriod) %>%
  summarise(tot=sum(CPUEZoop))

df_zoop_ordertot2 <- df_zoop_ordertot %>% group_by(Region,Year,SamplePeriod,Order) %>%
  summarise(avg=mean(tot))

df_zoop_regmean <- df_zoop_ordertot %>% group_by(Region,Order) %>% 
  summarise(avg=mean(tot))

datfig <- df_zoop %>% group_by(Year,Order,Region,SamplePeriod) %>% 
  summarise(avg=mean(CPUEZoop))
write.csv(datfig, file = "C:/Users/jadams/Documents/DES docs and forms/NDFA/Manuscript/ND-FASTR/Zoop_code/Jesse-zoop-analyses/datfig.csv")

df_zoop_sampavg <- df_zoop %>% group_by(Date,StationCode,Year,SamplePeriod,Region,Order) %>% 
  summarise(avg=mean(CPUEZoop))
# Upstream/Downstream
fig4 <- ggplot(df_zoop_sampavg, aes(x = SamplePeriod, 
<<<<<<< HEAD
                                 y = tot, 
=======
                                 y = avg, 
>>>>>>> ee35623b151d8b4475bea871837d7940bf579ff9
                                 fill = Order)) + 
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0))+
  theme(axis.text.y = element_text())

fig4 + 
  labs(x = NULL, 
       y = bquote('Average Zooplankton CPUE ' ~ ('organisms *'~m^-3)), 
       title = paste0(NULL)) + 
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
       filename = paste0("fig4new_zoop_order_by_year.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")

#P. forbesi plot
testdata <- df_zoop_gen %>% filter(TaxonName=="Pseudodiaptomus forbesi")
testdata$logCPUEzoop <- log(testdata$CPUEZoop+1)
testplot <- ggplot(testdata, aes(x = SamplePeriod, y = logCPUEzoop, fill = Region))+
  geom_boxplot()
testplot+
  labs(x = bquote(NULL), 
       y = expression(paste("Log CPUE of " ,italic(" P. forbesi "), ~ ('organisms *'~m^-3))), 
       title = paste0(NULL)) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8")) +
                      theme(axis.text.x = element_text(angle = 0, vjust = 0.7)+
                              theme(axis.text.y = element_text()))

ggsave(path = output,
       filename = paste0("Pforbesi_regfpp.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")
testplot+
  facet_wrap(vars(Year), ncol = 3, dir = "h") +
  labs(x = bquote(NULL), 
       y = expression(paste("Log CPUE of " ,italic(" P. forbesi "), ~ ('organisms *'~m^-3))), 
       title = paste0(NULL)) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7))

ggsave(path = output,
       filename = paste0("Pforbesi_regfpp_facet.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7.5, 
       dpi="print")
#trying geom_jitter (i don't think it's better)
testplot2 <- ggplot(testdata, aes(x = SamplePeriod, y = logCPUEzoop, fill = Region))+
  geom_jitter(aes(colour=Region))
testplot2+
  labs(x = bquote('Flow Pulse Period'), 
       y = bquote('log CPUE of P. forbesi ' ~ ('organisms *'~L^-1)), 
       title = paste0("Abundance of P. forbesi by Region and Flow Pulse Period")) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("#E41A1C",
                                "#377EB8")) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.7))
testplot2+
  facet_wrap(vars(Year), ncol = 3, dir = "h") +
  labs(x = bquote('Flow Pulse Period'), 
       y = bquote('log CPUE of P. forbesi ' ~ ('organisms *'~L^-1)), 
       title = paste0("Abundance of P. forbesi by Region and Flow Pulse Period")) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("#E41A1C",
                               "#377EB8")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7))

#All calanoids plot
# Summarize by taxon for NMDS plots
df_zoop_f <- df_zoop %>%
  group_by(Year, Month, Date, Region,SamplePeriod, WYType, FlowPulseType, StationCode, Family, Genus, TaxonName, Taxlife) %>%
  summarize(across(CPUEZoop:BPUE, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

# Add zeros to genus data frame
temp_f <- df_zoop_f %>% select(Year:StationCode,Taxlife:CPUEZoop)

temp_f <- pivot_wider(temp_f, 
                    names_from = "Taxlife", 
                    values_from = "CPUEZoop",
                    values_fill = 0)

df_zoop_f <- pivot_longer(temp_f,
                            cols = `Sinocalanus doerrii adult`:last_col(),
                            names_to = "Taxlife",
                            values_to = "CPUEZoop")

#P. forbesi plot
testdata_f <- df_zoop_f %>% filter(Taxlife=="Pseudodiaptomus forbesi copepodid")
testdata_f$logCPUEzoop <- log(testdata_f$CPUEZoop+1)
testplot_f <- ggplot(testdata_f, aes(x = SamplePeriod, y = logCPUEzoop, fill = Region))+
  geom_boxplot()
testplot_f+
  labs(x = bquote(NULL), 
       y = expression(paste("Log CPUE of " ,italic(" P. forbesi "), ~ ('organisms *'~m^-3))), 
       title = paste0(NULL)) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7)+
          theme(axis.text.y = element_text()))


testplot_f+
  facet_wrap(vars(Year), ncol = 3, dir = "h") +
  labs(x = bquote(NULL), 
       y = expression(paste("Log CPUE of " ,italic(" P. forbesi copepodid "), ~ ('organisms *'~m^-3))), 
       title = paste0(NULL)) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7))

#repeat above but for calanoid copepodid


#P. forbesi plot
testdata_c <- df_zoop_f %>% filter(Taxlife=="Calanoida copepodid")
testdata_c$logCPUEzoop <- log(testdata_c$CPUEZoop+1)
testplot_c <- ggplot(testdata_c, aes(x = SamplePeriod, y = logCPUEzoop, fill = Region))+
  geom_boxplot()
testplot_c+
  labs(x = bquote(NULL), 
       y = expression(paste("Log CPUE of " ,italic(" Calanoid copepodid "), ~ ('organisms *'~m^-3))), 
       title = paste0(NULL)) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7)+
          theme(axis.text.y = element_text()))


testplot_c+
  facet_wrap(vars(Year), ncol = 3, dir = "h") +
  labs(x = bquote(NULL), 
       y = expression(paste("Log CPUE of " ,italic(" Calanoida copepodid "), ~ ('organisms *'~m^-3))), 
       title = paste0(NULL)) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7))

# Summarize by taxon
df_zoop_ord <- df_zoop %>%
  group_by(Year, Month, Date, Region,SamplePeriod, WYType, FlowPulseType, StationCode, Order) %>%
  summarize(across(CPUEZoop:BPUE, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

# Add zeros to order data frame
temp_ord <- df_zoop_ord %>% select(Year:StationCode,Order:CPUEZoop)

temp_ord <- pivot_wider(temp_ord, 
                    names_from = "Order", 
                    values_from = "CPUEZoop",
                    values_fill = 0)

df_zoop_ord <- pivot_longer(temp_ord,
                            cols = `Calanoida`:last_col(),
                            names_to = "Order",
                            values_to = "CPUEZoop")


testdata_ord <- df_zoop_ord %>% filter(Order=="Calanoida")
testdata_ord$logCPUEzoop <- log(testdata_ord$CPUEZoop+1)
testplot_ord <- ggplot(testdata_ord, aes(x = SamplePeriod, y = logCPUEzoop, fill = Region))+
  geom_boxplot()
testplot_ord+
  labs(x = bquote(NULL), 
       y = expression(paste("Log CPUE of " ,italic(" Calanoida"), ~ ('organisms *'~m^-3))), 
       title = paste0(NULL)) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7))

testplot_ord+
  facet_wrap(vars(Year), ncol = 3, dir = "h") +
  labs(x = bquote(NULL), 
       y = expression(paste("Log CPUE of " ,italic(" Calanoida "), ~ ('organisms *'~m^-3))), 
       title = paste0(NULL)) + 
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("#E41A1C",
                               "#377EB8")) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.7))
#end()