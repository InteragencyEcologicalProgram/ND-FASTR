# Analyze FASTR contaminants data- ANOVAs/comparison with EPA benchmarks
# Laura Twardochleb
# 2/12/21

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(lsmeans)
library(EnvStats)
library(viridisLite)
library(emmeans)
library(car)
library(visreg)
library(cowplot)
library(here)

# Source functions
source(here("global_ndfa_funcs.R"))

# Define file path in the repository for figure and table outputs
fp_output <- here("Water_Quality/Output_Report")

# Define file path in the repository for contaminants analyses directory
fp_contam <- here("Water_Quality/Contaminants/Analyses")

# Import data
load(here("Water_Quality/Data_Processed/contam_proc_data.RData"))
epa_benchmarks <- read_csv(file.path(fp_contam, "Aquatic_life_benchmarks3.csv"), na = c("NR"))

# Prepare and combine data
contam_all <- 
  list(
    water = contam_water,
    sediment = contam_ss,
    zooplankton = contam_zoop
  ) %>% 
  bind_rows(.id = "Response_type") %>% 
  # Add Date, Year, Flow Action Periods, and Regions
  mutate(
    Date = date(DateTime),
    Year = year(DateTime)
  ) %>% 
  ndfa_action_periods() %>% 
  mutate(
    Year_fac = factor(Year),
    Region = case_when(
      StationCode %in% c("RCS", "RD22", "I80", "LIS", "STTD", "RMB") ~ "Upstream",
      StationCode %in% c("BL5", "RYI") ~ "Downstream",
      TRUE ~ NA_character_
    ),
    Region = factor(Region, levels = c("Upstream", "Downstream")),
    FlowActionPeriod = factor(FlowActionPeriod, levels = c("Before", "During", "After"))
  )

# Explore the data- summarize number of samples (by region, station, year, and flow pulse period)
number_stations <- contam_all %>% 
  filter(Result != "< MDL") %>%
  group_by(Response_type, Year_fac, Region, FlowActionPeriod) %>%
  summarize(n = length(unique(StationCode))) %>% 
  ungroup() %>% 
  arrange(Response_type, Year_fac, Region, FlowActionPeriod)

# not enough replication to analyze by region for water, zooplankton - can
# analyze water by year and flowperiod and zooplankton by year or flowperiod
by_year <- contam_all %>%
  filter(Result != "< MDL") %>%
  group_by(Response_type, Year_fac, FlowActionPeriod) %>%
  summarize(n = length(unique(StationCode))) %>% 
  ungroup()

# only 'after' period for 2015 for water and no 'during' period for 2017 for
# zooplankton - need to drop from analysis, also drop SHR
water_total <- contam_all %>%
  filter(
    Response_type == "water",
    Year_fac != "2015",
    Result != "< MDL",
    StationCode != "SHR"
  ) %>%
  group_by(Year_fac, FlowActionPeriod, Date, StationCode) %>%
  summarize(totals = sum(as.numeric(Result))) %>% 
  ungroup()

zoop_total <- contam_all %>%
  filter(
    Response_type == "zooplankton",
    Result != "< MDL",
    StationCode != "SHR"
  ) %>%
  group_by(Year_fac, FlowActionPeriod, Date, StationCode) %>%
  summarize(totals = sum(as.numeric(Result))) %>% 
  ungroup()

# examine data distribution
hist(water_total$totals) #try log-transform of the data to achieve normality
hist(log(water_total$totals)) #looks better

hist(zoop_total$totals)
hist(log(zoop_total$totals)) #try this or could use negative binomial distribution
hist(sqrt(zoop_total$totals))

# Create custom theme for figures
theme_contam <- list(
  theme_bw(),
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(), 
    axis.text.y = element_text(size = 12), 
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15), 
    axis.text.x = element_text(size = 12),
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 15)
  )
)

# explore water data
# examine data patterns- it looks like during is always higher than after, not
# sure about interaction by year
year_water_total <- water_total %>% 
  ggplot(aes(x = Year_fac, y = log(totals))) +
  geom_boxplot(aes(fill = Year_fac), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in water (ng/L))")), 
    limits = c(5, 10), 
    expand = c(0, 0)
  ) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(x = "Year", fill = "Year", title = "A)") +
  theme_contam

# by flow period
flowperiod_water_total <- water_total %>% 
  ggplot(aes(x = FlowActionPeriod, y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in water (ng/L))")), 
    limits = c(5, 10), 
    expand = c(0, 0)
  ) +
  # scale_x_discrete(limits = c("Before", "During", "After")) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(fill = "Flow period", title = "B)") +
  xlab("Flow period") +
  annotate("text", x = "Before", y = 9.8, label = "a") +
  annotate("text", x = "During", y = 9.8, label = "b") +
  annotate("text", x = "After", y = 9.8, label = "c") +
  theme_contam

# year, flow period interaction
year_flowperiod_water_total <- water_total %>% 
  ggplot(aes(x = interaction(FlowActionPeriod, Year_fac), y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in water (ng/L))")), 
    limits = c(5, 10), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(labels = c("", "2016", "", "", "2017", "", "", "2018", "", "", "2019", "")) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(fill = "Flow period", title = "C)") +
  xlab("Interaction (Year x flow period)") +
  theme_contam

# create multipanel plot, add annotations to indicate which significantly
# different, change colors to viridis Use cowplot to align and stack plots. Save
# figures to project file.
contaminants_water_plot <- plot_grid(
  year_water_total, flowperiod_water_total, year_flowperiod_water_total, 
  ncol = 1, 
  align = "v"
)

ggsave(
  filename = file.path(fp_output, "contaminants_water_v2.png"), 
  plot = contaminants_water_plot, 
  height = 12, 
  width = 8, 
  dpi = 600
)

# two-way anova (Year*FlowPeriod) and unbalanced design
water.total.anova <- lm(log(totals) ~ Year_fac * FlowActionPeriod, data = water_total)
summary(water.total.anova)
type3.water <- Anova(water.total.anova, type = 3)

# run specifying the order of terms differently (since unbalanced)
water.total.anova2 <- lm(log(totals) ~ FlowActionPeriod + Year_fac, data = water_total)
summary(water.total.anova2) # flow period is still significant

# run type 2 anova (for unbalanced design, and because interaction term is not significant)
type2.water <- Anova(water.total.anova2, type = 2) # essentially same result as type 1 anova- use type 2
type2.water
plot(water.total.anova2)

# tukey test on type 2 anova output
emmeans1 <- emmeans(water.total.anova2, specs = pairwise ~ FlowActionPeriod, adjust = "sidak")
print(test(emmeans1)$contrasts)
# overall:  no significant differences in contaminants in water between years
# there is an effect of flow period, where during the pulse contaminants are
# significantly higher than before or after. Before is higher than after

# visreg package to visualize output of anova
visreg(water.total.anova2)

################## analyze zooplankton data #######################################
# by year- higher in 2018 and 2019
year_zoop <- zoop_total %>% 
  ggplot(aes(x = Year_fac, y = log(totals))) +
  geom_boxplot(aes(fill = Year_fac), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in zooplankton (ng/g))")), 
    limits = c(3, 8),
    expand = c(0, 0)
  ) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(x = "Year", fill = "Year", title = "A)") +
  annotate("text", x = "2017", y = 7.8, label = "a") +
  annotate("text", x = "2018", y = 7.8, label = "ab") +
  annotate("text", x = "2019", y = 7.8, label = "b") +
  theme_contam

# by flow period- higher during
flowperiod_zoop <- zoop_total %>% 
  ggplot(aes(x = FlowActionPeriod, y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in zooplankton (ng/g))")), 
    limits = c(3, 8), 
    expand = c(0, 0)
  ) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(fill = "Flow period", title = "B)") +
  xlab("Flow period") +
  annotate("text", x = "Before", y = 7.8, label = "a") +
  annotate("text", x = "During", y = 7.8, label = "b") +
  annotate("text", x = "After", y = 7.8, label = "ab") +
  theme_contam

# year, flow period interaction- looks like during is much higher in 2018 and
# 2019- like the flow pulse is pushing zoops to STTD downstream and they have
# higher contaminants concentrations
year_flowperiod_zoop <- zoop_total %>% 
  ggplot(aes(x = interaction(FlowActionPeriod, Year_fac), y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(pesticide concentration (ng/g))")), 
    limits = c(3, 8), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    limits = c(
      "Before.2017",
      "During.2017",
      "After.2017",
      "Before.2018",
      "During.2018",
      "After.2018",
      "Before.2019",
      "During.2019",
      "After.2019"
    ),
    labels = c("", "2017", "", "", "2018", "", "", "2019", "", "")
  ) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(fill = "Flow period", title = "C)") +
  xlab("Interaction (Year x flow period)") +
  theme_contam

# try figure above but with flow period on x-axis and faceted by year
year_flowperiod_zoop2 <- zoop_total %>% 
  ggplot(aes(x = FlowActionPeriod, y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in zooplankton (ng/g))")), 
    limits = c(3, 8), 
    expand = c(0, 0)
  ) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(fill = "Flow period", title = "C)") +
  xlab("Flow period") +
  facet_wrap(vars(Year_fac), nrow = 1) +
  theme_contam

contaminants_zoop_plot <- plot_grid(
  year_zoop, flowperiod_zoop, year_flowperiod_zoop2, 
  ncol = 1, 
  axis = "lr", 
  align = "v"
)

ggsave(
  filename = file.path(fp_output, "contaminants_zoop_v2.png"),
  plot = contaminants_zoop_plot,
  height = 12,
  width = 8,
  dpi = 600
)

# two-way anova (Year*FlowPeriod) and unbalanced design- not enough power to run this model
zoop.total.anova <- lm(log(totals) ~ Year_fac * FlowActionPeriod, data = zoop_total)
summary(zoop.total.anova)
type3.zoop <- Anova(zoop.total.anova, type = 3)

# run type 2 anova (for unbalanced design, because can't include an interaction term)
zoop.total.anova2 <- lm(log(totals) ~ Year_fac + FlowActionPeriod, data = zoop_total)
summary(zoop.total.anova2)
type2.zoop <- Anova(zoop.total.anova2, type = 2) # essentially same result as type 1 anova- use type 2
type2.zoop # year and flowperiod are significant

# tukey test on type 2 anova output
emmeans2 <- emmeans(zoop.total.anova2, specs = pairwise ~ Year_fac, adjust = "sidak")
print(test(emmeans2)$contrasts)
emmeans3 <- emmeans(zoop.total.anova2, specs = pairwise ~ FlowActionPeriod, adjust = "sidak")
print(test(emmeans3)$contrasts)
# 2019 higher than 2017, during is higher than before

# visreg package to visualize output of anova
visreg(zoop.total.anova2)

# run t-tests comparing SHR and STTD for water and zoops- may be able to run an anova?
water_total2 <- contam_all %>%
  filter(
    Response_type == "water",
    Year_fac != "2015",
    Result != "< MDL"
  ) %>%
  group_by(Year_fac, FlowActionPeriod, Date, StationCode) %>%
  summarize(totals = sum(as.numeric(Result))) %>% 
  ungroup()

zoop_total2 <- contam_all %>%
  filter(
    Response_type == "zooplankton",
    Result != "< MDL"
  ) %>%
  group_by(Year_fac, FlowActionPeriod, Date, StationCode) %>%
  summarize(totals = sum(as.numeric(Result))) %>% 
  ungroup()

# run welch's two-sample t-test for SHR and STTD for zoop samples
# convert data to wide format
zoop_total_wide <- pivot_wider(zoop_total2, names_from = StationCode, values_from = totals)

# SHR significantly higher than STTD overall
zoop.t.test <- t.test(log(zoop_total_wide$SHR), log(zoop_total_wide$STTD), na.action(na.omit))
zoop.t.test

################## explore variation across stations for water samples ###############
# are the stations that are closer to source higher in contaminants, and how does
# that vary by year and flow pulse type? representative station for upstream vs.
# downstream for water: RD22, BL5

# BL5 is lower overall than RD22, BL5 but not RD22 increases during the action
station_flowperiod <- water_total %>%
  filter(StationCode == "RD22" | StationCode == "BL5") %>%
  ggplot(aes(x = interaction(FlowActionPeriod, StationCode), y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log total contaminant concentration (ng/L)")), 
    limits = c(), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    limits = c(
      "Before.RD22",
      "During.RD22",
      "After.RD22",
      "Before.BL5",
      "During.BL5",
      "After.BL5"
    )
  )

# both sites increased during the flow pulse in 2018 and 2019, but contaminants
# were lower during the flow pulse in 2016 (but SHR is higher overall? May need
# another source sample for Sac River action years) it looks like there are main
# effects of year and flowperiod and a flowperiod by year interaction
station_year <- water_total %>%
  filter(StationCode == "RD22" | StationCode == "BL5") %>%
  ggplot(aes(x = interaction(Year_fac, StationCode), y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log total contaminant concentration (ng/L)")), 
    limits = c(), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    limits = c(
      "2016.RD22",
      "2016.BL5",
      "2017.RD22",
      "2017.BL5",
      "2018.RD22",
      "2018.BL5",
      "2019.RD22",
      "2019.BL5"
    )
  )

# ANOVA for contaminants in water by station, year, flowperiod (two-way
# interaction bw/ flow period and year)
by_station <- lm(log(totals) ~ StationCode + Year_fac * FlowActionPeriod, water_total[(water_total$StationCode == "BL5" | water_total$StationCode == "RD22"), ])
summary(by_station)
by_station_ANOVA <- Anova(by_station, type = 3)
by_station_ANOVA

# create multipanel plot, add annotations to indicate which significantly
# different, change colors to viridis station effect
site <- water_total %>%
  filter(StationCode == "RD22" | StationCode == "BL5") %>%
  ggplot(aes(x = StationCode, y = log(totals))) +
  geom_boxplot(aes(fill = StationCode), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log total contaminant concentration (ng/L)")), 
    limits = c(5, 9), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(limits = c("RD22", "BL5"), labels = c("RD22", "BL5")) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("RD22", "BL5")) +
  labs(fill = "Site", title = "A)") +
  xlab("Site") +
  annotate("text", x = "RD22", y = 8.8, label = "a") +
  annotate("text", x = "BL5", y = 8.8, label = "b") +
  theme_contam

# year effect
year <- water_total %>%
  filter(StationCode == "RD22" | StationCode == "BL5") %>%
  ggplot(aes(x = Year_fac, y = log(totals))) +
  geom_boxplot(aes(fill = Year_fac), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log total contaminant concentration (ng/L)")), 
    limits = c(5, 9), 
    expand = c(0, 0)
  ) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(fill = "Year", title = "B)") +
  xlab("Year") +
  theme_contam

# flowperiod effect
flowperiod <- water_total %>%
  filter(StationCode == "RD22" | StationCode == "BL5") %>%
  ggplot(aes(x = FlowActionPeriod, y = log(totals))) +
  geom_boxplot(aes(fill = Year_fac), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log total contaminant concentration (ng/L)")), 
    limits = c(5, 9), 
    expand = c(0, 0)
  ) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(fill = "Year", title = "C)") +
  xlab("Flow Period") +
  theme_contam

# year, flowperiod interaction
station_year_flow_interaction <- water_total %>%
  filter(StationCode == "RD22" | StationCode == "BL5") %>%
  ggplot(aes(x = interaction(Year_fac, FlowActionPeriod), y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log total contaminant concentration (ng/L)")), 
    limits = c(5, 9), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    limits = c(
      "2016.Before",
      "2016.During",
      "2016.After",
      "2017.Before",
      "2017.During",
      "2017.After",
      "2018.Before",
      "2018.During",
      "2018.After",
      "2019.Before",
      "2019.During",
      "2019.After"
    ),
    labels = c("", "2016", "", "", "2017", "", "", "2018", "", "", "2019", "", "")
  ) +
  scale_colour_viridis_d(aesthetics = "fill") +
  labs(fill = "Flow period", title = "B)") +
  xlab("Interaction (Year x flow period)") +
  annotate("text", x = "2016.Before", y = 8.8, label = "ab") +
  annotate("text", x = "2016.During", y = 8.8, label = "a") +
  annotate("text", x = "2016.After", y = 8.8, label = "a") +
  annotate("text", x = "2017.Before", y = 8.8, label = "ab") +
  annotate("text", x = "2017.During", y = 8.8, label = "ab") +
  annotate("text", x = "2017.After", y = 8.8, label = "a") +
  annotate("text", x = "2018.Before", y = 8.8, label = "ab") +
  annotate("text", x = "2018.During", y = 8.8, label = "b") +
  annotate("text", x = "2018.After", y = 8.8, label = "ab") +
  annotate("text", x = "2019.Before", y = 8.8, label = "ab") +
  annotate("text", x = "2019.During", y = 8.8, label = "ab") +
  annotate("text", x = "2019.After", y = 8.8, label = "a") +
  theme_contam

contaminants_interaction_water_plot <- plot_grid(
  site, station_year_flow_interaction, 
  ncol = 1, 
  align = "v"
)

ggsave(
  filename = file.path(fp_output, "contaminants_water_interaction_v1.png"),
  plot = contaminants_interaction_water_plot,
  height = 12,
  width = 8,
  dpi = 600
)


# run lsmeans on year by flowperiod interaction and effect of stationcode
emmeans4 <- emmeans(by_station, specs = pairwise ~ Year_fac:FlowActionPeriod, adjust = "sidak")
print(test(emmeans4)$contrasts)

emmeans5 <- emmeans(by_station, specs = pairwise ~ StationCode, adjust = "sidak")
print(test(emmeans5)$contrasts)
# RD22 is significantly higher
# no differences among years in before period
# 2018 during is higher than 2016, 2017, 2019 after and 2018 during is higher than 2016 during

# one-way ANOVA comparing RD22, BL5 and SHR- need to drop 2016 bc no SHR samples
by_station_one_way <- lm(log(totals) ~ StationCode, water_total2[(water_total2$Year_fac != "2016" & water_total2$StationCode == "SHR" | water_total2$StationCode == "BL5" | water_total2$StationCode == "RD22"), ])
summary(by_station_one_way)
by_station_one_way_ANOVA <- Anova(by_station_one_way, type = 2)
by_station_one_way_ANOVA

emmeans6 <- emmeans(by_station_one_way, specs = pairwise ~ StationCode, adjust = "sidak")
print(test(emmeans6)$contrasts)
# RD22 significantly higher than SHR and BL5, BL5 significantly higher than SHR

# figure of SHR and yolo site comparisons: 2 panels, one for water and one for zoops

################## find analytes that exceed EPA benchmarks ###################
# water samples:
# use water object, remove analytes <MDL, merge datasets on Analyte name, mutate
# a new column for yes if any benckmark exceeded and separate columns for each
# individual benchmark
# remove leading symbols from epa_benchmarks- want acute 1 and chronic 2 (for
# fish) and acute 3 and chronic 4 (inverts)
epa_benchmarks_c1 <- epa_benchmarks %>% 
  select(Pesticide, matches("[1-4]$")) %>% 
  mutate(
    across(starts_with(c("Acute", "Chronic")), ~ str_remove(.x, "[:symbol:] ")),
    # remove trailing characters in pesticide compound name using regex
    Pesticide = str_remove(Pesticide, ".{18}$")
  )

# Look for Analytes without matches in the EPA benchmarks data frame 
contam_all %>% 
  filter(
    Response_type != "zooplankton",
    Result != "< MDL"
  ) %>%
  anti_join(epa_benchmarks_c1, by = c("Analyte" = "Pesticide")) %>% 
  distinct(Analyte)
# There are 15 analytes without matches

# Update the Pesticide names in the EPA benchmarks data frame so they match with
# names in the contaminants concentration data set
epa_benchmarks_c2 <- epa_benchmarks_c1 %>% 
  mutate(
    Pesticide = case_when(
      Pesticide == "Propanil degradate - 3,4-Dichloroaniline (3,4-DCA)" ~ "3,4-Dichloroaniline",
      Pesticide == "Thiophanate methyl degradate Carbendazim (HOE 017411)" ~ "Carbendazim",
      Pesticide == "Fipronil degradate fipronil desulfinyl (MB46513)" ~ "Desulfinylfipronil",
      Pesticide == "Fipronil degradate fipronil sulfide (MB45950)" ~ "Fipronil sulfide",
      Pesticide == "Fipronil degradate fipronil sulfone (MB46136)" ~ "Fipronil sulfone",
      Pesticide == "Piperonyl Butoxide" ~ "Piperonyl butoxide",
      Pesticide == "Dichlorvos (DDVP)" ~ "Dichlorvos",
      Pesticide == "Imidacloprid urea degradate" ~ "Imidacloprid urea",
      TRUE ~ Pesticide
    )
  )

# Find water and suspended sediment samples that exceeded an EPA benchmark
water_ss_benchmarks <- contam_all %>%
  filter(
    Response_type != "zooplankton",
    Result != "< MDL"
  ) %>%
  left_join(epa_benchmarks_c2, by = c("Analyte" = "Pesticide")) %>% 
  mutate(
    # convert Result values to micrograms to be consistent with EPA benchmarks
    Result = as.numeric(Result) / 1000,
    across(starts_with(c("Acute", "Chronic")), as.numeric),
    across(starts_with(c("Acute", "Chronic")), ~ if_else(Result > .x, "Yes", "No"), .names = "Exceeds_{.col}")
  ) %>% 
  select(Response_type, Year, Analyte, FlowActionPeriod, starts_with("Exceeds")) %>% 
  mutate(
    Response_type = recode_factor(
      Response_type, 
      "water" = "Water", 
      "sediment" = "Suspended Sediment"
    )
  )

# Count number of EPA benchmark exceedances by sample type, year, analyte, flow
# period, and benchmark type
water_ss_benchmarks_exceed <- water_ss_benchmarks %>% 
  pivot_longer(
    cols = starts_with("Exceeds"),
    names_to = "Benchmark",
    values_to = "Exceedance"
  ) %>% 
  mutate(
    Benchmark = recode_factor(
      Benchmark,
      "Exceeds_Acute1" = "Acute_fish",
      "Exceeds_Chronic2" = "Chronic_fish",
      "Exceeds_Acute3" = "Acute_invert",
      "Exceeds_Chronic4" = "Chronic_invert"
    )
  ) %>% 
  filter(Exceedance == "Yes") %>% 
  count(Response_type, Year, Analyte, FlowActionPeriod, Benchmark, name = "N_Exceedance")

# Count number of EPA benchmark exceedances by sample type, year, flow period,
# and benchmark type, and export for report figures
water_ss_benchmarks_exceed %>% 
  group_by(Response_type, Year, FlowActionPeriod, Benchmark) %>% 
  summarize(N_Exceedance = sum(N_Exceedance)) %>% 
  ungroup() %>% 
  complete(Response_type, Year, FlowActionPeriod, Benchmark, fill = list(N_Exceedance = 0)) %>% 
  write_csv(file.path(fp_contam, "epa_benchmark_exceedances_flowperiod.csv"))

# Count number of EPA benchmark exceedances by sample type, year, analyte,
# and benchmark type, and export for report table
water_ss_benchmarks_exceed %>%
  group_by(Response_type, Year, Analyte, Benchmark) %>% 
  summarize(N_Exceedance = sum(N_Exceedance)) %>% 
  ungroup() %>%
  complete(nesting(Response_type, Year, Analyte), Benchmark, fill = list(N_Exceedance = 0)) %>% 
  pivot_wider(names_from = Benchmark, values_from = N_Exceedance) %>% 
  left_join(contam_type_class, by = "Analyte") %>%
  select(
    Year,
    Analyte,
    PesticideType,
    PesticideClass,
    ends_with("fish"),
    ends_with("invert"),
    Response_type
  ) %>% 
  arrange(Year, Analyte, Response_type) %>% 
  write_csv(file.path(fp_output, "epa_benchmark_exceedances_analytes.csv"))

