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

# Define file path in the repository for contaminants directory
fp_contam <- here("Water_Quality/Contaminants")

# Import data
load(here("Water_Quality/Data_Processed/contam_proc_data.RData"))
epa_benchmarks <- read_csv(here("Water_Quality/Contaminants/Aquatic_life_benchmarks3.csv"), na = c("NR"))

# Prepare and combine data
contam_all <- 
  list(
    "water" = contam_water,
    "sediment" = contam_ss,
    "zooplankton" = contam_zoop
  ) %>% 
  bind_rows(.id = "Response_type") %>% 
  # Add Date, Year, Flow Action Periods, and Regions
  mutate(
    Date = date(DateTime),
    Year = year(DateTime)
  ) %>% 
  ndfa_action_periods() %>% 
  mutate(
    Year = as.character(Year),
    Region = case_when(
      StationCode %in% c("RCS", "RD22", "I80", "LIS", "STTD", "RMB") ~ "Upstream",
      StationCode %in% c("BL5", "RYI") ~ "Downstream",
      TRUE ~ NA_character_
    ),
    Region = factor(Region, levels = c("Upstream", "Downstream"))
  )

# Explore the data- summarize number of samples (by region, station, year, and flow pulse period)
number_stations <- contam_all %>% 
  filter(Result != "< MDL") %>%
  group_by(Response_type, Year, Region, FlowActionPeriod) %>%
  summarize(n = length(unique(StationCode))) %>% 
  ungroup() %>% 
  arrange(Response_type, Year, Region, FlowActionPeriod)

# not enough replication to analyze by region for water, zooplankton - can
# analyze water by year and flowperiod and zooplankton by year or flowperiod
by_year <- contam_all %>%
  filter(Result != "< MDL") %>%
  group_by(Response_type, Year, FlowActionPeriod) %>%
  summarize(n = length(unique(StationCode))) %>% 
  ungroup()

# only 'after' period for 2015 for water and no 'during' period for 2017 for
# zooplankton - need to drop from analysis, also drop SHR
water_total <- contam_all %>%
  filter(
    Response_type == "water",
    Year != "2015",
    Result != "< MDL",
    StationCode != "SHR"
  ) %>%
  group_by(Year, FlowActionPeriod, Date, StationCode) %>%
  summarize(totals = sum(as.numeric(Result))) %>% 
  ungroup()

zoop_total <- contam_all %>%
  filter(
    Response_type == "zooplankton",
    Result != "< MDL",
    StationCode != "SHR"
  ) %>%
  group_by(Year, FlowActionPeriod, Date, StationCode) %>%
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
  ggplot(aes(x = Year, y = log(totals))) +
  geom_boxplot(aes(fill = Year), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in water (ng/L))")), 
    limits = c(5, 10), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "2019")) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("2016", "2017", "2018", "2019")) +
  labs(fill = "Year", title = "A)") +
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
  scale_x_discrete(limits = c("Before", "During", "After")) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("Before", "During", "After")) +
  labs(fill = "Flow period", title = "B)") +
  xlab("Flow period") +
  annotate("text", x = "Before", y = 9.8, label = "a") +
  annotate("text", x = "During", y = 9.8, label = "b") +
  annotate("text", x = "After", y = 9.8, label = "c") +
  theme_contam

# year, flow period interaction
year_flowperiod_water_total <- water_total %>% 
  ggplot(aes(x = interaction(FlowActionPeriod, Year), y = log(totals))) +
  geom_boxplot(aes(fill = FlowActionPeriod), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in water (ng/L))")), 
    limits = c(5, 10), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    limits = c(
      "Before.2016",
      "During.2016",
      "After.2016",
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
    labels = c("", "2016", "", "", "2017", "", "", "2018", "", "", "2019", "")
  ) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("Before", "During", "After")) +
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
water.total.anova <- lm(log(totals) ~ Year * FlowActionPeriod, data = water_total)
summary(water.total.anova)
type3.water <- Anova(water.total.anova, type = 3)

# run specifying the order of terms differently (since unbalanced)
water.total.anova2 <- lm(log(totals) ~ FlowActionPeriod + Year, data = water_total)
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
  ggplot(aes(x = Year, y = log(totals))) +
  geom_boxplot(aes(fill = Year), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log(Pesticide in zooplankton (ng/g))")), 
    limits = c(3, 8),
    expand = c(0, 0)
  ) +
  scale_x_discrete(limits = c("2017", "2018", "2019")) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("2017", "2018", "2019")) +
  labs(fill = "Year", title = "A)") +
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
  scale_x_discrete(limits = c("Before", "During", "After")) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("Before", "During", "After")) +
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
  ggplot(aes(x = interaction(FlowActionPeriod, Year), y = log(totals))) +
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
  scale_colour_viridis_d(aesthetics = "fill", limits = c("Before", "During", "After")) +
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
  scale_x_discrete(limits = c("Before", "During", "After")) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("Before", "During", "After")) +
  labs(fill = "Flow period", title = "C)") +
  xlab("Flow period") +
  facet_wrap(vars(Year), nrow = 1) +
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
zoop.total.anova <- lm(log(totals) ~ Year * FlowActionPeriod, data = zoop_total)
summary(zoop.total.anova)
type3.zoop <- Anova(zoop.total.anova, type = 3)

# run type 2 anova (for unbalanced design, because can't include an interaction term)
zoop.total.anova2 <- lm(log(totals) ~ Year + FlowActionPeriod, data = zoop_total)
summary(zoop.total.anova2)
type2.zoop <- Anova(zoop.total.anova2, type = 2) # essentially same result as type 1 anova- use type 2
type2.zoop # year and flowperiod are significant

# tukey test on type 2 anova output
emmeans2 <- emmeans(zoop.total.anova2, specs = pairwise ~ Year, adjust = "sidak")
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
    Year != "2015",
    Result != "< MDL"
  ) %>%
  group_by(Year, FlowActionPeriod, Date, StationCode) %>%
  summarize(totals = sum(as.numeric(Result))) %>% 
  ungroup()

zoop_total2 <- contam_all %>%
  filter(
    Response_type == "zooplankton",
    Result != "< MDL"
  ) %>%
  group_by(Year, FlowActionPeriod, Date, StationCode) %>%
  summarize(totals = sum(as.numeric(Result))) %>% 
  ungroup()

# run welch's two-sample t-test for SHR and STTD for zoop samples
# convert data to wide format
zoop_total_wide <- pivot_wider(zoop_total2, names_from = StationCode, values_from = totals)

# SHR significantly higher than STTD overall
zoop.t.test <- t.test(log(zoop_total_wide$SHR), log(zoop_total_wide$STTD), na.action(na.omit))

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
  scale_fill_discrete(limits = c("Before", "During", "After")) +
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
  mutate(FlowActionPeriod = factor(FlowActionPeriod, levels = c("Before", "During", "After"))) %>% 
  ggplot(aes(x = interaction(Year, StationCode), y = log(totals))) +
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
by_station <- lm(log(totals) ~ StationCode + Year * FlowActionPeriod, water_total[(water_total$StationCode == "BL5" | water_total$StationCode == "RD22"), ])
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
  ggplot(aes(x = Year, y = log(totals))) +
  geom_boxplot(aes(fill = Year), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log total contaminant concentration (ng/L)")), 
    limits = c(5, 9), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(limits = c("2016", "2017", "2018", "2019"), labels = c("2016", "2017", "2018", "2019")) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("2016", "2017", "2018", "2019")) +
  labs(fill = "Year", title = "B)") +
  xlab("Year") +
  theme_contam

# flowperiod effect
flowperiod <- water_total %>%
  filter(StationCode == "RD22" | StationCode == "BL5") %>%
  ggplot(aes(x = FlowActionPeriod, y = log(totals))) +
  geom_boxplot(aes(fill = Year), notch = FALSE) +
  scale_y_continuous(
    name = expression(paste("log total contaminant concentration (ng/L)")), 
    limits = c(5, 9), 
    expand = c(0, 0)
  ) +
  scale_x_discrete(limits = c("Before", "During", "After"), labels = c("Before", "During", "After")) +
  scale_colour_viridis_d(aesthetics = "fill", limits = c("2016", "2017", "2018", "2019")) +
  labs(fill = "Year", title = "C)") +
  xlab("Year") +
  theme_contam

# year, flowperiod interaction
station_year_flow_interaction <- water_total %>%
  filter(StationCode == "RD22" | StationCode == "BL5") %>%
  ggplot(aes(x = interaction(Year, FlowActionPeriod), y = log(totals))) +
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
  scale_colour_viridis_d(aesthetics = "fill", limits = c("Before", "During", "After")) +
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
emmeans4 <- emmeans(by_station, specs = pairwise ~ Year:FlowActionPeriod, adjust = "sidak")
print(test(emmeans4)$contrasts)

emmeans5 <- emmeans(by_station, specs = pairwise ~ StationCode, adjust = "sidak")
print(test(emmeans5)$contrasts)
# RD22 is significantly higher
# no differences among years in before period
# 2018 during is higher than 2016, 2017, 2019 after and 2018 during is higher than 2016 during

# one-way ANOVA comparing RD22, BL5 and SHR- need to drop 2016 bc no SHR samples
by_station_one_way <- lm(log(totals) ~ StationCode, water_total2[(water_total2$Year != "2016" & water_total2$StationCode == "SHR" | water_total2$StationCode == "BL5" | water_total2$StationCode == "RD22"), ])
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
  mutate(
    across(starts_with(c("Acute", "Chronic")), ~ str_remove(.x, "[:symbol:] ")),
    # remove trailing characters in pesticide compound name using regex
    Pesticide = str_remove(Pesticide, ".{18}$")
  )

water_benchmarks <- contam_all %>%
  filter(Result != "< MDL") %>%
  left_join(epa_benchmarks_c1, by = c("Analyte" = "Pesticide"))
# convert Result values to micrograms to be consistent with EPA benchmarks
water_benchmarks$Result_micrograms <- as.numeric(water_benchmarks$Result) / 1000
water_benchmarks$Exceeds_Acute1 <- if_else(as.numeric(water_benchmarks$Result_micrograms) > as.numeric(water_benchmarks$Acute1), "Yes", "No")
water_benchmarks$Exceeds_Acute3 <- if_else(as.numeric(water_benchmarks$Result_micrograms) > as.numeric(water_benchmarks$Acute3), "Yes", "No")
water_benchmarks$Exceeds_Chronic2 <- if_else(as.numeric(water_benchmarks$Result_micrograms) > as.numeric(water_benchmarks$Chronic2), "Yes", "No")
water_benchmarks$Exceeds_Chronic4 <- if_else(as.numeric(water_benchmarks$Result_micrograms) > as.numeric(water_benchmarks$Chronic4), "Yes", "No")

# summarize by year an pesticide class: how many chronic vs. acute exceedances
# per flow period, per year? (may want to have a figure)
# how many exceedances of different pesticide classes?
acute_fish_summary <- water_benchmarks %>%
  group_by(Year, FlowActionPeriod) %>%
  filter(Exceeds_Acute1 == "Yes") %>%
  summarize(N_acute_fish = length(Exceeds_Acute1)) %>%
  arrange(Year, (factor(FlowActionPeriod, levels = c("Before", "During", "After"))), desc(FlowActionPeriod))

chronic_fish_summary <- water_benchmarks %>%
  group_by(Year, FlowActionPeriod) %>%
  filter(Exceeds_Chronic2 == "Yes") %>%
  summarize(N_chronic_fish = length(Exceeds_Chronic2)) %>%
  arrange(Year, (factor(FlowActionPeriod, levels = c("Before", "During", "After"))), desc(FlowActionPeriod))

acute_invert_summary <- water_benchmarks %>%
  group_by(Year, FlowActionPeriod) %>%
  filter(Exceeds_Acute3 == "Yes") %>%
  summarize(N_acute_invert = length(Exceeds_Acute3)) %>%
  arrange(Year, (factor(FlowActionPeriod, levels = c("Before", "During", "After"))), desc(FlowActionPeriod))

chronic_invert_summary <- water_benchmarks %>%
  group_by(Year, FlowActionPeriod) %>%
  filter(Exceeds_Chronic4 == "Yes") %>%
  summarize(N_chronic_invert = length(Exceeds_Chronic4)) %>%
  arrange(Year, (factor(FlowActionPeriod, levels = c("Before", "During", "After"))), desc(FlowActionPeriod))

write.csv(acute_fish_summary, file.path(fp_contam, "acute_fish_toxicity.csv"))
write.csv(acute_invert_summary, file.path(fp_contam, "acute_invert_toxicity.csv"))
write.csv(chronic_fish_summary, file.path(fp_contam, "chronic_fish_toxicity.csv"))
write.csv(chronic_invert_summary, file.path(fp_contam, "chronic_invert_toxicity.csv"))

# provide a table of analytes above benchmarks- for fish and inverts together
water_benchmarks_table <- water_benchmarks %>%
  select(c(Year, Analyte, Exceeds_Acute1, Exceeds_Chronic2, Exceeds_Acute3, Exceeds_Chronic4)) %>%
  filter(Exceeds_Acute1 == "Yes" | Exceeds_Chronic2 == "Yes" | Exceeds_Acute3 == "Yes" | Exceeds_Chronic4 == "Yes") %>%
  left_join(contam_type_class, by = "Analyte") %>%
  distinct() %>%
  arrange(Year)

write.csv(water_benchmarks_table, file.path(fp_output, "epa_benchmark_exceedances.csv"))

