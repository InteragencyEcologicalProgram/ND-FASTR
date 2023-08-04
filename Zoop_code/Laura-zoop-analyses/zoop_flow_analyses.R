# Analyze zooplankton cpue data for effects of flow by region
# Laura Twardochleb
# 7/21/23

#1. Global Code and Functions ---------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(conflicted)
library(mgcv)
library(gratia)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# Define file path in the repository for figure and table outputs
output<- here("Zoop_code/Laura-zoop-analyses")

#2. Read in and combine flow and zoop cpue data --------------------------------------------------------------------------------------------------------

df_zoop<-read_csv(here(output, "clean_zoop_long.csv"))%>%group_by(Date, Region, StationCode)%>%
  summarize(totalCPUE=sum(CPUEZoop))

df_flow<-read_csv(here(output, "flow_daily_avg_2013-2019.csv"))

df_flow<-df_flow%>%
  mutate(Date=as.Date(Date, format="%m/%d/%Y"))%>% 
  filter(StationCode %in% c("RD22", "LIS")) %>% 
  mutate(StationCode = case_match(StationCode, "RD22" ~ "I80", "LIS" ~ "STTD")) %>% 
  bind_rows(df_flow)

df_zoop_flow<-df_zoop%>%left_join(df_flow)

#3. Manipulate data and explore relationship between zoop cpue and flow -----------------------------------------------------------------------------------
# Create a vector for the factor order of StationCode
sta_order <- c(
  "RCS",
  "RD22",
  "I80",
  "LIS",
  "STTD",
  "LIB",
  "RVB"
)

# Prepare chlorophyll and flow data for exploration and analysis
df_zoop_flow1 <- df_zoop_flow %>% 
  mutate(Year=year(Date))%>%
  # Remove all NA flow values
  drop_na(FlowAvg) %>%
  mutate(StationCode = factor(StationCode, levels = sta_order), Year_fct = factor(Year)) %>% 
  arrange(StationCode, Date)%>%
  filter(StationCode!="LIB")%>% #remove LIB because all flow values are negative
  mutate(DOY=yday(Date))
# Explore sample counts by Station

df_zoop_flow_summary <- df_zoop_flow1%>%
  group_by(StationCode, Year)%>%
  summarize(
    min_date = min(Date),
    max_date = max(Date),
    num_samples = n()) %>% 
  arrange(StationCode, Year) 

# Plots- explore the data with some plots of cpue vs. flow faceted by station, region, and year
hist(log(df_zoop_flow1$totalCPUE))

#by station
df_zoop_flow1 %>% 
  ggplot(aes(x = FlowAvg, y = log(totalCPUE))) +
  geom_point() +
  geom_smooth(formula = "y ~ x") +
  facet_wrap(vars(StationCode), scales = "free") +
  theme_bw()

#by region
df_zoop_flow1 %>% 
  ggplot(aes(x = FlowAvg, y = log(totalCPUE))) +
  geom_point() +
  geom_smooth(formula = "y ~ x") +
  facet_wrap(vars(Region), scales = "free") +
  theme_bw()

#by year
df_zoop_flow1 %>% 
  ggplot(aes(x = FlowAvg, y = log(totalCPUE))) +
  geom_point() +
  geom_smooth(formula = "y ~ x") +
  facet_wrap(vars(Year), scales = "free") +
  theme_bw()

#by year and region
df_zoop_flow1 %>% 
  ggplot(aes(x = DOY, y = log(totalCPUE))) +
  geom_point() +
  geom_smooth(formula = "y ~ x") +
  facet_wrap(vars(Year, Region), scales = "free") +
  theme_bw()

df_zoop_flow1 %>% 
  ggplot(aes(x = FlowAvg, y = log(totalCPUE))) +
  geom_point() +
  geom_smooth(formula = "y ~ x") +
  facet_wrap(vars(Year, Region), scales = "free") +
  theme_bw()

#it doesn't look like flow will be a good predictor of zoop cpue, but let's try running a model anyway for the upstream stations
#run a gam- a very poor fitting model
zoop_flow_gam <- gam(log(totalCPUE) ~ FlowAvg+StationCode+s(DOY, k=3), data = df_zoop_flow1,method = "REML", family="gaussian")
summary(zoop_flow_gam)
appraise(zoop_flow_gam)
k.check(zoop_flow_gam) #overfitted at k=9
draw(zoop_flow_gam, select = 1, residuals = TRUE, rug = FALSE) #try fitting a linear model

zoop_flow_lm<-lm(log(totalCPUE)~FlowAvg+StationCode, data = df_zoop_flow1)
summary(zoop_flow_lm)
plot(zoop_flow_lm) #no relationship with flow
