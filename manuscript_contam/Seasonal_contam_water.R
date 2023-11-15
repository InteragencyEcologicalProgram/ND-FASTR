# Analyze seasonal contaminants concentration data for water
# Laura Twardochleb
# 9/7/2023

#1. Global Code and Functions ---------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(rlang)
library(conflicted)
library(mgcv)
library(gratia)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# Define file path in the repository for figure and table outputs
contam_output <- here("manuscript_contam/figs_tables")

# Define file path in the repository for data
contam_data<- here("manuscript_contam/data/raw")
flow_data<-here("manuscript_contam/data/processed")

#2. Read in flow and pesticide concentration data for water --------------------------------------------------------------------------------------------------------

# Import total pesticide concentration data for SHR and STTD
df_shr_conc <- read_excel(
  here(contam_data, "DataforReportFigs_toDWR.xlsx"),
  sheet = "Fig6&8 Seasonal water",
  range = "A1:E22"
)

df_sttd_conc <- read_excel(
  here(contam_data, "DataforReportFigs_toDWR.xlsx"),
  sheet = "Fig6&8 Seasonal water",
  range = "G1:K22"
)

df_flow<-read_csv(
  here(flow_data, "LIS_SR_DailyAvgFlow_2013-2022.csv")) %>%mutate(Date=mdy(Date))

#3. Clean and Integrate pesticide count and flow Data for water ---------------------------------------------

# Prepare data for SHR and STTD to be combined
df_shr_conc_c <- df_shr_conc %>% 
  mutate(Date = date(`Date SHR`),
         Station="SHR")%>%
  group_by(Station, Date)%>%
  summarize(totalconc=sum(`Total Fungicide Concentration water ng/L`,`Total Herbicide Concentration water ng/L`,`Total Insecticide Concentration water ng/L`))

df_sttd_conc_c <- df_sttd_conc %>% 
  mutate(Date = date(`Date STTD`),
         Station="STTD")%>%
  group_by(Station, Date)%>%
  summarize(totalconc=sum(`Total Fungicide Concentration water ng/L`,`Total Herbicide Concentration water ng/L`,`Total Insecticide Concentration water ng/L`))


# Combine data for SHR and STTD
df_conc_all <- bind_rows(df_shr_conc_c, df_sttd_conc_c)

#combined conc and flow data
df_conc_flow <- df_conc_all %>% 
  left_join(
    df_flow %>% select(Date, SR_DailyAvgNetFlow, LIS_DailyAvgNetFlow),
    by = join_by(Date)
  )

#4. Explore and visualize concentration and flow data for water -----------------------------------------------------------------------------------------------

#visualize relationships with flow for each station
#overall conc. increases with flow for SHR
df_conc_flow %>% filter(Station=="SHR")%>%
  ggplot(aes(x = SR_DailyAvgNetFlow, y = totalconc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Daily average net flow (cfs)",
    y = "Pesticide concentration"
  )  +
  theme_bw()

#overall conc. increases with flow for STTD
df_conc_flow %>% filter(Station=="STTD")%>%
  ggplot(aes(x = LIS_DailyAvgNetFlow, y = totalconc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Daily average net flow (cfs)",
    y = "Pesticide concentration"
  )  +
  theme_bw()

#concentration appears higher during summer- but concentration higher in zooplankton during fall so it's possible that water concentration in summer predicts zoop concentration in fall
df_conc_flow %>% filter(Station=="SHR")%>%
  ggplot(aes(x = Date, y = totalconc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Month",
    y = "Pesticide concentration"
  )  +
  theme_bw()

#relationship between month and concentration appears complicated
df_conc_flow %>% filter(Station=="STTD")%>%
  ggplot(aes(x = Date, y = totalconc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Month",
    y = "Pesticide concentration"
  )  +
  theme_bw()

shr<-df_conc_flow%>%filter(Station=="SHR")%>%mutate(DOY = yday(Date))
sttd<-df_conc_flow%>%filter(Station=="STTD")%>%mutate(DOY = yday(Date))
#zoop conc. data distribution

hist(shr$totalconc) #data look pretty uniformly distributed
hist(log(sttd$totalconc)) #data look pretty normally distributed

#5. Analyze relationships between flow, DOY, and pesticide concentration for zooplankton -----------------------------------------------------------------------------------------------

#try running gams with a smoother for day of year
#SHR
shr_water_gam <- gam((totalconc) ~ SR_DailyAvgNetFlow+s(DOY, bs="cr"), data = shr,method = "REML", family="gaussian")
summary(shr_water_gam)
appraise(shr_water_gam)
k.check(shr_water_gam)
draw(shr_water_gam, select = 1, residuals = TRUE, rug = FALSE)
plot(shr_water_gam, pages = 1, all.terms = TRUE)
AIC(shr_water_gam)

#STTD
sttd_water_gam <- gam(log(totalconc) ~ LIS_DailyAvgNetFlow+s(DOY, bs="cr"), data = sttd,method = "REML", family="gaussian")
summary(sttd_water_gam)
appraise(sttd_water_gam)
k.check(sttd_water_gam)
draw(sttd_water_gam, select = 1, residuals = TRUE, rug = FALSE)
plot(sttd_water_gam, pages = 1, all.terms = TRUE)
AIC(sttd_water_gam)

#6. Read-in and manipulate use data -------------------------------------------------------------------------------------------------------------------------------------------------
df_use<-read_csv(
  here(flow_data, "pesticide_use_daily_tot_2017-2020.csv")) 

#pivot wider to summarize the different use classes into total use and average application of each class by month, calculate average total application by month
df_use_c<-df_use%>%pivot_wider(names_from=PesticideClass, values_from = TotalApplication)%>%
  mutate(Month=month(Date))%>%
  group_by(Region, Year, Month)%>%
  summarize(total_application=sum(Pyrethroid, Other, Rice, na.rm = TRUE))%>%#are the NA values zeros or are they missing? how to handle?
  mutate(Station=case_when(Region=="Toe Drain" ~ "STTD", 
                           Region=="Sacramento River" ~ "SHR"))

#prepare to combine with the zoop pesticide dataset
df_conc_flow_c<-df_conc_flow%>%mutate(Month=month(Date))%>%mutate(Year=year(Date))%>%
  left_join(df_use_c, by=c("Station", "Year", "Month"))


#7. Explore use data and relationship with concentration--------------------------------------------------------------------------------------------------------
#want to standardize use data to area of the watershed for yolo and sac river

#divided by full watershed area in sq miles
df_conc_flow_c %>% filter(Station=="STTD")%>%
  ggplot(aes(x = total_application/4217.503529, y = totalconc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Pesticide use",
    y = "Pesticide concentration"
  )  +
  theme_bw()


df_conc_flow_c %>% filter(Station=="SHR")%>%
  ggplot(aes(x = total_application/22526.79948, y = totalconc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Pesticide use",
    y = "Pesticide concentration"
  )  +
  theme_bw()

#divided by public land survey area in 2019 in square miles
df_conc_flow_c %>% filter(Station=="STTD")%>%
  ggplot(aes(x = total_application/1844.22, y = totalconc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Pesticide use",
    y = "Pesticide concentration"
  )  +
  theme_bw()


df_conc_flow_c %>% filter(Station=="SHR")%>%
  ggplot(aes(x = total_application/3463.938, y = totalconc)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Pesticide use",
    y = "Pesticide concentration"
  )  +
  theme_bw()


#8. Analyze relationships between flow, DOY, pesticide use, and pesticide concentration for zooplankton -----------------------------------------------------------------------------------------------
shr2<-df_conc_flow_c%>%filter(Station=="SHR")%>%mutate(DOY = yday(Date))%>%mutate(application_watershed_area=total_application/22526.79948) #standardize application data
sttd2<-df_conc_flow_c%>%filter(Station=="STTD")%>%mutate(DOY = yday(Date))%>%mutate(application_watershed_area=total_application/4217.503529)

#try pesticide load- multiply flow by concentration in water and convert units to kg/day or g/day and use rainfall and application and smooth for DOY as a predictor
#try running gams with a smoother for day of year- try with lag of application data and try with rainfall as predictor
#SHR
shr_water_gam2 <- gam((totalconc) ~ SR_DailyAvgNetFlow+ application_watershed_area +s(DOY, bs="cr"), data = shr2,method = "REML", family="gaussian")
summary(shr_water_gam2)
appraise(shr_water_gam2)
k.check(shr_water_gam2)
draw(shr_water_gam2, select = 1, residuals = TRUE, rug = FALSE)
plot(shr_water_gam2, pages = 1, all.terms = TRUE)
AIC(shr_water_gam2)

#SHR with one month lag of application data+ current application
shr3<-shr2%>%mutate(previous_month_application = dplyr::lag(application_watershed_area, n=2))

shr_water_gam3 <- gam((totalconc) ~ SR_DailyAvgNetFlow+previous_month_application+ application_watershed_area +s(DOY, bs="cr"), data = shr3,method = "REML", family="gaussian")
summary(shr_water_gam3)
appraise(shr_water_gam3)
k.check(shr_water_gam3)
draw(shr_water_gam3, select = 1, residuals = TRUE, rug = FALSE)
plot(shr_water_gam3, pages = 1, all.terms = TRUE)
AIC(shr_water_gam3)

#SHR with one month lag of application data
shr_water_gam4 <- gam((totalconc) ~ SR_DailyAvgNetFlow+previous_month_application+s(DOY, bs="cr"), data = shr3,method = "REML", family="gaussian")
summary(shr_water_gam4)
appraise(shr_water_gam4)
k.check(shr_water_gam4)
draw(shr_water_gam4, select = 1, residuals = TRUE, rug = FALSE)
plot(shr_water_gam4, pages = 1, all.terms = TRUE)
AIC(shr_water_gam4)

#SHR lm with flow and one month lag in application data- select this model
shr_water_lm <- lm((totalconc) ~ SR_DailyAvgNetFlow+previous_month_application, data = shr3)
summary(shr_water_lm)
plot(shr_water_lm)
AIC(shr_water_lm)

#SHR with a two month lag in application data
shr4<-shr2%>%mutate(previous_two_month_application = dplyr::lag(application_watershed_area, n=4))
#try running lm
shr_water_lm2 <- lm((totalconc) ~ SR_DailyAvgNetFlow+previous_two_month_application, data = shr4)
summary(shr_water_lm2)
plot(shr_water_lm2)
AIC(shr_water_lm2)

#STTD
sttd_water_gam2 <- gam(log(totalconc) ~ LIS_DailyAvgNetFlow+application_watershed_area+s(DOY, bs="cr"), data = sttd2,method = "REML", family="gaussian")
summary(sttd_water_gam2)
appraise(sttd_water_gam2)
k.check(sttd_water_gam2)
draw(sttd_water_gam2, select = 1, residuals = TRUE, rug = FALSE)
plot(sttd_water_gam2, pages = 1, all.terms = TRUE)
AIC(sttd_water_gam2)

#STTD with one-month lag on application data
sttd3<-sttd2%>%mutate(previous_month_application = dplyr::lag(application_watershed_area, n=2))

sttd_water_gam3 <- gam(log(totalconc) ~ LIS_DailyAvgNetFlow+previous_month_application+s(DOY, bs="cr"), data = sttd3,method = "REML", family="gaussian")
summary(sttd_water_gam3)
appraise(sttd_water_gam3)
k.check(sttd_water_gam3)
draw(sttd_water_gam3, select = 1, residuals = TRUE, rug = FALSE)
plot(sttd_water_gam3, pages = 1, all.terms = TRUE)
AIC(sttd_water_gam3)

#STTD-lm with application data
sttd_water_lm <- lm(log(totalconc) ~ LIS_DailyAvgNetFlow+previous_month_application, data = sttd3)
summary(sttd_water_lm)
plot(sttd_water_lm)
AIC(sttd_water_lm)

#STTD- lm with just flow- select this model
sttd_water_lm2 <- lm(log(totalconc) ~ LIS_DailyAvgNetFlow, data = sttd3)
summary(sttd_water_lm2)
plot(sttd_water_lm2)
AIC(sttd_water_lm2)

#STTD- lm with two month lag in application data
sttd4<-sttd2%>%mutate(previous_two_month_application = dplyr::lag(application_watershed_area, n=4))
sttd_water_lm3 <- lm(log(totalconc) ~ LIS_DailyAvgNetFlow+previous_two_month_application, data = sttd4)
summary(sttd_water_lm3)
plot(sttd_water_lm3)
AIC(sttd_water_lm3)

