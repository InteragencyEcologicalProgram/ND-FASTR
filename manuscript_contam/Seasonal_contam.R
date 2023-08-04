# Analyze contaminants data by hydrophobicity for zoop and water
# Laura Twardochleb
# 5/19/2023

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

#2. Read in flow and pesticide concentration data for zooplankton --------------------------------------------------------------------------------------------------------

# Import total pesticide concentration data for SHR and STTD
df_shr_conc <- read_excel(
  here(contam_data, "DataforReportFigs_toDWR.xlsx"),
  sheet = "Figs 7&9 Seasonal Zoop",
  range = "A1:D22"
)

df_sttd_conc <- read_excel(
  here(contam_data, "DataforReportFigs_toDWR.xlsx"),
  sheet = "Figs 7&9 Seasonal Zoop",
  range = "F1:I22"
)

df_flow<-read_csv(
    here(flow_data, "LIS_SR_DailyAvgFlow_2013-2022.csv")) %>%mutate(Date=mdy(Date))

#3. Clean and Integrate pesticide count and flow Data for zooplankton ---------------------------------------------

# Prepare data for SHR and STTD to be combined
df_shr_conc_c <- df_shr_conc %>% 
  mutate(Date = date(`Date SHR`),
         Station="SHR")%>%
  group_by(Station, Date)%>%
summarize(totalconc=sum(`Total Fungicide Concentration zoop ng/g`,`Total Herbicide Concentration zoop ng/g`,`Total Insecticide Concentration zoop ng/g`))

df_sttd_conc_c <- df_sttd_conc %>% 
  mutate(Date = date(`Date STTD`),
         Station="STTD")%>%
  group_by(Station, Date)%>%
  summarize(totalconc=sum(`Total Fungicide Concentration zoop ng/g`,`Total Herbicide Concentration zoop ng/g`,`Total Insecticide Concentration zoop ng/g`))


# Combine data for SHR and STTD
df_conc_all <- bind_rows(df_shr_conc_c, df_sttd_conc_c)

#combined conc and flow data
df_conc_flow <- df_conc_all %>% 
  left_join(
    df_flow %>% select(Date, SR_DailyAvgNetFlow, LIS_DailyAvgNetFlow),
    by = join_by(Date)
  )

#4. Explore and visualize concentration and flow data for zooplankton -----------------------------------------------------------------------------------------------

#visualize relationships with flow for each station
#overall conc. decreases with flow for SHR
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

#concentration appears higher during fall
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
hist(log10(shr$totalconc)) #data look pretty normally distributed
hist(sqrt(sttd$totalconc)) #data look pretty normally distributed

#5. Analyze relationships between flow, DOY, and pesticide concentration for zooplankton -----------------------------------------------------------------------------------------------

#try running gams with a smoother for day of year
#SHR
shr_zoop_gam <- gam(log10(totalconc) ~ SR_DailyAvgNetFlow+s(DOY, bs="cr"), data = shr,method = "REML", family="gaussian")
summary(shr_zoop_gam)
appraise(shr_zoop_gam)
k.check(shr_zoop_gam)
draw(shr_zoop_gam, select = 1, residuals = TRUE, rug = FALSE)
plot(shr_zoop_gam, pages = 1, all.terms = TRUE)
AIC(shr_zoop_gam)

#STTD
sttd_zoop_gam <- gam(sqrt(totalconc) ~ LIS_DailyAvgNetFlow+s(DOY, bs="cr"), data = sttd,method = "REML", family="gaussian")
summary(sttd_zoop_gam)
appraise(sttd_zoop_gam)
k.check(sttd_zoop_gam)
draw(sttd_zoop_gam, select = 1, residuals = TRUE, rug = FALSE)
plot(sttd_zoop_gam, pages = 1, all.terms = TRUE)
AIC(sttd_zoop_gam)

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

#either way, they have opposite relationships with use in the two watersheds



#8. Analyze relationships between flow, DOY, pesticide use, and pesticide concentration for zooplankton -----------------------------------------------------------------------------------------------
shr2<-df_conc_flow_c%>%filter(Station=="SHR")%>%mutate(DOY = yday(Date))%>%mutate(application_watershed_area=total_application/22526.79948) #standardize application data
sttd2<-df_conc_flow_c%>%filter(Station=="STTD")%>%mutate(DOY = yday(Date))%>%mutate(application_watershed_area=total_application/4217.503529)

#try pesticide load- multiply flow by concentration in water and convert units to kg/day or g/day and use rainfall and application and smooth for DOY as a predictor
#try running gams with a smoother for day of year- try with lag of application data and try with rainfall as predictor
#SHR
shr_zoop_gam2 <- gam(log10(totalconc) ~ SR_DailyAvgNetFlow+ application_watershed_area +s(DOY, bs="cr"), data = shr2,method = "REML", family="gaussian")
summary(shr_zoop_gam2)
appraise(shr_zoop_gam2)
k.check(shr_zoop_gam2)
draw(shr_zoop_gam2, select = 1, residuals = TRUE, rug = FALSE)
plot(shr_zoop_gam2, pages = 1, all.terms = TRUE)
AIC(shr_zoop_gam2)

#STTD- select this model 
sttd_zoop_gam2 <- gam(sqrt(totalconc) ~ LIS_DailyAvgNetFlow+application_watershed_area+s(DOY, bs="cr"), data = sttd2,method = "REML", family="gaussian")
summary(sttd_zoop_gam2)
appraise(sttd_zoop_gam2)
k.check(sttd_zoop_gam2)
draw(sttd_zoop_gam2, select = 1, residuals = TRUE, rug = FALSE)
plot(sttd_zoop_gam2, pages = 1, all.terms = TRUE)
AIC(sttd_zoop_gam2)

#STTD with one-month lag on application data- previous model appears to be a better fit to data
sttd3<-sttd2%>%mutate(previous_month_application = dplyr::lag(application_watershed_area, n=2))

sttd_zoop_gam3 <- gam(sqrt(totalconc) ~ LIS_DailyAvgNetFlow+previous_month_application+s(DOY, bs="cr"), data = sttd3,method = "REML", family="gaussian")
summary(sttd_zoop_gam3)
appraise(sttd_zoop_gam3)
k.check(sttd_zoop_gam3)
draw(sttd_zoop_gam3, select = 1, residuals = TRUE, rug = FALSE)
plot(sttd_zoop_gam3, pages = 1, all.terms = TRUE)
AIC(sttd_zoop_gam3)

