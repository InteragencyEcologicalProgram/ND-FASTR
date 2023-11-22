# Analyze contaminants data by hydrophobicity for zoop and water
# Laura Twardochleb
# 5/19/2023

#1. Startup commands ---------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(rlang)


#2. Read in and manipulate the data summarized by year and site------------------------------------------------------------------------------------------------------------------------------
hydrophob<-read_csv("C:/Users/ltwardochleb/Documents/NDFA/Contaminants/Data_analyses/Contaminants_MS/hydrophobicity.csv") #data summarized by year and site

#pivot to long format and create habitat and matrix columns
hydrophob2<-hydrophob%>%pivot_longer(cols = c(`Sacramento_River_2019-2020_Water_Detection_Frequency`, `Sacramento_River_2019-2020_Zooplankton_Detection_Frequency`, `Toe_Drain_2019-2020_Water_Detection_Frequency`, `Toe_Drain_2019-2020_Zooplankton_Detection_Frequency`),
                                     names_to = "Detection_type" , values_to = "Frequency")%>%mutate(Habitat=if_else(Detection_type %in% c("Sacramento_River_2019-2020_Water_Detection_Frequency","Sacramento_River_2019-2020_Zooplankton_Detection_Frequency"), "Sac", "Yolo"))%>%
  mutate(Matrix=if_else(Detection_type%in%c("Sacramento_River_2019-2020_Water_Detection_Frequency", "Toe_Drain_2019-2020_Water_Detection_Frequency"), "Water", "Zoop"))

#3. Analyze data -------------------------------------------------------------------------------------------------------------------------------------------------

hist(hydrophob2$Frequency) #detection frequency is zero inflated

hist(log(hydrophob2$Frequency+1)) #log-transformed data look better but still need to deal with zero-inflation

#use zero-inflated neg. binom model to model detection frequency by hydrophobicity, matrix, and habitat type
library(glmmTMB)

library(multcomp)

#zero-inflated model with matrix and hydrophobicity
phobtest1 = glmmTMB(Frequency~ log_Kow * Matrix, zi = ~Matrix*log_Kow, 
               family = "nbinom2", data = hydrophob2)

summary(phobtest1) #there is a significant interaction between matrix and hydrophobicity in the zero-inflation term and the non-zero inflation portion of the model

#examine the model fit

phobtest1
library(DHARMa)

phobtest1_simres <- simulateResiduals(phobtest1)

plot(phobtest1_simres)


#use zero-inflated neg. binom model to model detection frequency by hydrophobicity, matrix, and habitat type
phobtest2 = glmmTMB(Frequency~ log_Kow * Matrix + Habitat, zi = ~Matrix*log_Kow, 
                    family = "nbinom2", data = hydrophob2)

summary(phobtest2) #no significant differences between habitats

#4. Read in pesticide count and flow data summarized by site and date --------------------------------------------------------------------------------------------------------

# Import total pesticide concentration data for SHR and STTD
df_shr_count <- read_excel(
  here("DataforReportFigs_toDWR.xlsx"),
  sheet = "Figs 3&4",
  range = "A1:E37"
)

df_sttd_count <- read_excel(
  here("DataforReportFigs_toDWR.xlsx"),
  sheet = "Figs 3&4",
  range = "I1:M38"
)

df_davg_flow<-read_csv('LIS_SR_DailyAvgFlow_2013-2022.csv')%>%mutate(Date=mdy(Date))

#5. Clean and Integrate pesticide count and flow Data ---------------------------------------------

# Prepare data for SHR and STTD to be combined
df_shr_count_c <- df_shr_count %>% 
  transmute(
    Date = date(Date),
    Station = "SHR",
    TotalCount_Zoop = `SHR Zoop Pesticide Count`,
    TotalCount_Water = `SHR water Pesticide Count`
  ) %>% 
  # remove count data for 2020
  filter(year(Date) < 2020)

df_sttd_count_c <- df_sttd_count %>% 
  transmute(
    Date = date(Date),
    Station = "STTD",
    TotalCount_Zoop = `STTD Zoop Pesticide Count`,
    TotalCount_Water = `STTD water Pesticide Count`
  ) %>% 
  # remove count data for 2020
  filter(year(Date) < 2020) %>% 
  # remove water count data for 2017-2018 since it's from LIS
  mutate(TotalCount_Water = if_else(year(Date) < 2019, NA_real_, TotalCount_Water))

# Combine data for SHR and STTD
df_count_all <- bind_rows(df_shr_count_c, df_sttd_count_c)
#combined SHR count and flow data
df_counts_flow_shr <- df_count_all %>% 
  filter(Station == "SHR") %>% 
  left_join(
    df_davg_flow %>% select(Date, DailyAvgNetFlow = SR_DailyAvgNetFlow),
    by = join_by(Date)
  )

#combine STTD flow and count data
df_counts_flow_sttd <- df_count_all %>% 
  filter(Station == "STTD") %>% 
  left_join(
    df_davg_flow %>% select(Date, DailyAvgNetFlow = LIS_DailyAvgNetFlow),
    by = join_by(Date)
  )

# Combine data and filter to dates in July-November, drop nas, mutate year
df_zoop <- bind_rows(df_counts_flow_shr, df_counts_flow_sttd)%>%
  select(-TotalCount_Water)%>%
  filter(month(Date)<12&month(Date)>6)%>%
  drop_na()%>%
  mutate(Year = as.character(year(Date)), .after = Date)

df_water<- bind_rows(df_counts_flow_shr, df_counts_flow_sttd)%>%
  select(-TotalCount_Zoop)%>%
  filter(month(Date)<12&month(Date)>6)%>%
  drop_na()%>%
  mutate(Year = as.character(year(Date)), .after = Date)

df_all<-bind_rows(df_counts_flow_shr, df_counts_flow_sttd)%>%
  filter(month(Date)<12&month(Date)>6)%>%
  drop_na()%>%
  mutate(Year = as.character(year(Date)), .after = Date)%>%
  pivot_longer(cols=starts_with("Total"), names_to = "Matrix", values_to = "TotalCount")

#6. Explore and analyze count and flow data -----------------------------------------------------------------------------------------------

#2019, STTD analyses: flow*habitat + flow*matrix + habitat*matrix;
#by year- STTD only has data for 2019, so can only compare water and zoop in 2019
#counts are higher in water at STTD than SHR, but they appear about the same in zoop between sites; 
#water increases with flow at both sites and zoop counts increase with flow at STTD but decreases with flow at SHR;
#counts are higher in water than zoop at STTD and higher in zoop than water at SHR
df_all %>% filter(Year==2019)%>%
  ggplot(aes(x = DailyAvgNetFlow, y = TotalCount, color = Matrix)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Daily average net flow (cfs)",
    y = "Number of pesticides detected"
  ) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(vars(Station), scales = "free") +
  theme_bw()

#as Dave mentioned, probably need to analyze the data separately for the two sites due to differences in flow magnitude
#will focus on comparing water and zoop at STTD in 2019 and water and zooplankton at SHR across years, zoop at STTD across years

df_2019STTD<-df_all%>%filter(Year==2019)%>%filter(Station=="STTD")

#water and zoop, 2019, STTD
hist(df_2019STTD$TotalCount) #data look pretty normally distributed

#set up poisson or negative binomial model for 2019 STTD data

poisson2019STTD<-glm(TotalCount~Matrix*DailyAvgNetFlow, family=poisson(), data=df_2019STTD)
poisson2019STTD
summary(poisson2019STTD) #overdispersion based on residual deviance, try nb model

library(MASS)
negb<-glm.nb(TotalCount~Matrix*DailyAvgNetFlow, data=df_2019STTD)
summary(negb)

#assess goodness of fit for these models
phobtest2_simres <- simulateResiduals(negb)

plot(phobtest2_simres)

#only matrix is significant- zoop has lower numbers of detections than water at STTD in 2019; no signifcant effects of flow


#SHR- flow* matrix across all years- both zoop and water counts increase with flow at SHR; next examine diffs by year
df_all %>% filter(Station=="SHR")%>%
  ggplot(aes(x = DailyAvgNetFlow, y = TotalCount, color = Matrix)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Daily average net flow (cfs)",
    y = "Number of pesticides detected"
  ) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  theme_bw()

#Differences by year and matrix at SHR; 2019 there is a decrease in zoop detections with increasing flow- otherwise zoop and water detections increase with flow
df_all %>% filter(Station=="SHR")%>%
  ggplot(aes(x = DailyAvgNetFlow, y = TotalCount, color = Matrix)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Daily average net flow (cfs)",
    y = "Number of pesticides detected"
  ) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  facet_wrap(vars(Year), scales = "free") +
  theme_bw()

#fit nb model
df_SHR<-df_all%>%filter(Station=="SHR")
hist(df_SHR$TotalCount) #data look approximately normal
hist(log(df_SHR$TotalCount+1)) #looks worse

negbSHR<-df_all%>%filter(Station=="SHR")%>%glm.nb(TotalCount~Matrix*DailyAvgNetFlow + Year*DailyAvgNetFlow +Year*Matrix, data=., control=glm.control(maxit=100))
summary(negbSHR)

#assess goodness of fit for these models
phobtest3_simres <- simulateResiduals(negbSHR)

plot(phobtest3_simres)

#marginal effect of flow on detections

negbSHR2<-df_all%>%filter(Station=="SHR")%>%glm.nb(TotalCount~Year*DailyAvgNetFlow +Year*Matrix, data=., control=glm.control(maxit=50))
summary(negbSHR2)

#assess goodness of fit for these models
phobtest4_simres <- simulateResiduals(negbSHR2)

plot(phobtest4_simres)



negbSHR3<-df_all%>%filter(Station=="SHR")%>%glm.nb(TotalCount~Year*Matrix, data=., control=glm.control(maxit=100))
summary(negbSHR3)

#assess goodness of fit for these models
phobtest5_simres <- simulateResiduals(negbSHR3)

plot(phobtest5_simres)


negbSHR4<-df_all%>%filter(Station=="SHR")%>%glm.nb(TotalCount~Matrix*DailyAvgNetFlow, data=., control=glm.control(maxit=100))
summary(negbSHR4)

#assess goodness of fit for these models
phobtest6_simres <- simulateResiduals(negbSHR4)

plot(phobtest6_simres)

negbSHR5<-df_all%>%filter(Station=="SHR")%>%glm.nb(TotalCount~Year*DailyAvgNetFlow, data=., control=glm.control(maxit=100))
summary(negbSHR5)

#assess goodness of fit for these models
phobtest7_simres <- simulateResiduals(negbSHR5)

plot(phobtest7_simres)

#additive model
negbSHR6<-df_all%>%filter(Station=="SHR")%>%glm.nb(TotalCount~Year + Matrix + DailyAvgNetFlow, data=., control=glm.control(maxit=100))
summary(negbSHR4)

#assess goodness of fit for these models
phobtest8_simres <- simulateResiduals(negbSHR6)

plot(phobtest8_simres)

#use AIC model selection to select model
AIC(negbSHR, negbSHR2, negbSHR3, negbSHR4, negbSHR5, negbSHR6) #negbSHR2 has lowest AIC score, but negbSHR is nearly the same

#negbSHR2 shows that there are significant differences by matrix, year, and that detections increase significantly with flow; there is a significant interaction between year and matrix 

#plot raw data- zooplankton at STTD for all years- it looks like zoop detections increase with flow
df_zoop %>% filter(Station=="STTD")%>%
  ggplot(aes(x = DailyAvgNetFlow, y = TotalCount_Zoop, color = Year)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(
    x = "Daily average net flow (cfs)",
    y = "Number of pesticides detected in zooplankton"
  ) +
  theme_bw()


