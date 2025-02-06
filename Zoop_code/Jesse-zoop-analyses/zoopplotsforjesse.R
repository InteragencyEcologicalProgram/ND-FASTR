#quick pull of zooplankton for 

library(zooper)
library(tidyverse)
library(deltamapr)
library(sf)


zoops =  Zoopsynther(Data_type = "Community", Size_class = "Meso",
                    Years = c(2010:2020), Months = c(6:11), 
                    Sources = c("EMP", "FMWT", "STN", "20mm", "DOP"))

regions = R_EDSM_Subregions_1617P1 %>%
  filter(SubRegion %in% c("Sacramento River Ship Channel",
                          "Cache Slough and Liberty Island",
                          "Sacramento River near Rio Vista"  ))

zoops2 = st_as_sf(filter(zoops, !is.na(Latitude)),  coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(regions))

zoops3 = st_join(zoops2, regions) %>%
  st_drop_geometry() %>%
  mutate(Month = month(Date), Year = year(Date),
         Include = case_when(Year == 2016 & Month %in% c(6:9) ~ "Yes",
                             Year != 2016 & Month %in% c(7:11) ~ "Yes",
                             TRUE ~ "No")) %>%
  filter(!is.na(SubRegion), Include == "Yes", Taxlifestage != "Pseudodiaptomus_UnID Larva")

zoopsave = group_by(zoops3, Month, Year, SubRegion, Taxname) %>%
  summarize(CPUE = mean(CPUE))

ggplot(zoopsave, aes(x = Month, y = CPUE, fill = Taxname))+geom_col()+
  facet_grid(SubRegion~Year)

pseudo = filter(zoops3, Genus == "Pseudodiaptomus") %>%
  group_by(Month, Year, SubRegion, Taxlifestage) %>%
  summarize(CPUE = mean(CPUE))

ggplot(pseudo, aes(x = Month, y = CPUE, fill = Taxlifestage))+geom_col()+
  facet_grid(SubRegion~Year)

ggplot(pseudo, aes(x = Month, y = CPUE, fill = Taxlifestage))+geom_col(position = "dodge")+
  facet_grid(SubRegion~Year)

pseudosum = filter(zoops3, Genus == "Pseudodiaptomus") %>%
  group_by(Month, Year, SubRegion, SampleID) %>%
  summarize(Pseudo = sum(CPUE))

ggplot(pseudosum, aes(x = SubRegion, y = log(Pseudo+1)))+geom_boxplot()+
  facet_wrap(~Year)


#re-run without DOP

pseudo2 = filter(zoops3, Genus == "Pseudodiaptomus", Source != "DOP") %>%
  group_by(Month, Year, SubRegion, Taxlifestage) %>%
  summarize(CPUE = mean(CPUE))

ggplot(pseudo, aes(x = Month, y = CPUE, fill = Taxlifestage))+geom_col()+
  facet_grid(SubRegion~Year)

ggplot(pseudo, aes(x = Month, y = CPUE, fill = Taxlifestage))+geom_col(position = "dodge")+
  facet_grid(SubRegion~Year)

pseudosum2 = filter(zoops3, Genus == "Pseudodiaptomus", Source != "DOP") %>%
  group_by(Month, Year, SubRegion, SampleID) %>%
  summarize(Pseudo = sum(CPUE))

ggplot(pseudosum, aes(x = SubRegion, y = log(Pseudo+1)))+geom_boxplot()+
  facet_wrap(~Year)

#now by cladocera, clanoids, cyclopoids
zoops3.1 = filter(zoops3, Order %in% c("Cyclopoida", "Calanoida", "Cladocera"), 
                  Lifestage %in% c("Juvenile", "Adult")) %>%
  group_by(Month, Year, SubRegion, SampleID, Order) %>%
  summarize(CPUE = sum(CPUE))

zoops3.1ave = zoops3.1 %>%
  group_by(Month, Year, SubRegion, Order) %>%
  summarize(CPUE = mean(CPUE))

ggplot(zoops3.1ave, aes(x = Month, y = CPUE, fill = Order))+geom_col(position = "dodge")+
  facet_grid(SubRegion~Year)+
  coord_cartesian()
