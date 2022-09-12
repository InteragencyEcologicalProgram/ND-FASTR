## Generate maps of EMP's fixed sampling locations
## 7/25/2021

library("tidyverse");packageVersion("tidyverse")
library("deltamapr");packageVersion("deltamapr")
library("sf");packageVersion("sf")
library("ggrepel");packageVersion("ggrepel")
library("maps");packageVersion("maps")
library("RColorBrewer");packageVersion("RColorBrewer")

# Set working directory
setwd("./FASTR.phyto.final/")
getwd()

## Clean workspace
rm(list=ls()) 

# Set folder name to output graphs into
output <- "plots"

## Read EMP Station data
df_EMP <- read_csv("CSVs/EMP_Stations_All.csv")
df_EMP$Longitude <- df_EMP$Longitude * -1

# Read region data
df_regions <- read_csv("CSVs/station_regions_EMP.csv")

# Rename header for StationCode & select phyto stations
df_EMP <- df_EMP %>% 
  rename("StationCode" = "Station ID") %>%
  filter(StationType == "Phytoplankton")

df_EMP <- df_EMP %>% select(StationCode:Longitude)

# Combine region data and phyto data
df_EMP <- left_join(df_EMP, df_regions)

# Select only stations in FASTR report
df_EMP <- df_EMP %>%
  filter(!Region %in% c("San.Pablo.Bay","Grizzly.and.Suisun.Bay","Entrapment.Zone"))

#Import data about city locations
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")
cities$long <- cities$long * -1

## Plot EMP station maps
plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_point(data = df_EMP,
             aes(x = Longitude,
                 y = Latitude,
                 fill = Region,
                 size = 3),
             pch = 21,
             color = "black") +
  #scale_fill_manual(values = c("red","purple")) +
  geom_point(data = cities %>% arrange(pop) %>% tail(250),
             aes(x = long,
                 y = lat)) +
  geom_text_repel(data = cities %>% arrange(pop) %>% tail(250), 
                  aes(x = long,
                      y = lat, 
                      label = name)) +
  ylim(37.65, 38.4) +
  xlim(122.0, 121.2) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

plot + 
  labs(x = "Longitude",
            y = "Latitude",
            fill = "Region") +
  guides(size = "none")


ggsave(path=output,
       filename = "EMP.phyto.map.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=5.5,
       width=6.5, 
       dpi="print")
