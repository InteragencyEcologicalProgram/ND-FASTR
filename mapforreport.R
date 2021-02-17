#Make a map of the fish sampling stations for the report

library(tidyverse)

library(tidyverse)
library(leaflet)
library(sf)
library(sp)
library(maptools)
library(rgdal)
library(mapview)
library(ggmap)

fish = read_csv("fish data/FISH_MAN_allIEPsurveys_20201030.csv", guess_max = 10000)

stas = read_csv("fish data/Stations_NDFA_2020-10-30.csv")

#export an sf file of the stations so I can create a shapefile of regions in my shiny app
stashap = mutate(stas, geometry = NULL)
stashap = st_as_sf(stashap, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84")
save(stashap, file = "NDFAstations.RData")

#get the geometry column into the right format

AllSta2 = mutate(stas, geometry = NULL)
NoEsta = filter(AllSta2, !Survey.x %in%  c("EDSM", "NDFA", "USGS", "EMP"))

### GIS
# Make shapefile
NoEsta2 = NoEsta
coordinates(NoEsta) =~Longitude+Latitude
proj4string(NoEsta) <- CRS("+proj=longlat +datum=WGS84")

plot(NoEsta)
NoSF = st_as_sf(NoEsta)

# https://hub.arcgis.com/datasets/esri::usa-rivers-and-streams
Hydro <- read_sf("fish data/hydro_delta_marsh.shp")
head(Hydro)


library(viridis)
##### Make a map ----------------------
# Define palette
pal <- colorFactor("magma", domain = AllSta2$Survey.x)
regions = read_sf("regions/NDFAregions.shp")
regions$notes = c("Colusa Drain", "Upper Yolo", "Lower \n Yolo", "Cache Slough \n Complex", "Lower Sac \n River", "Middle Sac \n River")


NoEsta2 %>%
  leaflet()%>%
  addTiles() %>%
  addCircleMarkers(
    color = ~pal(Survey.x),
    stroke = FALSE,
    radius = 5,
    fillOpacity = 1,
    lng = ~Longitude,
    lat = ~Latitude,
    popup = ~paste(StationCode, "<br/>", 
                   "Region:", Region, "<br/>", 
                   "Survey:", Survey.x)) %>%
  addLegend(pal = pal,
    values = ~Survey.x,
            position = "bottomright")

ggplot()+
  geom_sf(data = regions, aes(fill = notes), alpha = 0.5)+
  geom_sf(data = Hydro, fill = "lightblue")+
  geom_sf(data = NoSF, aes(shape = Survey.x, color = Survey.x), size = 3) +
  scale_shape_manual(values = c(15, 16, 17, 18, 11, 7, 10, 8), name = "Survey")+
  scale_fill_brewer(palette = "Pastel1", guide = NULL)+
  scale_color_brewer(palette = "Dark2", name = "Survey")+
  geom_sf_text(data = regions, aes(label = notes),  
               fontface = "bold",
               nudge_x = c(0, -0.05, -0.03, -0.03, -0.06, 0), 
               nudge_y =  c(0, 0, 0.03, 0.05, 0, -.05))+
  coord_sf(xlim = c(-121.8, -121.48), ylim = c(38, 38.9))+
  theme_bw()

