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

stas = read_csv("fish data/Stations_Fish_NDFA_2021-01-13.csv")

#export an sf file of the stations so I can create a shapefile of regions in my shiny app
stashap = mutate(stas, geometry = NULL)
stashap = st_as_sf(stashap, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84")
save(stashap, file = "NDFAstations.RData")

#get the geometry column into the right format

ggplot() + geom_sf(data = stashap, aes(color = Region))

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
regions = read_sf("regions/FishRegions.shp")
regions$notes = c("Downstream", "Upstream", "Middle Sac")


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


#try to specify x axis breaks
ewbrks <- c(-121.7, -121.5)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x > 0, paste(x, "°E"), ifelse(x < 0, paste(-x, "°W"),x))))

# create the map

ggplot()+
  geom_sf(data = regions, aes(fill = notes), alpha = 0.5)+
  geom_sf(data = Hydro, fill = "lightblue")+
  geom_sf(data = NoSF, aes(shape = Survey.x, color = Survey.x), size = 3) +
  scale_shape_manual(values = c(15, 16, 17, 18, 11, 7, 10, 8), name = "Survey")+
  scale_fill_brewer(palette = "Pastel2", guide = NULL)+
  scale_color_brewer(palette = "Dark2", name = "Survey")+
  geom_sf_text(data = regions, aes(label = notes),  
               fontface = "bold",
               nudge_x = c(0, -0.05, -0.03), 
               nudge_y =  c(0, 0, 0.05))+
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  
  coord_sf(ylim = c(38, 38.9), xlim = c(-121.8, -121.48))+
  
  theme_bw()+
  theme(axis.text = element_text(size = 10))

