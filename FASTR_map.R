# Create map of FASTR sampling locations
# Laura Twardochleb
# 11/21/23

#1. Global Code and Functions ---------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)

setwd("C:/Users/ltwardochleb/Documents/NDFA/ND-FASTR/manuscript_synthesis/")

require(sf)
require(ggplot2)
require(dplyr)

SubRegions<-deltamapr::R_EDSM_Subregions_Mahardja%>%
  filter(Region!="South")%>%
  filter(!SubRegion%in%c("Upper Napa River", "Lower Napa River", "San Pablo Bay", "San Francisco Bay"))

yolo<-sf::st_read("/data/Yolo Bypass Extent")%>%
  st_transform(crs=st_crs(SubRegions))%>%
  st_union()

add<-tibble(Latitude=c(37.9, 38.8), Longitude=c(-121.9, -121.2))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  st_transform(crs=st_crs(SubRegions))

base<-deltamapr::WW_Watershed%>%
  st_transform(crs=st_crs(SubRegions))%>%
  st_crop(st_union(st_union(st_buffer(yolo, units::set_units(3000, "m")), SubRegions), add))%>%
  filter(!HNAME%in%c("PUTAH CREEK", "LAKE BERRYESSA", "SOUTH FORK PUTAH CREEK", "LAKE CURRY", "UPPER SAN LEANDRO RESERVOIR", "BETHANY RESERVOIR", "LAKE CHABOT", "SAN FRANCISCO BAY"))%>%
  st_union()

stations<-tibble(Station=c("RCS", "RD22", "I80", "LIS", "STTD", "BL5", "LIB", "RYI", "RVB"),
                 Region=c(rep("Upstream", 5), rep("Downstream", 4)),
                 Latitude=c(38.793457, 38.676367, 38.573111, 38.474816, 38.353461, 38.274460, 38.242100, 38.213167, 38.159737),
                 Longitude=c(-121.725447, 	-121.643972, -121.582958, -121.588584, -121.642975, -121.665652, -121.684900,-121.668591, -121.686355))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)%>%
  mutate(Region=factor(Region, levels=c("Upstream", "Downstream")))

locations_points<-tibble(Location=c( "Freeport", "Antioch", "Rio Vista"),
                         Latitude=c(38.461562, 38.004094, 38.155604),
                         Longitude=c(-121.499371, -121.805606, -121.691347))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)

locations_text<-tibble(Location=c("Freeport", "Antioch", "Rio Vista", "Cache Slough", "Yolo Bypass", "Sacramento River"),
                       Latitude=c(38.481562, 37.99, 38.155603, 38.36, 38.5, 38.51),
                       Longitude=c(-121.419371, -121.805606, -121.8, -121.8, -121.75, -121.35))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)


p<-ggplot()+
  geom_sf(data=yolo, color=NA, fill="gray80", alpha=0.5)+
  geom_sf(data=base, fill="slategray3", color="slategray4")+
  geom_sf(data=locations_points)+
  geom_sf(data=stations, aes(fill=Region), shape=21, color="black", size=3)+
  geom_sf_text(data=locations_text, aes(label=Location), lineheight = 1)+
  scale_fill_manual(values=c("#E41A1C", "#377EB8"))+
  theme_void()
p

ggsave("FASTR_MS_map.png", plot=p, device="png", width=8, height=8, units = "in")
