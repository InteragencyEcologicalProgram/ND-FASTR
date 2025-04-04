## Generate map of FASTR sampling locations
## 6/3/2024

# Load required libraries ------------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(deltamapr)))
suppressWarnings(suppressMessages(library(sf)))
suppressWarnings(suppressMessages(library(ggrepel)))
suppressWarnings(suppressMessages(library(ggspatial)))
suppressWarnings(suppressMessages(library(maps)))
suppressWarnings(suppressMessages(library(here)))

# Set plot themes and output ---------------------------------------------------
plots <- here(file = here("manuscript_synthesis","plots"))

# Read in FASTR station and city location data ---------------------------------

# Import data about city locations
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")

# Import FASTR station data (Laura code)
SubRegions<-deltamapr::R_EDSM_Subregions_Mahardja%>%
  filter(Region!="South")%>%
  filter(!SubRegion%in%c("Upper Napa River", "Lower Napa River", "San Pablo Bay", "San Francisco Bay"))

yolo<-sf::st_read(here("manuscript_synthesis","data","Yolo Bypass Extent"))%>%
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

locations_points<-tibble(Location=c( "Freeport", "Antioch"),
                         Latitude=c(38.461562, 38.004094),
                         Longitude=c(-121.499371, -121.805606))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)

locations_text<-tibble(Location=c("Freeport", "Antioch", "Rio Vista", "Cache Slough", "Yolo Bypass", "Sacramento River"),
                       Latitude=c(38.481562, 37.99, 38.155603, 38.36, 38.5, 38.51),
                       Longitude=c(-121.419371, -121.805606, -121.77, -121.78, -121.7, -121.38))%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326)


# Plot all stations maps together ----------------------------------------------
theme_set(theme_bw())

# Set the theme
theme_update(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.border = element_rect(fill = NA, colour = "black"),
  strip.background = element_rect(fill = "gray", colour = "black"),
  legend.position = "bottom",
  legend.key = element_rect(fill = "white", colour = NA)
)

plot <- ggplot(WW_Delta) +
  geom_sf(data=yolo, color=NA, fill="gray80")+
  geom_sf(data=base, fill="slategray3", color="slategray4")+
  geom_sf(data=locations_points)+
  geom_sf_text(data=locations_text, aes(label=Location), lineheight = 1)+
  geom_sf(fill = "lightblue") + 
  geom_sf(data=stations, aes(fill=Region), shape=21, color="black", size=3)+
  geom_sf_text(data=stations, aes(label=Station), nudge_x = 0.05)+
  annotation_scale(location = "bl", 
                   width_hint = 0.4, 
                   unit_category = "imperial") +
  scale_fill_manual(values=c("#E41A1C", "#377EB8")) +
  ylim(38.0, 38.8) +
  xlim(-122.2, -121.2)

plot + labs(x = NULL,
            y = NULL,
            fill = NULL) +
  guides(size = "none")

ggsave(path=plots,
       filename = "FASTR-stations-all.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=8,
       width=8, 
       dpi="print")
