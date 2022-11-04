library(data.table)
library(dplyr)
library(sf)
library(magrittr)
library(ggshadow) #geom_glowpoint functionality
library(ggplot2)

##data is from https://data.cityofnewyork.us/Environment/Forestry-Tree-Points/k5ta-2trh , save as csv to your computer

trees <- fread("path_to_csv")
boros <- st_read("boroughs.shp") #nyc boroughs outline

### remove null lat/long from crashes table
trees <- trees[ !(trees$Geometry =="") & !(trees$Geometry =="GEOMETRYCOLLECTION EMPTY"),]


###separate out "location" column in to lon and lat, remove parenthese
trees_sf <- trees %>%
  dplyr::mutate( 
    # replace text and parenthesis
    Location = gsub("[()]", "", Location)
  ) %>%
  # separate into lat and lon columns
  tidyr::separate(Location, into=c('lon', 'lat'), sep=', ') %>%
  # convert to sf point object 
  sf::st_as_sf(coords = c('lat', 'lon'), crs=4326) #%>%
  

##create ggplot
forests <- ggplot() +
  coord_sf(xlim = c(-74.25559, -73.70001), ylim = c(40.49612, 40.91553), expand = FALSE)+
  geom_sf(data=boros, size=.5, fill="black")+
  geom_glowpoint(data = trees_sf, aes(geometry=geometry),  alpha = .01,
          size=.001,
          color = "#6bb857",
          shadowcolour = "#6bb857",
          shadowalpha = .001,
          stat = "sf_coordinates",
          show.legend = FALSE)+
  theme_void()+
  labs(title = "NYC Forestry Tree Points",
       caption = "Source: NYC Parks and Recreation on NYC Open Data \n https://data.cityofnewyork.us/Environment/Forestry-Tree-Points/k5ta-2trh")+
  theme(plot.title=element_text(hjust=0.5))

###png export
ggsave(
  "nyc_forestry.png",
  plot = forests,
  width = 8.5,
  height = 11,
  units = c("in"),
  dpi = 600,
  bg = "white"
)
          
