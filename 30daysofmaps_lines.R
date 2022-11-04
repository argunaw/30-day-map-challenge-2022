library(data.table)
library(dplyr)
library(sf)
library(magrittr)
library(ggplot2)
library(ggspatial)
library(raster)

##data source: https://geodata.bts.gov/datasets/usdot::north-american-rail-network-lines/about
freight <- st_read("filepath_to_file")

##what are the top 10 railroad owners?
x <- as.data.table(table(freight$RROWNER1))

###codes taken from https://info.ornl.gov/sites/publications/Files/Pub111143.pdf
##KCS= Kansas City Railroad
###BNSF=BNSF
##UP= Union Pacific
##CN=Canada National Railway
##CPRS=Canada Pacific Railway
##CSXT=CSX Transportation
##NS=Norfolk Southern Railway
##AMTK=Amtrak
##USG= United States Gypsum
##CASO=Conrail
##PVTX= Privately Owned Railroad


###get the top 5 by track mileage, 10 was too many for this map
lens <- as.data.table(aggregate(freight$MILES, by=list(Category=freight$RROWNER1), FUN=sum))

###create new column with complete names
freight_sf <- freight %>%
  mutate(`Railroad Company` = case_when(
    RROWNER1=="BNSF"  ~ "BNSF Railway",
    RROWNER1=="UP"  ~ "Union Pacific Railroad",
    RROWNER1=="CN"  ~ "Canada National Railway",
    RROWNER1=="CSXT"  ~ "CSX Transportation",
    RROWNER1=="NS"  ~ "Norfolk Southern Railway",
    RROWNER1=="AMTK"  ~ "Amtrak",
    TRUE ~ "Other" 
    )
    )


rail <- ggplot()+
  annotation_map_tile("cartodark")+
  geom_sf(data=freight_sf, aes(geometry=geometry, color=`Railroad Company`), size=.2)+
  scale_colour_brewer(palette = "Set1")+
  theme_void()+
  labs(title = "North American Rail Lines",
       caption = "Source: United States DOT \n https://geodata.bts.gov/datasets/usdot::north-american-rail-network-lines/about")+
  theme(plot.title=element_text(hjust=0.5))

###below image was .2 line size
ggsave(
  "us_rail.png",
  plot = rail,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  bg = "white"
)

##below pdf was .01 line size
ggsave(
  "us_rail.pdf",
  plot = rail,
  width = 9,
  height = 6,
  units = c("in"),
  dpi = 600,
  bg = "white"
)
