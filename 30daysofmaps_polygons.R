library(data.table)
library(sf)
library(showtext)
##Highway Gothic font https://www.cufonfonts.com/font/highway-gothic#google_vignette
font_add(family = "Highway Gothic", regular = "path_to_ttf" )
library(ggplot2)
library(dplyr)

###import all land use data from NJ DEP
landuse <- st_read("filepath_to_shp")

###get all land use data that isn't water, rename column
combined <- landuse[landuse$TYPE15 != "WATER",]
combined_sf <- combined %>% rename(`Land Use` = TYPE15)


##decide hex codes for land use
##forest: #316C5D
##agriculture: "#80ACA1"
## wetlands: #296791
## barren land: #A2A2A2 


###create ggplot
showtext_auto() ##needed for imported fonts
nj_map <- ggplot()+
  geom_sf(data=combined_sf, aes(fill=`Land Use`), lwd=0, color=NA)+
  scale_fill_manual(values = c("#80ACA1","#A2A2A2","#316C5D", "#B7C71C", "#296791"))+
  labs(title = "Land Use in the Garden State",
       caption = "Source: NJDEP Bureau of GIS \n https://www.arcgis.com/home/item.html?id=6f76b90deda34cc98aec255e2defdb45")+
  theme_void()+
  theme(plot.title=element_text(size = 28)
        ,text=element_text(family="Highway Gothic")
        )

###png export
showtext_auto(enable = FALSE) ###needed for imported fonts
ggsave(
  "nj_landuse.png",
  plot = nj_map,
  width = 7,
  height = 11,
  units = c("in"),
  dpi = 600,
  bg = "white"
)

##pdf export
showtext_auto(enable = FALSE)
ggsave(
  "nj_landuse.pdf",
  plot = nj_map,
  device = cairo_pdf,
  width = 8.5,
  height = 11,
  units = c("in"),
  dpi = 600,
  bg = "white"
)




