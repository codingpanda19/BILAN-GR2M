
#---------------------GIS_CATCHEMENTS_RIVERS_RAINGUAGES----------------------------------------------------------

GR<-readOGR(paste('GMB_wat/GMB_water_areas_dcw.shp'))
GR<-spTransform(GR, CRSobj="+init=EPSG:3857")

DB<-readOGR(paste('CATCH_GAMBIA_RIVER/6CATCH_GAMBIA_RIVER.shp'))
DB<- spTransform(DB, CRSobj ="+init=EPSG:3857")

CN<-readOGR(paste('CATCH_GAMBIA_RIVER/6CHANNELS_GAMBIA_RIVER.shp'))
CN<- spTransform(CN, CRSobj ="+init=EPSG:3857")

OUTLETS<-readOGR(paste('CATCH_GAMBIA_RIVER/6OUTLETS_GAMBIA_RIVER.shp'))
OUTLETS<- spTransform(OUTLETS, CRSobj ="+init=EPSG:3857")

RGST<-readOGR(paste('CATCH_GAMBIA_RIVER/7PT_RGST_GAMBIA_RIVER.shp'))
RGST<- spTransform(RGST, CRSobj ="+init=EPSG:3857")

#---------------------------------------OSM VIEW-------------------------------------
library(leaflet)
library(magrittr) 

SNmap<-leaflet() %>%
                    addTiles() %>%
                    setView(-13,14, zoom=5)%>%
                    addMarkers(-13.73, 13.47, popup="SF by Area")
SNmap

#----------------------------------------------------------------------------------

plot(DB, lwd=.7)
plot(GR, lwd=.7, col='blue', add=TRUE)
plot(CN, col='grey',lwd=0.2, add=TRUE)
plot(OUTLETS, col='blue', pch=19,lwd=0.8, add=TRUE)
plot(RGST, col='darkorchid4', pch=19, lwd=0.8, add=TRUE)
