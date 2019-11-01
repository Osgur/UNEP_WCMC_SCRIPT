library(arcgisbinding)
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence
library(foreign)
library(stringr)
library(maptools)
library(sf)

arc.check_product()
options(scipen = 999)

setwd("~/ArcGIS/Projects/Ocean+/Mangrove_Change.gdb")
MangroveLayers <- ogrListLayers("~/ArcGIS/Projects/Ocean+/Mangrove_Change.gdb") 
MangroveLayers <- MangroveLayers[c(grep("DIS", MangroveLayers))]

Change <- arc.open(paste0(getwd(),"/", MangroveLayers[1]))%>%
  arc.select(c("ISO3","Sum_Area"))
colnames(Change)[2] <- "Mangrove_Area(Km2)_1996"

for( i in 2:length(MangroveLayers)){
  MangroveYear <- arc.open(paste0(getwd(),"/", MangroveLayers[i]))%>%
    arc.select(c("ISO3","Sum_Area"))
  
  Year <- sub("_v2_DIS","",sub("GMW_","",MangroveLayers[i]))
  name <- paste0("Mangrove_Area(Km2)_", Year)
  colnames(MangroveYear)[2] <- name
  
  Change <- merge(Change,MangroveYear, by = "ISO3", all = T)

}


setwd("C:/Users/OsgurM/Documents/Ocean+StatsCsvs")
write.csv(Change, "Mangrove_Change.csv", row.names = F)
