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
library(dplyr)

arc.check_product()
options(scipen = 999)

setwd("~/ArcGIS/Projects/Ocean+/Ocean+Stats.gdb")
HabitatLayers <- ogrListLayers("~/ArcGIS/Projects/Ocean+/Ocean+Stats.gdb") 
HabitatLayers <- HabitatLayers[c(grep("INT_DIS", HabitatLayers))]

HabitatProtectedLayers <- ogrListLayers("~/ArcGIS/Projects/Ocean+/Ocean+Stats.gdb") 

  HabitatProtectedLayers <- HabitatProtectedLayers[c(grep("Country", HabitatProtectedLayers))]


Seagrass <- arc.open(paste0(getwd(),"/", HabitatLayers[1]))%>%
  arc.select(c("ISO3","SUM_Shape_area"))
colnames(Seagrass)[2] <- "Total_Area(Km2)"
SeagrassProtected <- arc.open(paste0(getwd(),"/", HabitatProtectedLayers[1]))%>%
  arc.select(c("ISO3","SUM_AREA_KM2"))
colnames(SeagrassProtected)[2] <- "Protected_Area(Km2)"

SeagrassOut <- merge(Seagrass, SeagrassProtected, by = "ISO3")
SeagrassOut$PercentProtected <- SeagrassOut$`Protected_Area(Km2)`/SeagrassOut$`Total_Area(Km2)`*100


#ColdCoral
ColdCoral <- arc.open(paste0("C:/Users/OsgurM/Documents/ArcGIS/Projects/Ocean+/Ocean+Stats.gdb/WCMC001_ColdCorals2017_Py_v5_ISO_INT"))%>%
  arc.select("ISO3_1") %>%
  group_by(ISO3_1) %>%
  summarise(n())

colnames(ColdCoral) <- c("ISO3","Total_Points")
ColdCoralProtected <- arc.open("C:/Users/OsgurM/Documents/ArcGIS/Projects/Ocean+/Ocean+Stats.gdb/WCMC001_ColdCorals2017_Pt_Country")%>%
  arc.select(c("ISO3"))%>%
  group_by(ISO3)%>%
  summarise(n())

colnames(ColdCoralProtected)[2] <- "Total_Protected"

ColdCoralOut <- merge(ColdCoral, ColdCoralProtected, by = "ISO3", all = TRUE)
ColdCoralOut$PercentProtected <- ColdCoralOut$Total_Protected/ColdCoralOut$Total_Points*100

#CoralReef
CoralReef <- arc.open(paste0(getwd(),"/", HabitatLayers[3]))%>%
  arc.select(c("ISO3","SUM_Shape_area"))
colnames(CoralReef)[2] <- "Total_Area(Km2)"
CoralReefProtected <- arc.open(paste0(getwd(),"/", HabitatProtectedLayers[3]))%>%
  arc.select(c("ISO3","SUM_AREA_KM2"))
colnames(CoralReefProtected)[2] <- "Protected_Area(Km2)"

CoralReefOut <- merge(CoralReef, CoralReefProtected, by = "ISO3")
CoralReefOut$PercentProtected <- CoralReefOut$`Protected_Area(Km2)`/CoralReefOut$`Total_Area(Km2)`*100

#Mangrove
Mangrove <- arc.open(paste0(getwd(),"/", HabitatLayers[4]))%>%
  arc.select(c("ISO3","SUM_Shape_area"))
colnames(Mangrove)[2] <- "Total_Area(Km2)"
MangroveProtected <- arc.open(paste0(getwd(),"/", HabitatProtectedLayers[4]))%>%
  arc.select(c("ISO3","SUM_AREA_KM2"))
colnames(MangroveProtected)[2] <- "Protected_Area(Km2)"

MangroveOut <- merge(Mangrove, MangroveProtected, by = "ISO3")
MangroveOut$PercentProtected <- MangroveOut$`Protected_Area(Km2)`/MangroveOut$`Total_Area(Km2)`*100


#Saltmarsh
Saltmarsh <- arc.open(paste0(getwd(),"/", HabitatLayers[5]))%>%
  arc.select(c("ISO3","SUM_Shape_area"))
colnames(Saltmarsh)[2] <- "Total_Area(Km2)"
SaltmarshProtected <- arc.open(paste0(getwd(),"/", HabitatProtectedLayers[5]))%>%
  arc.select(c("ISO3","SUM_AREA_KM2"))
colnames(SaltmarshProtected)[2] <- "Protected_Area(Km2)"

SaltmarshOut <- merge(Saltmarsh, SaltmarshProtected, by = "ISO3")
SaltmarshOut$PercentProtected <- SaltmarshOut$`Protected_Area(Km2)`/SaltmarshOut$`Total_Area(Km2)`*100

setwd("C:/Users/OsgurM/Documents/Ocean+StatsCsvs")

write.csv(SeagrassOut, "Seagrass_Output.csv", row.names = F)
write.csv(CoralReefOut, "CoralReef_Output.csv", row.names = F)
write.csv(ColdCoralOut, "ColdCoral_Output.csv", row.names = F)
write.csv(MangroveOut, "Mangrove_Output.csv", row.names = F)
write.csv(Saltmarsh, "Saltmarsh_Output.csv", row.names = F)

