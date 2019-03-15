library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence
library(foreign)
library(stringr)
library(maptools)


EEZFolder <- "C:/Users/OsgurM/Downloads/World_EEZ_v10_20180221"

EEZFiles <- ogrListLayers(EEZFolder)
EEZ <- readOGR(dsn=EEZFolder,layer= EEZFiles[2])


EEZ$ISO_Ter2[2]

if(EEZ$Pol_type == "Joint regime"|"Overlapping claim"){
  
}

for(i in 1:length(EEZ$ISO_Ter1)){
if(!is.na(EEZ$ISO_Ter3[i]) == TRUE){
  EEZ$ISO_Com[i] <- paste(EEZ$ISO_Ter1[i], EEZ$ISO_Ter2[i], EEZ$ISO_Ter3[i], sep = "; ")
}
if(!is.na(EEZ$ISO_Ter2[i]) == TRUE & is.na(EEZ$ISO_Ter3[i]) == TRUE){
    EEZ$ISO_Com[i] <- paste(EEZ$ISO_Ter1[i], EEZ$ISO_Ter2[i], sep = "; ")
  }
}

EEZ$ISO_Ter1 <- as.character(EEZ$ISO_Ter1)

for(j in 1:length(EEZ$ISO_Ter1)){
  if(!is.na(EEZ$ISO_Com[j]) == TRUE){
    EEZ$ISO_Ter1[j] <- EEZ$ISO_Com[j]
  }

}


writeOGR(obj=EEZ, dsn=getwd(), "EEZ_Overlap", overwrite_layer=TRUE,driver="ESRI Shapefile") # this is in geographical projection

