

# Install the necessary library packages
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(gtools)
library(spdep)  # spatial dependence
library(foreign)
library(stringr)
library(devtools)
library(sf)
library(xlsx)
library(plyr)

options(scipen=999)

setwd("//wcmc-data-03/WDPA/2_July_2018_Analyses/Section0_July_WDPA/1_Public_July_WDPA/WDPA_Jul2018_Public.gdb")
ogrListLayers(getwd())

poly <- data.frame(read_sf(getwd(), layer = ogrListLayers(getwd())[grep("poly_Jul2018_Regions_Restricted", ogrListLayers(getwd()))]))
point <- data.frame(read_sf(getwd(), layer = ogrListLayers(getwd())[grep("point_Jul2018_Regions_Restricted", ogrListLayers(getwd()))]))


poly <- poly[,-ncol(poly)]
point <- point[,-ncol(point)]

Region_Identifier <- c("UNEP_Regions", "UNListRegions_18")


Regions <- as.character(unique(poly[,c(Region_Identifier)]))

OUT <- openxlsx::createWorkbook()

for(k in Region_Identifier){
  RegionGroupings <-data.frame()
  
  Regions <- as.character(unique(poly[,c(k)]))
for( i in Regions){
  OUT <- openxlsx::createWorkbook()
  
  RegionPolyData <- poly[which(poly[,k] == i),]
  RegionPolyData$POINT_POLY <- "Polygon"
  RegionPointData <- point[which(point[,k] == i),]
  if(length(RegionPointData[,1])> 0){
  RegionPointData$POINT_POLY <- "Point"}
  
  
  if(length(RegionPointData[,1])> 0){
    RegionData <- rbind.fill(RegionPointData,RegionPolyData)} else{RegionData <- RegionPolyData}
  
  
  openxlsx::addWorksheet(OUT, paste(i))
  openxlsx::writeData(OUT, sheet = paste(i), x = RegionData)


  RegionPolyData <- RegionPolyData[, c("WDPAID", "WDPA_PID", "GIS_AREA")]
  RegionPointData <- RegionPointData[, c("WDPAID", "WDPA_PID", "REP_AREA")]
  
  RegionPolyData_PID <- data.frame(tapply(RegionPolyData$GIS_AREA, RegionPolyData$WDPAID, sum))
  RegionPolyData_PID$WDPAID <- row.names(RegionPolyData_PID)
  colnames(RegionPolyData_PID)[1] <- "AREA"
  
  if(length(RegionPointData[,1])> 0){
  RegionPointData_PID <- data.frame(tapply(RegionPointData$REP_AREA, RegionPointData$WDPAID, sum))
  RegionPointData_PID$WDPAID <- row.names(RegionPointData_PID)
  colnames(RegionPointData_PID)[1] <- "AREA"}
  
  RegionPolyData_PID <- transform(RegionPolyData_PID, group=cut(AREA,  breaks=c(-Inf,1, 10, 100, 1000,10000,Inf),
                                                        labels=c('<1', "1-10", "10-100","100-1000","1000-10000", ">10000")))
  if(length(RegionPointData[,1])> 0){
  RegionPointData_PID <- transform(RegionPointData_PID, group=cut(AREA,  breaks=c(-Inf,1, 10, 100, 1000,10000,Inf),
                                                        labels=c('<1', "1-10", "10-100","100-1000","1000-10000", ">10000")))
  }
  
  RegionGroupings <- rbind(RegionGroupings,t(data.frame(c(i , "poly", nrow(RegionPolyData_PID), as.character(data.frame(table(RegionPolyData_PID$group))[,2]), as.character(data.frame(table(RegionPolyData_PID$group)/nrow(RegionPolyData_PID)*100)[,2])))))
  
  if(length(RegionPointData[,1])> 0){
    RegionGroupings <- rbind(RegionGroupings,t(data.frame(c(i , "point", nrow(RegionPointData_PID), as.character(data.frame(table(RegionPointData_PID$group))[,2]), as.character(data.frame(table(RegionPointData_PID$group)/nrow(RegionPointData_PID)*100)[,2])))))
  }
  }

# Export the file
setwd("C:/Users/OsgurM/Desktop/WDPA_JULY_2018_REGIONS")

colnames(RegionGroupings) <- c("Region","Point_Poly","Count",'<1_Count', "1-10_Count", "10-100_Count","100-1000_Count","1000-10000_Count", ">10000_Count",'<1_%', "1-10_%", "10-100_%","100-1000_%","1000-10000_%", ">10000_%")
#setwd("//wcmc-data-03/WDPA/2_July_2018_Analyses/Section0_July_WDPA/1_Public_July_WDPA/WDPA_Jul2018_Public.gdb")
write.xlsx(RegionGroupings, paste("RegionalProportions_", k,".xlsx", sep = ""), row.names = F)
openxlsx::saveWorkbook(OUT, file = paste("WDPA_JULY_2018_", k, ".xlsx", sep =""), overwrite = T) 
rm(OUT)
}
