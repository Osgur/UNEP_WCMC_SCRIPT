

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
library(tidyverse)

options(scipen=999)

setwd("//wcmc-data-03/WDPA/2_July_2018_Analyses/Section1_Global_Analyses/1_Data/WDPA_with_Regions_2018.gdb")
ogrListLayers(getwd())

poly <- data.frame(read_sf(getwd(), layer = "WDPA_poly_Jul2018_Regions_Restricted"))
point <- data.frame(read_sf(getwd(), layer = "WDPA_point_Jul2018_Regions_Restricted"))


WDPA_Restricted <- rbind.fill(poly, point)
WDPA_Restricted <- WDPA_Restricted[!WDPA_Restricted$STATUS %in% c("Not Reported", "Proposed"),]


WDPA_Restricted <- WDPA_Restricted %>%
  group_by(WDPAID) %>%
  summarise(REP_AREA = sum(REP_AREA), 
            UNListRegions_18 = first(UNListRegions_18), 
            MARINE = first(MARINE), 
            UNEP_Regions = first(UNEP_Regions))


RegionGroupings <- data.frame()
Regions <- unique(WDPA_Restricted$UNListRegions_18)

for( i in Regions){

  RegionData <- WDPA_Restricted[which(WDPA_Restricted[,"UNListRegions_18"] == i),]

  RegionData <- RegionData[, c("WDPAID", "REP_AREA")]

  RegionData$WDPAID <- row.names(RegionData)
  colnames(RegionData)[2] <- "AREA"
  
  
  RegionData <- transform(RegionData, group=cut(AREA,  breaks=c(-Inf,0.000001, 1, 10, 100, 1000,10000,Inf),
                                                        labels=c('0',"<1", "1-10", "10-100","100-1000","1000-10000", ">10000")))

  RegionGroupings <- rbind(RegionGroupings,t(data.frame(c(i , nrow(RegionData), as.character(data.frame(table(RegionData$group))[,2]), as.character(data.frame(table(RegionData$group)/nrow(RegionData)*100)[,2])))))
  
}
  
  
  Counts <- data.frame()
  
  for(i in Regions){
    RegionData <- WDPA_Restricted[which(WDPA_Restricted[,"UNListRegions_18"] == i),]
    Terrestrial_PAs <- sum(table(RegionData$MARINE)[c(which(names(table(RegionData$MARINE)) %in% c(0,1)))])
    Marine_PAs <- sum(table(RegionData$MARINE)[c(which(names(table(RegionData$MARINE)) %in% c(2)))])
    
    out <- data.frame(cbind(i, Terrestrial_PAs, Marine_PAs))
    Counts <- rbind(Counts, out)
    
  }
  
  Counts_2 <- data.frame()
  Regions <- unique(WDPA_Restricted$UNEP_Regions)
  
  for(i in Regions){
    RegionData <- WDPA_Restricted[which(WDPA_Restricted[,"UNEP_Regions"] == i),]
    Terrestrial_PAs <- sum(table(RegionData$MARINE)[c(which(names(table(RegionData$MARINE)) %in% c(0,1)))])
    Marine_PAs <- sum(table(RegionData$MARINE)[c(which(names(table(RegionData$MARINE)) %in% c(2)))])
    
    out <- data.frame(cbind(i, Terrestrial_PAs, Marine_PAs))
    Counts_2 <- rbind(Counts_2, out)
    
  }
  

# Export the file
setwd("C:/Users/OsgurM/Desktop/WDPA_JULY_2018_REGIONS")

colnames(RegionGroupings) <- c("Region","Total","0",'<1_Count', "1-10_Count", "10-100_Count","100-1000_Count","1000-10000_Count", ">10000_Count","0_%",'<1_%', "1-10_%", "10-100_%","100-1000_%","1000-10000_%", ">10000_%")
colnames(Counts) <- c("Region", "Number of terrestrial and coastal PAs", "Number of Marine PAs")
colnames(Counts_2) <- c("Region", "Number of terrestrial and coastal PAs", "Number of Marine PAs")

RegionGroupings <- RegionGroupings[order(as.character(RegionGroupings$Region)),]

#setwd("//wcmc-data-03/WDPA/2_July_2018_Analyses/Section0_July_WDPA/1_Public_July_WDPA/WDPA_Jul2018_Public.gdb")
write.csv(RegionGroupings, "UNListRegions2018_SizeClasses.csv", row.names = F)
write.csv(Counts, "UNListRegions2018_Count.csv", row.names = F)
write.csv(Counts_2, "UNEP_Regions2018_Count.csv", row.names = F)



