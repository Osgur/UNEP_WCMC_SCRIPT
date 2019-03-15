

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
library(dplyr)

options(scipen=999)

setwd("//wcmc-data-03/WDPA/2_July_2018_Analyses/Section1_Global_Analyses/1_Data/WDPA_with_Regions_2018.gdb")
ogrListLayers(getwd())

poly <- data.frame(read_sf(getwd(), layer = "WDPA_poly_Jul2018_Regions_Restricted"))
point <- data.frame(read_sf(getwd(), layer = "WDPA_point_Jul2018_Regions_Restricted"))


WDPA <- rbind.fill(poly, point)

WDPA <- WDPA[!WDPA$STATUS %in% c("Not Reported", "Proposed"),]


WDPA_Restricted <- WDPA %>%
  group_by(WDPAID) %>%
  slice(which.max(REP_AREA))


##IUCN Categories

RegionIUCNCatSum <- setNames(data.frame(matrix(ncol = 12, nrow = 0)), c("Region",unique(WDPA_Restricted$IUCN_CAT), "Total"))
for(j in unique(WDPA_Restricted$UNListRegions_18)){
  PSub <- subset(WDPA_Restricted, WDPA_Restricted$UNListRegions_18 == j)
  sumPoly <- data.frame(table(PSub$IUCN_CAT))
  sumPolyTr <- cbind(as.character(j), data.frame(t(sumPoly[,2])),nrow(PSub))
  colnames(sumPolyTr) <- c("Region", as.character(sumPoly[,1]),"Total")
  sumPolyTr$Region <- as.character(sumPolyTr$Region)
  RegionIUCNCatSum <- rbind.fill(RegionIUCNCatSum,sumPolyTr)
  RegionIUCNCatSum <- RegionIUCNCatSum[order(RegionIUCNCatSum$Region),]
}

#Governance Types

Governance_by_government <- c("Government-delegated management", "Federal or national ministry or agency", "Sub-national ministry or agency")
Shared_governance <- c("Transboundary governance", "Collaborative governance", "Joint governance" )
Private_governance <- c("Individual landowners", "Non-profit organisations", "For-profit organisations")
Governance_by_indigenous_peoples_and_local_communities <- c("Indigenous peoples", "Local communities")

  RegionGovSum <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("Region","Governance by government", "Shared governance", "Private governance", "Governance by indigenous peoples and local communities", "Not Reported" , "Total"))
for(j in unique(WDPA_Restricted$UNListRegions_18)){
  PSub <- subset(WDPA_Restricted, WDPA_Restricted$UNListRegions_18 == j)
  
  Governance_by_government_Out <- sum(data.frame(table(PSub$GOV_TYPE))[which(data.frame(table(PSub$GOV_TYPE))[,1] %in% Governance_by_government), 2])
  Shared_governance_Out <- sum(data.frame(table(PSub$GOV_TYPE))[which(data.frame(table(PSub$GOV_TYPE))[,1] %in% Shared_governance), 2])
  Private_governance_Out <- sum(data.frame(table(PSub$GOV_TYPE))[which(data.frame(table(PSub$GOV_TYPE))[,1] %in% Private_governance), 2])
  Governance_by_indigenous_peoples_and_local_communities_Out <- sum(data.frame(table(PSub$GOV_TYPE))[which(data.frame(table(PSub$GOV_TYPE))[,1] %in% Governance_by_indigenous_peoples_and_local_communities), 2])
  Not_Reported_Out <- sum(data.frame(table(PSub$GOV_TYPE))[which(data.frame(table(PSub$GOV_TYPE))[,1] %in% "Not Reported"), 2])
  
  
  sumPolyTr <- data.frame(cbind(as.character(j),Governance_by_government_Out, Shared_governance_Out, Private_governance_Out, Governance_by_indigenous_peoples_and_local_communities_Out, Not_Reported_Out,nrow(PSub)))
  colnames(sumPolyTr) <- c("Region","Governance by government", "Shared governance", "Private governance", "Governance by indigenous peoples and local communities" , "Not Reported", "Total")
  sumPolyTr$Region <- as.character(sumPolyTr$Region)
  RegionGovSum <- rbind(RegionGovSum,sumPolyTr)
  }


setwd("C:/Users/OsgurM/Desktop/WDPA_JULY_2018_REGIONS")
write.csv(RegionIUCNCatSum, "UNListRegions2018_IUCN_Cat.csv", row.names = F)
write.csv(RegionGovSum, "UNListRegions2018_Gov_Type.csv", row.names = F)
