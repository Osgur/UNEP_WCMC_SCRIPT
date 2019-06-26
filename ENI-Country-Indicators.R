library(arcgisbinding)
library(dplyr)
library(rgdal)
library(plyr)
library(tidyr)
library(reshape2)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(tidyverse)
library(data.table)
###Layer Preparation
#WDPA - Remove Mab sites and proposed buffer points and join to polygon. Intersect with EEZ_WVS LAyer. Dissolve by DesigType or DesigEng depending on requirement (filter out data required first).  Calculate area using mollweide world. 

###KBA intersect with EEZ_WVS layer and filter out by only confirmed for KBA IBA and AZE. Dissolve and calculate area by mollweide world. 

#AZE filter KBA

#Critical Habitat - Join Crit Hab to rasterised version of EEZ_WVS. Set pixel size so equals 1Km2 

#Set configuration
arc.check_product()
options(scipen=999)
#setwd to your folder containing the LAyers shapfiles or file geodatabase
setwd("filepath")
ogrListLayers(getwd())

#REad in the EEZ_WVS layer that has already had area calculated using mollweide world
EEZ <- arc.open("C:/Users/OsgurM/Documents/ArcGIS/Projects/ENI-Analysis/ENI-Analysis.gdb/Country_Boundaries_EEZ_ABNJ") %>%
  arc.select(fields = "*") 

##Read in WDPA dissolved using DESIG_ENG to identify ramsar and WHS sites
DESIG.arc <- arc.open("C:/Users/OsgurM/Documents/ArcGIS/Projects/ENI-Analysis/ENI-Analysis.gdb/WDPA_DESIG") %>%
  arc.select(fields = "*")%>% # read in the dataset
  dplyr::select(-OBJECTID)%>% # remove column
  dplyr::rename(WDPA_AREA = AREA) %>% # rename column
  group_by(ISO3, type)%>% # group by grouping columns 
  left_join(EEZ[,2:4],by= c("type", "ISO3"))%>% # join to EEZ layer to get total area
  unite("DESIG_AREA", c(DESIG_ENG, type)) # join columns for easier casting
##
DESIG_Out <- dcast(setDT(DESIG.arc), ISO3 ~DESIG_AREA, value.var = c("WDPA_AREA", "AREA_KM2")) # cast data into readable dataframe

##Read in WDPA dissolved for DESIG Type
DESIG_TYPE.arc <- arc.open("C:/Users/OsgurM/Documents/ArcGIS/Projects/ENI-Analysis/ENI-Analysis.gdb/WDPA_DESIGTYPE") %>%
  arc.select(fields = "*")%>%
  dplyr::select(-OBJECTID)%>%
  dplyr::rename(WDPA_AREA = AREA) %>%
  group_by(ISO3, type)%>%
  left_join(EEZ[,2:4],by= c("type", "ISO3"))%>%
  unite("DESIGType_AREA", c(DESIG_TYPE, type)) 


DESIG_TYPE_Out <- dcast(setDT(DESIG_TYPE.arc), ISO3 ~DESIGType_AREA, value.var = c("WDPA_AREA", "AREA_KM2"))

#Read in AZE dataset subset from KBA
AZE.arc <- arc.open("C:/Users/OsgurM/Documents/ArcGIS/Projects/ENI-Analysis/ENI-Analysis.gdb/AZE_Confirmed_DISS") %>%
  arc.select(fields = "*")%>%
  dplyr::select(-OBJECTID)%>%
  dplyr::rename(ISO3 = ISO3_1) %>%
  dplyr::rename(KBA_AREA = AREA) %>%
  group_by(ISO3, type)%>%
  left_join(EEZ[,2:4],by= c("type", "ISO3"))

AZE_Out <-  dcast(setDT(AZE.arc), ISO3 ~type, value.var = c("KBA_AREA", "AREA_KM2"))

#REad in KBA dataset
KBA.arc <- arc.open("C:/Users/OsgurM/Documents/ArcGIS/Projects/ENI-Analysis/ENI-Analysis.gdb/Kba_Confirmed_DISS") %>%
  arc.select(fields = "*")%>%
  dplyr::select(-OBJECTID)%>%
  dplyr::rename(ISO3 = ISO3_1) %>%
  dplyr::rename(KBA_AREA = AREA) %>%
  group_by(ISO3, type)%>%
  left_join(EEZ[,2:4],by= c("type", "ISO3"))

 KBA_Out <-  dcast(setDT(KBA.arc), ISO3 ~type, value.var = c("KBA_AREA", "AREA_KM2"))

 #read in Critical HAbitat LAyer
CH <- read.csv("C:/Users/OsgurM/Documents/ArcGIS/Projects/ENI-Analysis/Output/CH_Table.csv") %>%
  dplyr::select(-c(1, 2))%>%
  filter(CRIT_HAB != "UNCLASSIFIED")%>%
  left_join(EEZ[,2:4], by = c("type", "ISO3") )%>%
  unite("Type_Crit", c(type, CRIT_HAB)) 

CH_OUT <- dcast(setDT(CH), ISO3 ~ Type_Crit, value.var = c("Count", "AREA_KM2"))

#Output the Layers
setwd("~/ArcGIS/Projects/ENI-Analysis/Output")
write.csv(AZE_Out, "AZE_ENI.csv", row.names = F)
write.csv(KBA_Out, "KBA_ENI.csv", row.names = F)
write.csv(DESIG_Out, "WDPA_DESIG_ENI.csv", row.names = F)
write.csv(DESIG_TYPE_Out, "WDPA_DESIG_TYPE_ENI.csv", row.names = F)
write.csv(CH_OUT, "CritHab_ENI.csv", row.names = F)



