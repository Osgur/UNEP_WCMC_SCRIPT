#install.packages(c("sp", "raster", "rgdal", "rgeos", "gtools", "spdep", "foreign", "stringr", "devtools", "sf", "xlsx", "plyr", "readxl", "knitr", "dplyr", "shiny", "tidyverse"))
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
library(readxl)
library(knitr)
library(dplyr)
library(shiny)
library(tidyverse)
library(png)

Path <- "C:/Users/OsgurM/Downloads/WDPA_MASTER.gdb"
ogrListLayers(Path)


CountryNames <- read.csv("C:/Users/OsgurM/OneDrive - WCMC/00_Data Management/Look_Up_Tables/lu_CountryNames_Jan2018.csv") %>%
  select(c(1:2))

ISO2 <- read_excel("C:/Users/OsgurM/OneDrive - WCMC/00_Data Management/Look_Up_Tables/Regions2ISO3.xlsx")%>%
  select(c(2:3))
#######Read in the data layers######

WDPA_Stats <- read_excel("C:/Users/OsgurM/Downloads/July2019_National_Coverage_Stats.xlsx")

#Public

WDPA_Py <- data.frame(read_sf(Path, layer = "WDPA_poly_Jul2019"))
WDPA_Pt <- data.frame(read_sf(Path, layer = "WDPA_point_Jul2019"))


#Restricted for China
China_NR_Res<- data.frame(st_read(Path, layer = "CHN_restricted_Nov2018_NR"))
China_NNR_Res <- data.frame(st_read(Path, layer = "CHN_restricted_Nov2018_NNR"))

#Restricted for Rusia
Russia_Py_Res <- data.frame(read_sf(Path, layer = "RUS_only_for_public"))
Russia_Pt_Res <- data.frame(read_sf(Path, layer = "RUSpt_only_public_New"))

#Restricted for Saint Helena
SHN_Res <- data.frame(st_read(Path, layer = "SHN_restricted_July2018"))

#Restricted for Estonia
EST_Res <- data.frame(st_read(Path, layer = "EST_restricted_Aug2014_New"))


#Combine all layers
Combined <- bind_rows(WDPA_Py, WDPA_Pt,China_NR_Res, China_NNR_Res, Russia_Py_Res, Russia_Pt_Res, SHN_Res, EST_Res)

Polygons <- bind_rows(WDPA_Py, China_NNR_Res, Russia_Py_Res, SHN_Res, EST_Res) %>%  
  select(WDPAID, ISO3, DESIG_TYPE, VERIF) %>%
  distinct()%>%
  group_by(ISO3) %>%
  count(ISO3) %>% 
  ungroup()%>%
  mutate(ISO3 = strsplit(as.character(ISO3), ";")) %>%
  unnest() %>%
  group_by(ISO3) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE) %>%
  rename(No_Polygons = n)
  

Points <- bind_rows(WDPA_Pt,China_NR_Res, Russia_Pt_Res )%>%  
  select(WDPAID, ISO3, DESIG_TYPE, VERIF) %>%
  distinct()%>%
  group_by(ISO3) %>%
  count(ISO3) %>% 
  ungroup()%>%
  mutate(ISO3 = strsplit(as.character(ISO3), ";")) %>%
  unnest() %>%
  group_by(ISO3) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)%>%
  rename(No_Points = n)

Points_Poly <- merge(Polygons, Points, by = "ISO3" , all = T)

#identify numbers for verification by ISO3
Verified <- Combined %>%
  select(WDPAID, ISO3, DESIG_TYPE, VERIF) %>%
  distinct()%>%
  group_by(ISO3) %>%
  count(VERIF) %>%
  spread(VERIF, n)   


#subset dataset by International and Regional data types
Int_Reg <- subset(Combined, DESIG_TYPE %in% c("International", "Regional"))

Reg_Int_Sum <- Int_Reg %>%
  select(WDPAID, ISO3, DESIG_TYPE, VERIF) %>%
  distinct()%>%
  group_by(ISO3) %>%
  summarise(Int_Reg_Count = n())

#Set NA to 0
Verified[is.na(Verified)] <- 0 

Verified <- merge(Verified, Reg_Int_Sum, by = "ISO3",  all = T)  
Verified$Total <- rowSums(Verified[,2:4])


#Set NA to 0
Verified[is.na(Verified)] <- 0 

#Set the Colnames 
colnames(Verified)[5] <- "Number International or Regional"

#splits transboundary sites and assigns them to all countries
Verified <- Verified %>%
  mutate(ISO3 = strsplit(as.character(ISO3), ";")) %>%
  unnest() %>%
  group_by(ISO3) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)


Verified <- merge(ISOdata, Verified, by = "ISO3", all.y = T)
Verified <- merge(CountryNames, Verified, by = "ISO3")
Verified <- merge(Verified, Points_Poly, by = "ISO3")

Verified <- merge(Verified, WDPA_Stats, by.x = "ISO3", by.y = "iso3", all = T)
Verified <- merge(Verified, ISO2, by.x = "ISO3", by.y = "alpha-3", all = T)

Verified[is.na(Verified)] <- 0

write.csv(Verified, "WDPA_Verified.csv", row.names = F)
