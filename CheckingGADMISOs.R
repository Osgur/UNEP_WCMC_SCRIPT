library(arcgisbinding)
arc.check_product()
library(dplyr)
library(readxl)
library(ISOcodes)


setwd("~/ArcGIS/Projects/GADM_EEZ/BaseData.gdb")

PackageList <- ISO_3166_2 


ISOList <- read_excel("C:/Users/osgurm/OneDrive - WCMC/00_Data Management/Look_Up_Tables/ISO_CountryNames_Oct2019.xlsx")
ParentRel <- read_excel("C:/Users/OsgurM/OneDrive - WCMC/00_Data Management/Look_Up_Tables/lu_ParentISO3_ISO3_relationship.xlsx")

Gadm <- arc.open(paste0(getwd(), "/", "gadm36")) %>%
  arc.select("*")

EEZ <- arc.open(paste0(getwd(), "/", "eez_v10_updated")) %>%
  arc.select("*")

Gadm_ISO3 <- unique(Gadm$ISO3)
EEZ_ISO3 <- unique(EEZ$ISO_Ter1)

paste(Gadm_ISO3[Gadm_ISO3 %in% EEZ_ISO3], collapse = "';'")


Wrong <- sort(Gadm_ISO3[!Gadm_ISO3 %in% ISOList$`Alpha-3 code`])
EEZWrong <- sort(EEZ_ISO3[!EEZ_ISO3 %in% ISOList$`Alpha-3 code`])

NoEEZ <- sort(Gadm_ISO3[!Gadm_ISO3 %in% EEZ$ISO_Ter1])
paste(Wrong, collapse = ";")
paste(EEZWrong, collapse = ";")

NoEEZ[NoEEZ %in% ParentRel$ISO3]


paste(sort(Gadm_ISO3[!Gadm_ISO3 %in% NoEEZ]), collapse = "';'")
