library(arcgisbinding)

arc.check_product()
library(dplyr)
library(readxl)
library(ISOcodes)


setwd("~/ArcGIS/Projects/NE_EEZ/BaseData.gdb")

PackageList <- ISO_3166_2 


ISOList <- read_excel("C:/Users/osgurm/OneDrive - WCMC/00_Data Management/Look_Up_Tables/ISO_CountryNames_Oct2019.xlsx")
ParentRel <- read_excel("C:/Users/OsgurM/OneDrive - WCMC/00_Data Management/Look_Up_Tables/lu_ParentISO3_ISO3_relationship.xlsx")

NE <- arc.open(paste0(getwd(), "/", "ne_10m_admin_1_states_provinces_updated")) %>%
  arc.select("*")

EEZ <- arc.open(paste0(getwd(), "/", "eez_v10_updated")) %>%
  arc.select("*")

NE_ISO3 <- unique(NE$ISO3)
EEZ_ISO3 <- unique(EEZ$ISO_Ter1)

paste(NE_ISO3[NE_ISO3 %in% EEZ_ISO3], collapse = "';'")


Wrong <- sort(NE_ISO3[!NE_ISO3 %in% ISOList$`Alpha-3 code`])
EEZWrong <- sort(EEZ_ISO3[!EEZ_ISO3 %in% ISOList$`Alpha-3 code`])

NoEEZ <- sort(NE_ISO3[!NE_ISO3 %in% EEZ$ISO_Ter1])
paste(Wrong, collapse = ";")
paste(EEZWrong, collapse = ";")

NoEEZ[NoEEZ %in% ParentRel$ISO3]


paste(sort(NE_ISO3[!NE_ISO3 %in% NoEEZ]), collapse = "';'")

