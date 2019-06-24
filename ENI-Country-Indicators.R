library(rgdal)
library(dplyr)
library(sf)
library(arcgisbinding)
library(plyr)
library(raster)
library(rgeos)
library(ggplot2)
library(lwgeom)


arc.check_product()

Land.arc <- arc.open("C:/Users/OsgurM/Downloads/gadm36_shp/gadm36.shp")
EEZ.arc <- arc.open("C:/Users/OsgurM/Downloads/World_EEZ_v10_20180221/eez_v10.shp")
KBA.arc <-  arc.open("C:/Users/OsgurM/Documents/ArcGIS/Projects/Ocean+/Ocean+.gdb/KbaMapGlobal_POL_CopyFeatures")
CritHab <- stack("C:/Users/OsgurM/Downloads/WCMC_043_GlobalCH_IFCPS6_2017/DataPack_WCMC_043_GlobalCH_IFCPS6_2017/01_Data/crhab_mt.ovr")
WDPA.arc <- arc.open("C:/Users/OsgurM/Documents/ArcGIS/Projects/Ocean+/Ocean+.gdb/WDPA_poly_Jun2019_CopyFeatures")


ISO3 <- Land.arc %>%
  arc.select(fields = "GID_0") %>%
  distinct(GID_0) %>% 
  filter( !grepl("/",GID_0))

ISO3 <-"IRL"
for(i in ISO3){

Landsql <- paste0("GID_0 LIKE '%",i,"%'")
EEZsql <- paste0("ISO_Ter1 LIKE '%",i,"%'")
EEZComsql <- paste0("ISO_Ter1 LIKE '%",i,"%'")

Landsub <- Land.arc %>%
  arc.select(fields = "GID_0", where_clause = Landsql) %>%
  arc.data2sf() %>%
  st_cast("POLYGON")%>%
  st_transform("+proj=moll") %>%
  st_buffer(0) %>%
  st_union()

EEZsub <- EEZ.arc %>%
  arc.select(fields = "*", where_clause = EEZsql) %>%
  arc.data2sf() %>%
  st_cast("POLYGON")%>%
  st_transform("+proj=moll")




# WDPAsqlLand <- paste0("ISO3 LIKE '%",i,"%'")
# WDPAsqlEEZ <- paste0("ISO3 LIKE '%",i,"%'")
# 
# WDPAsubLand <- WDPA.arc %>%
#   arc.select(fields = "*", where_clause = WDPAsqlLand) %>%
#   filter(STATUS %in% c("Designated", "Inscribed", "Established"))%>%
#   arc.data2sf() %>%
#   st_cast("POLYGON") %>%
#   st_transform("+proj=moll") %>%
#   st_make_valid()%>%
#   st_intersection(Landsub)%>%
#   st_union()
# 
# WDPAsubLandArea <- st_area(WDPAsubLand)/1000000
# 
# WDPAsubEEZ <- WDPA.arc %>%
#   arc.select(fields = "*", where_clause = WDPAsqlEEZ) %>%
#   filter(STATUS %in% c("Designated", "Inscribed", "Established"))%>%
#   arc.data2sf() %>%
#   st_cast("POLYGON") %>%
#   st_transform("+proj=moll") %>%
#   st_buffer(0)%>%
#   st_intersection(Landsub)%>%
#   st_union()
# 
# 
# WDPAsubEEZArea <- st_area(WDPAsubEEZ)/1000000

####AZE
AZEsql <-  paste0("ISO3 LIKE '%",i,"%'", " AND AzeStatus = 'confirmed'")
AZEsubEEZ <- KBA.arc %>%
  arc.select(fields = "*", where_clause = AZEsql)
AZEsubEEZ <- AZEsubEEZ %>%
  {if (i %in% AZEsubEEZ$ISO3) 
  arc.data2sf() %>%
  st_cast("POLYGON") %>%
  st_transform("+proj=moll") %>%
  st_buffer(0)%>%
  st_intersection(EEZsub)%>%
  st_union()}

if(length(AZEsubEEZ) > 0){AZEsubEEZArea <- st_area(AZEsubEEZ)/1000000}

AZEsubLand <- KBA.arc %>%
  arc.select(fields = "*", where_clause = AZEsql)
AZEsubLand <- AZEsubLand %>%
  {if (i %in% AZEsubLand$ISO3) 
  arc.data2sf() %>%
  st_cast("POLYGON") %>%
  st_transform("+proj=moll") %>%
  st_buffer(0)%>%
  st_intersection(Landsub)%>%
  st_union()}

if(length(AZEsubLand) > 0){AZEsubLandArea <- st_area(AZEsubLand)/1000000}

####KBA 
KBAsql <- paste0("ISO3 LIKE '%",i,"%'")
KBAsubEEZ <- KBA.arc %>%
  arc.select(fields = "ISO3", where_clause = KBAsql) 
KBAsubEEZ <- KBAsubEEZ %>%
  arc.data2sf()%>%
  st_cast("POLYGON") %>%
  st_transform("+proj=moll") %>%
  st_buffer(0)%>%
  st_intersection(EEZsub)%>%
  st_union()

if(length(KBAsubEEZ) > 0){KBAsubEEZArea <- st_area(KBAsubEEZ)/1000000}

KBAsubLand <- KBA.arc %>%
  arc.select(fields = "ISO3", where_clause = KBAsql) 
KBAsubLand <- KBAsubLand %>%
  arc.data2sf()%>%
  st_cast("POLYGON") %>%
  st_transform("+proj=moll") %>%
  st_buffer(0)%>%
  st_intersection(Landsub)%>%
  st_union()
  
  if(length(KBAsubLand) > 0){KBAsubLandArea <- st_area(KBAsubLand)/1000000}
}


