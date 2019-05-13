library(sf)
library(rgdal)
#library(sp)
### Set the working directory to the File Geodatabase where the Licensed version is stored
setwd("C:/Users/osgurm/Downloads/WDPA_Apr2019_Licensed/WDPA_Apr2019_Licensed/WDPA_Apr2019_Licensed.gdb")

### Identify the layers in the file Geodatabase
ogrListLayers(getwd())

# Read in the Licensed version of WDPA Poly & Point
licensed_poly <- read_sf(getwd(),  "WDPA_poly_Apr2019")
licensed_point <- read_sf(getwd(),  "WDPA_point_Apr2019")

# identify unique WDPA_ids
WDPAIDs <-unique(licensed_poly$WDPAID,licensed_point$WDPAID)

# use a loop to create a kml for each wdpa_id
for (i in WDPAIDs){
  
  # subset the point and polygon layer by individual PAs ("i") identified in the unique WDPAID
  Sub_Poly <- licensed_poly %>%
    subset( WDPAID %in% i)
  
  Sub_Point <- licensed_point %>%
    subset( WDPAID %in% i)
  
  #set the working directory to a location you want the save the files to
  setwd("C:/Users/osgurm/Downloads/WDPAID_KML")
  #write the point and polygon kmls to the folder
  if(nrow(Sub_Poly) > 0 ){
    st_write(Sub_Poly, paste0("WDPA_poly_Apr2019_", i, ".kml"), driver='kml', update=TRUE)}
  if(nrow(Sub_Point) > 0) {
    st_write(Sub_Point, paste0("WDPA_point_Apr2019_", i, ".kml"), driver='kml', update=TRUE)}
  
}


# Identify the unique ISO values in the WDPA across both point and poly and accounting for countries that could only appear in transboundary sites
Countries <- unique(unique(unlist(strsplit(licensed_poly$ISO3, ";"))), unique(unlist(strsplit(licensed_point$ISO3, ";"))))
Countries <- sort(Countries)   

# for loop which supbsets the wdpa by each individual country and writes a kml for the point and polygon version in a country specific folder
for (i in Countries){
  
  # subset the point and polygon layer by individual country ("i") identified in the unique country iso3s
  ISO_Sub_Poly <- licensed_poly %>%
    subset( ISO3 %in% i)
  
  ISO_Sub_Point <- licensed_point %>%
    subset( ISO3 %in% i)
  
  #set the working directory to a location you want the save the files to
  setwd("C:/Users/osgurm/Downloads/Country_KML")
  #check if the country folder exists and create if it doesnt
  if(dir.exists(paste0(getwd(),"/",i)) == F){
    dir.create(paste0(getwd(),"/",i))
    setwd(paste0(getwd(),"/", i))
  }
  #write the point and polygon kmls to the country folder
  if(nrow(ISO_Sub_Poly) > 0 ){
  st_write(ISO_Sub_Poly, paste0("WDPA_poly_Apr2019_", i, ".kml"), driver='kml', update=TRUE)}
  if(nrow(ISO_Sub_Point) > 0 ){
  st_write(ISO_Sub_Point, paste0("WDPA_point_Apr2019_", i, ".kml"), driver='kml', update=TRUE)}
  
}



