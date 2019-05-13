library(sf)
library(rgdal)
library(readxl)
library(wkb)

setwd("C:/Users/osgurm/Documents/ArcGIS/Projects/Protected_Planet/WDPA/WDPA.gdb")

ogrListLayers(getwd())

licensed <- read_sf(getwd(),  "WDPA_poly_Apr2019")


setwd("C:/Users/osgurm/OneDrive - WCMC/Ferdi")

st_write(licensed[1:100,], "Licensed_XYZ.csv", layer_options = "GEOMETRY=AS_WKT" ) # writes X and Y as columns



