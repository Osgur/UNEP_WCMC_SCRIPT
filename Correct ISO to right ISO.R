library(sp)# vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence

proj <- CRS("+proj=longlat +datum=WGS84")

ISO3List <- read.csv("C:/Users/OsgurM/OneDrive - WCMC/ISO3List.csv", na.strings = "NA")


##################Creating Package data for countries for the following habitats
setwd("C:/Users/OsgurM/Downloads/Country_Packs_Data/Data")

Habitat_layers <- list.files( getwd())

for (k in Habitat_layers){
folder <- paste("C:/Users/OsgurM/Downloads/Country_Packs_Data/Data/", k, sep = "") # identify the gdb for the habitat layer 
files <- ogrListLayers(folder)# list the files

points_ShpLayer <- readOGR(dsn=folder,layer=paste(files[grepl("Pt", files)], sep = "")) # reads in the point shapefile for the habitat layer
poly_ShpLayer <- readOGR(dsn=folder,layer=paste(files[grepl("Py", files)], sep = "")) # reads in the point shapefile for the habitat layer

points_Table <- data.frame(points_ShpLayer)
poly_Table <- data.frame(poly_ShpLayer)


ISOIssue <- character()
for( i in levels(points_Table$ISO3)[!grepl(";",levels(points_Table$ISO3))][! levels(points_Table$ISO3)[!grepl(";",levels(points_Table$ISO3))] %in% c("N/A")]){
  tryCatch({
  points_Table$PARENT_ISO[which(points_Table$ISO3 == i)] <- ISO3List$Parent[which(ISO3List$ISO3 == i)]
  }, error=function(err){ ISOIssue <<- c(ISOIssue, i)})
}
