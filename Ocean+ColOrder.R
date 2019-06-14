library(rgdal)
library(dplyr)
library(sf)
library(arcgisbinding)
library(plyr)

arc.check_product()

###### Change these variables to update layers#####
setwd("P:/PROGRAMMES/MARINE/Data/_WCMC-OceanDataViewer-Datasets/WCMC-013-014-GlobalDistributionOfSeagrasses2005/014_001_WCMC013-014_SeagrassPtPy2018_v6/01_Data")
OutputPath <- "C:/Users/osgurm/Documents/ArcGIS/Projects/Ocean+/Ocean+Habitats.gdb"


ogrListLayers(getwd())


LayerName <- "WCMC_013_014_SeagrassesPy_v6"
OutputName <- "WCMC_013_014_SeagrassesPy_v6"
#####################################################################


Arc.Layer <- arc.open(paste0(getwd(), "/", LayerName, ".shp"))
Arc.Layer@extent
Arc.Layer@fields

Layer <- arc.select(object = Arc.Layer, fields = "*")
Layer.shape <- arc.shapeinfo(arc.shape(Layer))

#Layer <- readOGR(getwd(),LayerName)

TemplatePt <- c( "LAYER_ID", "METADATA_ID", "PARENT_ISO3","ISO3","SUB_LOC","NAME","ORIG_NAME","LOC_DEF","FAMILY","GENUS","SPECIES","DATA_TYPE","SURVEY_MET","START_DATE", "END_DATE", "DATE_TYPE","DEPTH_MIN","DEPTH_MAX", "PROTECT","PROTECT_FEAT","PROTECT_STAT","VERIF")


TemplatePy <- c( "LAYER_ID", "METADATA_ID", "PARENT_ISO3","ISO3","SUB_LOC","NAME","ORIG_NAME","LOC_DEF","FAMILY","GENUS","SPECIES", "GIS_AREA_KM2","DATA_TYPE","SURVEY_MET","START_DATE", "END_DATE", "DATE_TYPE","DEPTH_MIN","DEPTH_MAX", "PROTECT","PROTECT_FEAT","PROTECT_STAT","VERIF")


names(Layer)[which(names(Layer) == "METADATA_I")] <- "METADATA_ID"
names(Layer)[which(names(Layer) == "PARENT_ISO")] <- "PARENT_ISO3"
names(Layer)[which(names(Layer) == "PROTECT_ST")] <- "PROTECT_STAT"
names(Layer)[which(names(Layer) == "LAYER_NAME")] <- "LAYER_ID"
names(Layer)[which(names(Layer) == "PROTECT_FE")] <- "PROTECT_FEAT"
names(Layer)[which(names(Layer) == "GIS_AREA_K")] <- "GIS_AREA_KM2"



TemplatePt <- setNames(data.frame(matrix(ncol = 22, nrow = 0)), c(TemplatePt))
TemplatePy <- setNames(data.frame(matrix(ncol = 23, nrow = 0)), c(TemplatePy))

if(!"DEPTH_MAX" %in% names(Layer) ){
  TemplatePt <- TemplatePt[,-c(which(names(TemplatePt) == "DEPTH_MAX"), which(names(TemplatePt) == "DEPTH_MIN"))]
  TemplatePy <- TemplatePy[,-c(which(names(TemplatePy) == "DEPTH_MAX"), which(names(TemplatePy) == "DEPTH_MIN"))]
}

if(grepl("Py", LayerName)){
Layer <- Layer[,c(match(names(TemplatePy),names(Layer)))]
}
if(grepl("Pt", LayerName)){
  Layer <- Layer[,c(match(names(TemplatePt),names(Layer)))]
}

#readOGR(Layer, getwd() ,OutputName, "ESRI Shapefile")

arc.write(path = paste0(OutputPath,"/", OutputName), data = Layer, overwrite = T)

