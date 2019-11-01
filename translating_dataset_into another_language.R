install.packages("devtools")
devtools::install_github("ChristopherLucas/translateR", force = T)

library(arcgisbinding)
library(rgdal)
library(dplyr)
library(translateR)
arc.check_product()

#Set the working directory
setwd("D:/")

#Identify GIS files in working directory
ogrListLayers(getwd())

#read in dataset, can be a csv etc but here it is a shapefile
rus <- arc.open(paste0(getwd(),"/", "gen_pol_area.shp")) %>%
  arc.select("*")

#Translate the field of choice (here = "TITLE") and select source language (here = "ru", RUssian) and destination language( here = "en", English)
dataset.out <- translateR::translate(dataset = rus,
                                     content.field = 'TITLE', 
                                     microsoft.api.key  = '1f996f504c724c178e6ec4343cd90d64', 
                                     source.lang = 'ru', target.lang = 'en')

colnames(dataset.out)[ncol(dataset.out)] <- "TITLE_en"

# dataset.out <- translateR::translate(dataset = dataset.out,
#                                      content.field = 'SIG', 
#                                      microsoft.api.key  = '1f996f504c724c178e6ec4343cd90d64', 
#                                      source.lang = 'ru', target.lang = 'en')
# 
# colnames(dataset.out)[ncol(dataset.out)] <- "SIG_en"
# 
# dataset.out <- translateR::translate(dataset = dataset.out,
#                                      content.field = 'CATEGORY', 
#                                      microsoft.api.key  = '1f996f504c724c178e6ec4343cd90d64', 
#                                      source.lang = 'ru', target.lang = 'en')
# 
# colnames(dataset.out)[ncol(dataset.out)] <- "CATEGORY_en"
# 
# dataset.out <- translateR::translate(dataset = dataset.out,
#                                      content.field = 'STATUS', 
#                                      microsoft.api.key  = '1f996f504c724c178e6ec4343cd90d64', 
#                                      source.lang = 'ru', target.lang = 'en')
# 
# colnames(dataset.out)[ncol(dataset.out)] <- "STATUS_en"
# 
# dataset.out <- translateR::translate(dataset = dataset.out,
#                                      content.field = 'SUBRF', 
#                                      microsoft.api.key  = '1f996f504c724c178e6ec4343cd90d64', 
#                                      source.lang = 'ru', target.lang = 'en')
# 
# colnames(dataset.out)[ncol(dataset.out)] <- "SUBRF_en"

#write the output to where you want it by setting working directory using setwd()

#setwd <- "folder path you want to save to"
arc.write(paste0(getwd(),"/", "gen_pol_area_trans.shp"), data = dataset.out)

