library(dplyr)
library(plyr)
library(readxl)


#Read in the datasets
Pame <- read_excel("C:/Users/OsgurM/Downloads/pame.xlsx")
Pame <- Pame[!is.na(Pame$evaluation_id),]

China_Pame <- read.csv("C:/Users/OsgurM/Downloads/CHINA_PAME.csv")
lu_CountryNames_Jan2018 <- read.csv("C:/Users/OsgurM/OneDrive - WCMC/00_Data Management/Look_Up_Tables/lu_CountryNames_Jan2018.csv")
Unrestricted <- read.csv("C:/Users/OsgurM/Downloads/WDPA_May2019-csv/WDPA_May2019-csv.csv")
Unrestricted <- Unrestricted[,c(2,29)]

#Restricted Sites
EST_restricted_Aug2014_New_Res <- read_excel("C:/Users/OsgurM/Downloads/EST_restricted_Aug2014_New_Res.xlsx")
CHN_restricted_Nov2018_NR_Res <- read_excel("C:/Users/OsgurM/Downloads/CHN_restricted_Nov2018_NR_Res.xlsx")
CHN_restricted_Nov2018_NNR_Res <- read_excel("C:/Users/OsgurM/Downloads/CHN_restricted_Nov2018_NNR_Res.xlsx")
SHN_restricted_July2018_Res <- read_excel("C:/Users/OsgurM/Downloads/SHN_restricted_July2018_Res.xlsx")

Restricted <- rbind.fill(EST_restricted_Aug2014_New_Res,CHN_restricted_Nov2018_NR_Res,CHN_restricted_Nov2018_NNR_Res,SHN_restricted_July2018_Res)
Restricted$restricted <- TRUE

#order by metadata id so that we remover anything after the first row
Pame <- Pame[order(Pame$metadata_id),]

#change iso3 ro match country in the Pame dataset 
colnames(China_Pame)[7] <- "country"
China_Pame$country <- lu_CountryNames_Jan2018$Name[which(lu_CountryNames_Jan2018$ISO3=="CHN")]

#Join th two datasets 
Pame <- rbind.fill(Pame,China_Pame[,1:13])

#find unique rows based on particular columns
Pame <- Pame%>%
  distinct(url,year,methodology,wdpa_id,designation, .keep_all = TRUE)


#Add ISO3 column to the dataset
Pame_Merge <- merge(Pame, Unrestricted[,1:2], by.x = "wdpa_id", by.y = "WDPAID", all.x = TRUE)
Pame_Merge <- Pame_Merge[,c(2:3,1,14,4:13)]

#Pame_Merge$ISO3[c(21991:21992)] <- "GBR"
#Pame_Merge$ISO3[c(21991:21992)] <- "GBR"
#Pame_Merge$ISO3[c(27689:27698)] <- "GBR"
#Pame_Merge$ISO3[c(27780)] <- "GBR"
#Pame_Merge$ISO3[c(27805)] <- "USA"
#Pame_Merge$ISO3[c(27831)] <- "USA"
#Pame_Merge$ISO3[c(27846)] <- "USA"
Pame_Merge <- Pame_Merge%>%
  distinct(url,year,methodology,wdpa_id,designation, .keep_all = TRUE)

for(i in 1:length(Pame_Merge$ISO3)){
  if(is.na(Pame_Merge$ISO3[i])){
    Pame_Merge$ISO3[i] <- lu_CountryNames_Jan2018$ISO3[grep(Pame_Merge$country[i],lu_CountryNames_Jan2018$Name)]
  }
}

#Assign Restricted status
Pame_Merge <- merge(Pame_Merge, Restricted[,c(2,33)], by.x = "wdpa_id", by.y = "WDPAID", all.x = TRUE)

Pame_Merge$restricted[is.na(Pame_Merge$restricted)] <- FALSE

Pame_Merge <- Pame_Merge%>%
  distinct(url,year,methodology,wdpa_id,designation, .keep_all = TRUE)

Pame_Merge$ISO3 <- as.character(Pame_Merge$ISO3)
for ( i in 1:length(Pame_Merge$country)){
  Pame_Merge$country[i] <-  paste(lu_CountryNames_Jan2018$Name[c(which(lu_CountryNames_Jan2018$ISO3 %in% unlist(strsplit(Pame_Merge$ISO3[i], ";")) ))], collapse  = "; ")
}

#set the output location 
setwd("C:/Users/OsgurM/OneDrive - WCMC/02_WDPA/PAME/OutputDatasets")

#Output the dataset
write.csv(Pame_Merge, paste("PAME_",  Sys.Date(), ".csv"), row.names = F)
