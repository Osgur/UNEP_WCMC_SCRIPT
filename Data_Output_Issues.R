library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence
library(foreign)
library(stringr)
library(maptools)
library(sf)


#####Read in checklist folders
Red_Flag <- read.csv("C:/Users/OsgurM/OneDrive - WCMC/Data Management/lu_ISO3_flag_as_need_attention.csv")
ISO_List <- read.csv("C:/Users/OsgurM/OneDrive - WCMC/Data Management/lu_CountryNames_Jan2018.csv")
ParentISO_ISO3_relationship <- read.csv("C:/Users/OsgurM/OneDrive - WCMC/Data Management/lu_ParentISO3_ISO3_relationship.csv")

####Read in the EEZ files for checking ISO3
#EEZFolder <- "C:/Users/OsgurM/OneDrive - WCMC/EEZ/tempfgdb.gdb"
#EEZFiles <- ogrListLayers(EEZFolder)
#EEZ <- readOGR(dsn=EEZFolder,layer= EEZFiles)


#### Read in the Dataset being examined
folder <- "C:/Users/OsgurM/OneDrive - WCMC/Data Management/Dissolved.gdb" # identify the gdb for the data layer 
files <- ogrListLayers(folder)# list the files
filespt <- files[which(grepl("pt|Pt", files))]
filespy <- files[which(grepl("py|Py", files))]

for(j in filespy){
Hab <- files[which(grepl(j, files))]


Dataset <- readOGR(dsn=folder,layer= Hab) ### instead of "Py" insert a unique pattern in the dataset you want to examine 

###### Run quieries 
Red_Flags_Present <-  droplevels(Red_Flag[Red_Flag$ISO3 %in% Dataset$ISO3,1])
Incorrect_ISO3_present <- droplevels(unique(Dataset$ISO3[!Dataset$ISO3 %in% ISO_List$ISO3]))
Parent_ISO_Present <- droplevels(unique(Dataset$ISO3[Dataset$ISO3 %in% ParentISO_ISO3_relationship$Parent]))

Overseas_Info <- data.frame()
for(i in Parent_ISO_Present){
  Overseas <- droplevels(ParentISO_ISO3_relationship$ISO3[which(ParentISO_ISO3_relationship$Parent == i)])
  if(i %in% Overseas){
    Overseas <- Overseas[-which(Overseas == i)]}
  Overseas_Present <-  Overseas[Overseas %in% Dataset$ISO3]
  
  Overseas_Info <- rbind(Overseas_Info, cbind(i, paste(Overseas_Present, collapse = "; ")))
}
colnames(Overseas_Info) <- c("Parent_ISO", "Overseas_Territory_ISOs_Present")

factorNAValue <- NA %in% unlist(sapply(data.frame(Dataset)[,c(which(sapply(data.frame(Dataset), class) == "factor"))], levels))
factorZeroValue <- "0" %in% unlist(sapply(data.frame(Dataset)[,c(which(sapply(data.frame(Dataset), class) == "factor"))], levels))

numericNAValue <- NA %in% unlist(sapply(data.frame(Dataset)[,c(which(sapply(data.frame(Dataset), class) == "numeric"))], unique))
numericZeroValue <- 0 %in% unlist(sapply(data.frame(Dataset)[,c(which(sapply(data.frame(Dataset), class) == "numeric"))], unique))



colnames(data.frame(Dataset))


## chose based on unique combo which columns to remove for disolve. Type column names in the select section


setwd("C:/Users/OsgurM/OneDrive - WCMC/Data Management/Quality_Checks/Ouput_QA_reports")
sink(paste( Hab, ".txt", sep = ""))




print("Red_Flags_Present") 
print(Red_Flags_Present)
print("Incorrect_ISO3_present")
print(Incorrect_ISO3_present)
print("Parent_ISO_Present")
print(Parent_ISO_Present)
print("Overseas_Info")
print(Overseas_Info)
print("factorNAValue")
print(factorNAValue)
print("factorZeroValue")
print(factorZeroValue)
print("numericNAValue")
print(numericNAValue)
print("numericZeroValue")
print(numericZeroValue)




sink()
}
for(j in filespt){
  Hab <- files[which(grepl(j, files))]
  
  
  Dataset <- st_read(dsn=folder,layer= Hab) ### instead of "Py" insert a unique pattern in the dataset you want to examine 
  
  ###### Run quieries 
  Red_Flags_Present <-  droplevels(Red_Flag[Red_Flag$ISO3 %in% Dataset$ISO3,1])
  Incorrect_ISO3_present <- droplevels(unique(Dataset$ISO3[!Dataset$ISO3 %in% ISO_List$ISO3]))
  Parent_ISO_Present <- droplevels(unique(Dataset$ISO3[Dataset$ISO3 %in% ParentISO_ISO3_relationship$Parent]))
  
  Overseas_Info <- data.frame()
  for(i in Parent_ISO_Present){
    Overseas <- droplevels(ParentISO_ISO3_relationship$ISO3[which(ParentISO_ISO3_relationship$Parent == i)])
    if(i %in% Overseas){
      Overseas <- Overseas[-which(Overseas == i)]}
    Overseas_Present <-  Overseas[Overseas %in% Dataset$ISO3]
    
    Overseas_Info <- rbind(Overseas_Info, cbind(i, paste(Overseas_Present, collapse = "; ")))
  }
  colnames(Overseas_Info) <- c("Parent_ISO", "Overseas_Territory_ISOs_Present")
  
  factorNAValue <- NA %in% unlist(sapply(data.frame(Dataset)[,c(which(sapply(data.frame(Dataset), class) == "factor"))], levels))
  factorZeroValue <- "0" %in% unlist(sapply(data.frame(Dataset)[,c(which(sapply(data.frame(Dataset), class) == "factor"))], levels))
  
  numericNAValue <- NA %in% unlist(sapply(data.frame(Dataset)[,c(which(sapply(data.frame(Dataset), class) == "numeric"))], unique))
  numericZeroValue <- 0 %in% unlist(sapply(data.frame(Dataset)[,c(which(sapply(data.frame(Dataset), class) == "numeric"))], unique))
  
  
  
  colnames(data.frame(Dataset))
  
  
  ## chose based on unique combo which columns to remove for disolve. Type column names in the select section
  
  
  setwd("C:/Users/OsgurM/OneDrive - WCMC/Data Management/Quality_Checks/Ouput_QA_reports")
  sink(paste( Hab, ".txt", sep = ""))
  
  
  
  
  print("Red_Flags_Present") 
  print(Red_Flags_Present)
  print("Incorrect_ISO3_present")
  print(Incorrect_ISO3_present)
  print("Parent_ISO_Present")
  print(Parent_ISO_Present)
  print("Overseas_Info")
  print(Overseas_Info)
  print("factorNAValue")
  print(factorNAValue)
  print("factorZeroValue")
  print(factorZeroValue)
  print("numericNAValue")
  print(numericNAValue)
  print("numericZeroValue")
  print(numericZeroValue)
  
  
  
  
  sink()
}

