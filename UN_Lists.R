# Install the necessary library packages
install.packages(c("sp", "raster", "rgdal", "rgeos", "gtools", "spdep", "foreign", "stringr", "devtools", "sf", "xlsx", "plyr", "readxl", "knitr", "dplyr", "shiny", "tidyverse"))
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(gtools)
library(spdep)  # spatial dependence
library(foreign)
library(stringr)
library(devtools)
library(sf)
library(xlsx)
library(plyr)
library(readxl)
library(knitr)
library(dplyr)
library(shiny)
library(tidyverse)
library(png)

devtools::install_github("haozhu233/kableExtra")
library(kableExtra)


options(scipen=999)

setwd("//wcmc-data-03/WDPA/2_July_2018_Analyses/Section1_Global_Analyses/1_Data/WDPA_with_Regions_2018.gdb")
ogrListLayers(getwd())
CountryNames <- read.csv("C:/Users/OsgurM/OneDrive - WCMC/Data Management/lu_CountryNames_Jan2018.csv")
CountryNames <- CountryNames[,1:2]

poly <- data.frame(read_sf(getwd(), layer = "WDPA_poly_Jul2018_Regions"))
poly <- poly[,1:30]
point <- data.frame(read_sf(getwd(), layer = "WDPA_point_Jul2018_Regions"))
point <- point[,1:28]
WDPA <- rbind.fill(poly,point)
rm(poly, point)
gc()
PAME <- read_excel("C:/Users/OsgurM/OneDrive - WCMC/WDPA/UN_Lists/PAME.xlsx")

WDPA <- WDPA[, c("WDPAID", "NAME", "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT", "REP_AREA", "STATUS", "STATUS_YR","ISO3",  "PARENT_ISO3")]


PAME <- PAME[,-1]
PAME <- unique(PAME)

# SummaryPAME <- data.frame(table(PAME$wdpa_id))
#setwd("C:/Users/OsgurM/OneDrive - WCMC/WDPA/UN_Lists")
#write.csv(WDPA, "WDPA_Jul2018_Regions_Restricted.csv")

WDPA <- WDPA[with(WDPA,order(WDPAID, -rank(REP_AREA), decreasing = F)), ]
##Fix Parent ISO Issues
naissues <- subset(WDPA, is.na(WDPA$PARENT_ISO3))
WDPA[which(WDPA$ISO3 == "SHN"), "PARENT_ISO3"] <- "GBR"
WDPA[which(WDPA$ISO3 == "CHN"), "PARENT_ISO3"] <- "CHN"
WDPA[which(WDPA$ISO3 == "EST"), "PARENT_ISO3"] <- "EST"
naissues <- subset(WDPA, is.na(WDPA$PARENT_ISO3))

WDPA <- WDPA[, -c(which(colnames(WDPA)=="ISO3"))]


WDPA_Multi <-subset(WDPA, WDPA$WDPAID %in% as.numeric(names(table(WDPA$WDPAID)[which(table(WDPA$WDPAID) > 1)])))
##Cleaning up deparate names as a result of P_ID
WDPA_Multi$NAME <- gsub("\\s*\\([^\\)]+\\)","",WDPA_Multi$NAME)
WDPA_Multi$NAME <- gsub("#.*","",WDPA_Multi$NAME)
WDPA_Multi$NAME <- gsub("No..*","",WDPA_Multi$NAME)
WDPA_Multi$NAME <- gsub('[[:digit:]]+', '', WDPA_Multi$NAME)

#Remove Multi from the WDPA
WDPA <- WDPA[!WDPA$WDPAID %in% unique(WDPA_Multi$WDPAID), ]






#### remove duplication of rep area in chinese P_IDs 
WDPA_Multi_CHN <- subset(WDPA_Multi, WDPA_Multi$PARENT_ISO3 == "CHN")

WDPA_Multi_CHN <- WDPA_Multi_CHN %>% 
  group_by(WDPAID) %>%  
  summarise(DESIG_TYPE = paste(unique(DESIG_TYPE), collapse = "; "),
            DESIG_ENG = paste(unique(DESIG_ENG), collapse = "; "),
            NAME = head(NAME, n = 1),
            STATUS = paste(unique(STATUS), collapse = "; "),
            STATUS_YR = min(STATUS_YR),
            REP_AREA= head(REP_AREA, n = 1),
            IUCN_CAT = paste(unique(IUCN_CAT), collapse = "; "), 
            PARENT_ISO3 = paste(unique(PARENT_ISO3))
            )

#Remove Chinese from Multi
WDPA_Multi <- WDPA_Multi[!WDPA_Multi$WDPAID %in% unique(WDPA_Multi_CHN$WDPAID), ]

#Summarise Multi

WDPA_Multi <- WDPA_Multi %>% 
  group_by(WDPAID) %>%  
  summarise(DESIG_TYPE = paste(unique(DESIG_TYPE), collapse = "; "),
            DESIG_ENG = paste(unique(DESIG_ENG), collapse = "; "),
            NAME = head(NAME, n = 1),
            STATUS = paste(unique(STATUS), collapse = "; "),
            STATUS_YR = min(STATUS_YR),
            REP_AREA= sum(REP_AREA),
            IUCN_CAT = paste(unique(IUCN_CAT), collapse = "; "),
            PARENT_ISO3 = paste(unique(PARENT_ISO3)))


#Recombine Datasets
WDPA <- rbind(WDPA, WDPA_Multi, WDPA_Multi_CHN)


####PAME manipulation
PAME <- ddply(PAME, .(wdpa_id, methodology ), summarise, year = min(year)) ### removes duplicate methodology and assigns minimum year
PAME <- ddply(PAME, .(wdpa_id ), summarise,methodology = paste(unique(methodology), collapse = "; "), year = paste(unique(year), collapse = "; "))


### Join the WDPA with PAME

Join <- merge(WDPA, PAME, by.x = "WDPAID", by.y = "wdpa_id", all = T)
Join[which(Join$PARENT_ISO3 == "NDL")] <- "NLD"

Countries <- unique(Join$PARENT_ISO3) ## identify unique countries in the dataset
Countries <- sort(Countries)
Countries <- Countries[-c(grep(";", Countries))]
Countries <- na.omit(Countries)


##Remove Overseas for Map updates
Overseas <- c("AUS", "DNK", "FIN", "FRA", "GBR", "NLD", "NOR", "NZL", "USA")
Countries <- Countries[-c(which(Countries %in% Overseas))]
Countries <- na.omit(Countries)

Join[which(Join$PARENT_ISO3)]

for(i in Countries[1:length(Countries)]){
  CountryData <- Join[grep(i , Join$PARENT_ISO3),] # subset by country PARENT_ISO3
  CountryData <- CountryData[with(CountryData, order(DESIG_TYPE, DESIG_ENG, NAME, year)), ] ## Order the data by the columns we want
  CountryData$NAME <-  trimws(CountryData$NAME, which = c("left")) # remove leading white space on names
  
  ## summarise the dataset for each column to account for multiple WDPAIDs 
  
  CountryData2 <- CountryData %>% 
    group_by(WDPAID) %>%  
    summarise(DESIG_TYPE = paste(unique(DESIG_TYPE), collapse=", "),
              DESIG_ENG = paste(unique(DESIG_ENG), collapse=", "),
              NAME = paste(unique(NAME), collapse=", "),
              STATUS = paste(unique(STATUS), collapse=", "),
              STATUS_YR = paste(unique(STATUS_YR), collapse=", "),
              REP_AREA = paste(unique(REP_AREA), collapse=", "),
              IUCN_CAT = paste(unique(IUCN_CAT), collapse=", "),
              methodology = paste(unique(methodology), collapse=", "), 
              year=paste(year, collapse = ", "),
              NAMESort = paste(unique(NAME), collapse=", ")) %>%
              mutate(NAME = cell_spec(NAME, "html", link = paste("https://www.protectedplanet.net/", WDPAID, sep = "")))
  CountryData3 <- CountryData2
  CountryData3 <- CountryData3[,c(1,2,3,11,5,6,7,8,9,10)]
  colnames(CountryData3)[4:(ncol(CountryData3))] <- c("Name of Protected Areas",	"Status",	"Year of designation",	"Area (km2)",	"IUCN category", "PAME methodology", "Year of PAME assessment")  
  setwd("P:/PROGRAMMES/PROTECTED AREAS/STAFF FOLDERS/Marine/Osgur_to_Marine") # set folder to save into
  write.csv(CountryData3, paste(i,".csv", sep = ""), row.names = F) # write csv file for the country subseted by
  
  
  CountryData2$methodology <- gsub("NA", "Not Reported", CountryData2$methodology) # Change NA values to not reported
  CountryData2$year <- gsub("NA", 0, CountryData2$year) # Change NA values to 0 for year of assessment
  
  CountryData2 <- CountryData2[with(CountryData2, order(DESIG_TYPE, DESIG_ENG, NAMESort)), ] # order by columns we want
  CountryData2 <- CountryData2[,-1] # remove WDPAID column
  CountryData2$REP_AREA <- format(round(as.numeric(CountryData2$REP_AREA), 2), nsmall = 2)
  

 
  #### Creating tables
  intdiv <- unique(CountryData2[which(CountryData2$DESIG_TYPE== "International"), c("DESIG_ENG")]) # identify rows of the dataframe that fall under international Designation type
  natdiv <- unique(CountryData2[which(CountryData2$DESIG_TYPE== "National"), c("DESIG_ENG")])# identify rows of the dataframe that fall under National Designation type
  NAdiv  <- unique(CountryData2[which(CountryData2$DESIG_TYPE== "Not Applicable"), c("DESIG_ENG")])# identify rows of the dataframe that fall under Not Applicable Designation type
  regdiv <- unique(CountryData2[which(CountryData2$DESIG_TYPE== "Regional"), c("DESIG_ENG")])# identify rows of the dataframe that fall under Regional Designation type
  
  
  #change column names for table creation
  colnames(CountryData2)[3:(ncol(CountryData2)-1)] <- c("Designation Type/ Designation in English / Name of Protected Areas",	"Status",	"Year of designation",	"Area (km2)",	"IUCN category", "PAME methodology", "Year of PAME assessment")
  
  
  country <- CountryNames[which(CountryNames$ISO3 == i),2]
  
  
  
  Table <- CountryData2[3:(ncol(CountryData2)-1)] %>% 
    kable("html",table.attr = "class='dtable'", escape = FALSE, align = "c")%>%
    kable_styling("striped", full_width = F, bootstrap_options = c("hover", "condensed"))%>%
    row_spec(0, bold = T, color = "black", background = "#66b2ff")
  

  if("International" %in% CountryData2$DESIG_TYPE){
    status <- "International"
    Table <- group_rows(Table, "International", min(which(CountryData2$DESIG_TYPE == "International")), max(which(CountryData2$DESIG_TYPE == "International")), label_row_css = "background-color: #ff9400; color: #000000;")
    
    k <- 1
    while (k < nrow(intdiv)+1) {
      print(paste("International",k))
      Table <- group_rows(Table, intdiv[k,1], min(which(CountryData2[,2] == as.character(intdiv[k,1]))), max(which(CountryData2[,2] == as.character(intdiv[k,1]))), label_row_css = "background-color: #4f9e4a; color: #000000;")
      k = k+1
    }
  }
  if("National" %in% CountryData2$DESIG_TYPE){
    status <- "National"
    Table <- group_rows(Table, "National", min(which(CountryData2$DESIG_TYPE == "National")), max(which(CountryData2$DESIG_TYPE == "National")), label_row_css = "background-color: #ff9400; color: #000000;")
    k <- 1
    while (k < nrow(natdiv)+1) {
      print(paste("National",k))
      Table <- group_rows(Table, natdiv[k,1], min(which(CountryData2[,2] == as.character(natdiv[k,1]))), max(which(CountryData2[,2] == as.character(natdiv[k,1]))), label_row_css = "background-color: #4f9e4a; color: #000000;")
      k = k+1
    }
  }
  if("Not Applicable" %in% CountryData2$DESIG_TYPE){
    status <- "Not Applicable"
    Table <- group_rows(Table, "Not Applicable", min(which(CountryData2$DESIG_TYPE == "Not Applicable")), max(which(CountryData2$DESIG_TYPE == "Not Applicable")), label_row_css = "background-color: #ff9400; color: #000000;")
    k <- 1
    while (k < nrow(NAdiv)+1) {
      print(paste("Not Applicable",k))
      Table <- group_rows(Table, NAdiv[k,1], min(which(CountryData2[,2] == as.character(NAdiv[k,1]))), max(which(CountryData2[,2] == as.character(NAdiv[k,1]))), label_row_css = "background-color: #4f9e4a; color: #000000;")
      k = k+1
    }
  }
  if("Regional" %in% CountryData2$DESIG_TYPE){
    status <- "Regional"
    Table <- group_rows(Table,"Regional", min(which(CountryData2$DESIG_TYPE == "Regional")), max(which(CountryData2$DESIG_TYPE == "Regional")), label_row_css = "background-color: #ff9400; color: #000000;")
    k <- 1
    while (k < nrow(regdiv)+1) {
      print(paste("Regional",k))
      Table <- group_rows(Table, regdiv[k,1], min(which(CountryData2[,2] == as.character(regdiv[k,1]))), max(which(CountryData2[,2] == as.character(regdiv[k,1]))), label_row_css = "background-color: #4f9e4a; color: #000000;")
      k = k+1
    }
  }
  
  img <- readPNG(paste0("C:/Users/OsgurM/OneDrive - WCMC/WDPA/UN_Lists/Maps/", i, ".png")) 
  width <- dim(img)[2]
  height <- dim(img)[1]

  Table <-fluidPage(
    fluidRow(
      column(8,offset = 2, h1(paste("2018 United Nations List of Protected Areas of", country, sep = " "), align = "center"), h2(paste("Information extracted from the July 2018 WDPA release",sep = " "), align = "center"), h6(paste("This list was compiled from data based on the July 2018 version of the WDPA. In addition to protected areas with spatial data in the WDPA, the 2018 version of the UN List includes sites for which no spatial data is available but which have been provided by the relevant country. Management effectiveness information is extracted from GD-PAME. Both databases are updated and released via www.protectedplanet.net on a monthly basis. The depiction and use of boundaries and related data shown on maps are not guaranteed to be error free nor do they necessarily imply official endorsement or acceptance by the United Nations."), align = "center")),
      column(12, align="center",div(style="display: inline-block;",img(src=paste0("C:/Users/OsgurM/OneDrive - WCMC/WDPA/UN_Lists/Maps/", i, ".png"), height=height, width=width))),
      column(12, HTML(Table),div())))

  setwd("P:/PROGRAMMES/PROTECTED AREAS/STAFF FOLDERS/Marine/Osgur_to_Marine")
  save_kable(Table , file = paste(i,".html", sep = ""), self_contained = T)
  
  rm(Table, CountryData,CountryData2, CountryData3)
  gc()
  
}
for(i in Overseas[1:length(Overseas)]){
  CountryData <- Join[grep(i , Join$PARENT_ISO3),] # subset by country PARENT_ISO3
  CountryData <- CountryData[with(CountryData, order(DESIG_TYPE, DESIG_ENG, NAME, year)), ] ## Order the data by the columns we want
  CountryData$NAME <-  trimws(CountryData$NAME, which = c("left")) # remove leading white space on names
  
  ## summarise the dataset for each column to account for multiple WDPAIDs 
  
  CountryData2 <- CountryData %>% 
    group_by(WDPAID) %>%  
    summarise(DESIG_TYPE = paste(unique(DESIG_TYPE), collapse=", "),
              DESIG_ENG = paste(unique(DESIG_ENG), collapse=", "),
              NAME = paste(unique(NAME), collapse=", "),
              STATUS = paste(unique(STATUS), collapse=", "),
              STATUS_YR = paste(unique(STATUS_YR), collapse=", "),
              REP_AREA = paste(unique(REP_AREA), collapse=", "),
              IUCN_CAT = paste(unique(IUCN_CAT), collapse=", "),
              methodology = paste(unique(methodology), collapse=", "), 
              year=paste(year, collapse = ", "),
              NAMESort = paste(unique(NAME), collapse=", ")) %>%
    mutate(NAME = cell_spec(NAME, "html", link = paste("https://www.protectedplanet.net/", WDPAID, sep = "")))
  CountryData3 <- CountryData2
  CountryData3 <- CountryData3[,c(1,2,3,11,5,6,7,8,9,10)]
  colnames(CountryData3)[4:(ncol(CountryData3))] <- c("Name of Protected Areas",	"Status",	"Year of designation",	"Area (km2)",	"IUCN category", "PAME methodology", "Year of PAME assessment")  
  setwd("P:/PROGRAMMES/PROTECTED AREAS/STAFF FOLDERS/Marine/Osgur_to_Marine") # set folder to save into
  write.csv(CountryData3, paste(i,".csv", sep = ""), row.names = F) # write csv file for the country subseted by
  
  
  CountryData2$methodology <- gsub("NA", "Not Reported", CountryData2$methodology) # Change NA values to not reported
  CountryData2$year <- gsub("NA", 0, CountryData2$year) # Change NA values to 0 for year of assessment
  
  CountryData2 <- CountryData2[with(CountryData2, order(DESIG_TYPE, DESIG_ENG, NAMESort)), ] # order by columns we want
  CountryData2 <- CountryData2[,-1] # remove WDPAID column
  CountryData2$REP_AREA <- format(round(as.numeric(CountryData2$REP_AREA), 2), nsmall = 2)
  
  
  
  #### Creating tables
  intdiv <- unique(CountryData2[which(CountryData2$DESIG_TYPE== "International"), c("DESIG_ENG")]) # identify rows of the dataframe that fall under international Designation type
  natdiv <- unique(CountryData2[which(CountryData2$DESIG_TYPE== "National"), c("DESIG_ENG")])# identify rows of the dataframe that fall under National Designation type
  NAdiv  <- unique(CountryData2[which(CountryData2$DESIG_TYPE== "Not Applicable"), c("DESIG_ENG")])# identify rows of the dataframe that fall under Not Applicable Designation type
  regdiv <- unique(CountryData2[which(CountryData2$DESIG_TYPE== "Regional"), c("DESIG_ENG")])# identify rows of the dataframe that fall under Regional Designation type
  
  
  #change column names for table creation
  colnames(CountryData2)[3:(ncol(CountryData2)-1)] <- c("Designation Type/ Designation in English / Name of Protected Areas",	"Status",	"Year of designation",	"Area (km2)",	"IUCN category", "PAME methodology", "Year of PAME assessment")
  
  
  country <- CountryNames[which(CountryNames$ISO3 == i),2]
  
  
  
  Table <- CountryData2[3:(ncol(CountryData2)-1)] %>% 
    kable("html",table.attr = "class='dtable'", escape = FALSE, align = "c")%>%
    kable_styling("striped", full_width = F, bootstrap_options = c("hover", "condensed"))%>%
    row_spec(0, bold = T, color = "black", background = "#66b2ff")
  
  
  if("International" %in% CountryData2$DESIG_TYPE){
    status <- "International"
    Table <- group_rows(Table, "International", min(which(CountryData2$DESIG_TYPE == "International")), max(which(CountryData2$DESIG_TYPE == "International")), label_row_css = "background-color: #ff9400; color: #000000;")
    
    k <- 1
    while (k < nrow(intdiv)+1) {
      print(paste("International",k))
      Table <- group_rows(Table, intdiv[k,1], min(which(CountryData2[,2] == as.character(intdiv[k,1]))), max(which(CountryData2[,2] == as.character(intdiv[k,1]))), label_row_css = "background-color: #4f9e4a; color: #000000;")
      k = k+1
    }
  }
  if("National" %in% CountryData2$DESIG_TYPE){
    status <- "National"
    Table <- group_rows(Table, "National", min(which(CountryData2$DESIG_TYPE == "National")), max(which(CountryData2$DESIG_TYPE == "National")), label_row_css = "background-color: #ff9400; color: #000000;")
    k <- 1
    while (k < nrow(natdiv)+1) {
      print(paste("National",k))
      Table <- group_rows(Table, natdiv[k,1], min(which(CountryData2[,2] == as.character(natdiv[k,1]))), max(which(CountryData2[,2] == as.character(natdiv[k,1]))), label_row_css = "background-color: #4f9e4a; color: #000000;")
      k = k+1
    }
  }
  if("Not Applicable" %in% CountryData2$DESIG_TYPE){
    status <- "Not Applicable"
    Table <- group_rows(Table, "Not Applicable", min(which(CountryData2$DESIG_TYPE == "Not Applicable")), max(which(CountryData2$DESIG_TYPE == "Not Applicable")), label_row_css = "background-color: #ff9400; color: #000000;")
    k <- 1
    while (k < nrow(NAdiv)+1) {
      print(paste("Not Applicable",k))
      Table <- group_rows(Table, NAdiv[k,1], min(which(CountryData2[,2] == as.character(NAdiv[k,1]))), max(which(CountryData2[,2] == as.character(NAdiv[k,1]))), label_row_css = "background-color: #4f9e4a; color: #000000;")
      k = k+1
    }
  }
  if("Regional" %in% CountryData2$DESIG_TYPE){
    status <- "Regional"
    Table <- group_rows(Table,"Regional", min(which(CountryData2$DESIG_TYPE == "Regional")), max(which(CountryData2$DESIG_TYPE == "Regional")), label_row_css = "background-color: #ff9400; color: #000000;")
    k <- 1
    while (k < nrow(regdiv)+1) {
      print(paste("Regional",k))
      Table <- group_rows(Table, regdiv[k,1], min(which(CountryData2[,2] == as.character(regdiv[k,1]))), max(which(CountryData2[,2] == as.character(regdiv[k,1]))), label_row_css = "background-color: #4f9e4a; color: #000000;")
      k = k+1
    }
  }
  
  
  
  img <- readPNG(paste0("C:/Users/OsgurM/OneDrive - WCMC/WDPA/UN_Lists/Maps/", i, ".png")) 
  width <- dim(img)[2]
  height <- dim(img)[1]
  
  Table <-fluidPage(
    fluidRow(
      column(8,offset = 2, h1(paste("2018 United Nations List of Protected Areas of", country, sep = " "), align = "center"), h2(paste("Information extracted from the July 2018 WDPA release",sep = " "), align = "center"), h6(paste("This list was compiled from data based on the July 2018 version of the WDPA. In addition to protected areas with spatial data in the WDPA, the 2018 version of the UN List includes sites for which no spatial data is available but which have been provided by the relevant country. Management effectiveness information is extracted from GD-PAME. Both databases are updated and released via www.protectedplanet.net on a monthly basis. The depiction and use of boundaries and related data shown on maps are not guaranteed to be error free nor do they necessarily imply official endorsement or acceptance by the United Nations."), align = "center")),
      column(12, align="center",div(style="display: inline-block;",img(src=paste0("C:/Users/OsgurM/OneDrive - WCMC/WDPA/UN_Lists/Maps/", i, ".png"), height= height, width=width))),
      column(12, HTML(Table),div())))
  
  setwd("P:/PROGRAMMES/PROTECTED AREAS/STAFF FOLDERS/Marine/Osgur_to_Marine")
  save_kable(Table , file = paste(i,".html", sep = ""), self_contained = T)
  
  rm(Table, CountryData,CountryData2, CountryData3)
  gc()
  
}

#Problem Countries in terms of size
#DEU - 47
#SWE - 165
#usa


