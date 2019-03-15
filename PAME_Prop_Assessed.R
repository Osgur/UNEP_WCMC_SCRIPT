# Install the necessary library packages
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
library(kableExtra)
library(dplyr)


options(scipen=999)

setwd("//wcmc-data-03/WDPA/2_July_2018_Analyses/Section1_Global_Analyses/1_Data/WDPA_with_Regions_2018.gdb")
ogrListLayers(getwd())
CountryNames <- read.csv("C:/Users/OsgurM/OneDrive - WCMC(3)/Data Management/lu_CountryNames_Jan2018.csv")
CountryNames <- CountryNames[,1:2]

poly <- data.frame(read_sf(getwd(), layer = "WDPA_poly_Jul2018_Regions_Restricted"))
poly <- poly[,1:30]
point <- data.frame(read_sf(getwd(), layer = "WDPA_point_Jul2018_Regions_Restricted"))
point <- point[,1:28]
WDPA <- rbind.fill(poly,point)
rm(poly, point)
gc()
PAME <- read_excel("C:/Users/OsgurM/OneDrive - WCMC(3)/WDPA/UN_Lists/PAME.xlsx")

WDPA <- WDPA[, c("WDPAID", "NAME", "DESIG_ENG", "DESIG_TYPE", "IUCN_CAT", "REP_AREA", "STATUS", "STATUS_YR","ISO3",  "PARENT_ISO3")]

PAME <- PAME[,-1]
PAME <- unique(PAME)

# SummaryPAME <- data.frame(table(PAME$wdpa_id))
# setwd("C:/Users/OsgurM/OneDrive - WCMC/WDPA/UN_Lists/Output")
# write.csv(SummaryPAME, "PAME_WDPAFREQ.csv")

WDPA <- WDPA[with(WDPA,order(WDPAID, -rank(REP_AREA), decreasing = F)), ]
##Fix Parent ISO Issues
naissues <- subset(WDPA, is.na(WDPA$PARENT_ISO3))
WDPA[which(WDPA$ISO3 == "SHN"), "PARENT_ISO3"] <- "GBR"
WDPA[which(WDPA$ISO3 == "CHN"), "PARENT_ISO3"] <- "CHN"
WDPA[which(WDPA$ISO3 == "EST"), "PARENT_ISO3"] <- "EST"
naissues <- subset(WDPA, is.na(WDPA$PARENT_ISO3))


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
            PARENT_ISO3 = paste(unique(PARENT_ISO3)),
            ISO3 = paste(unique(ISO3)))

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
            PARENT_ISO3 = paste(unique(PARENT_ISO3)),
            ISO3 = paste(unique(ISO3)))


#Recombine Datasets
WDPA <- rbind(WDPA, WDPA_Multi, WDPA_Multi_CHN)


####PAME manipulation
PAME <- ddply(PAME, .(wdpa_id, methodology ), summarise, year = min(year)) ### removes duplicate methodology and assigns minimum year
PAME <- ddply(PAME, .(wdpa_id ), summarise,methodology = paste(unique(methodology), collapse = "; "), year = paste(unique(year), collapse = "; "))


### Join the WDPA with PAME

Join <- merge(WDPA, PAME, by.x = "WDPAID", by.y = "wdpa_id", all = T)


Countries <- unique(WDPA$ISO3) ## identify unique countries in the dataset
Countries <- sort(Countries)
Countries <- Countries[-c(grep(";", Countries))]
Countries <- na.omit(Countries)


Output <- data.frame()
for( i in Countries){
  CountryData <- Join[grep(i , Join$ISO3),] # subset by country PARENT_ISO3
  
  PAME_ASSESSED <- CountryData[!is.na(CountryData$methodology),]
  PAME_ASSESSED_LENGTH <- nrow(PAME_ASSESSED)
  TOTAL_Protected <- nrow(CountryData)
  CountryDetail <- data.frame(cbind(i, PAME_ASSESSED_LENGTH, TOTAL_Protected, PAME_ASSESSED_LENGTH/TOTAL_Protected*100))
  colnames(CountryDetail) <- c("Country", "Number Assessed", "Total Protected Areas", "Perc_Assessed")
  CountryDetail$Perc_Assessed <- as.numeric(as.character(CountryDetail$Perc_Assessed))
    
  CountryDetail <- transform(CountryDetail, group=cut(Perc_Assessed,  breaks=c(-Inf,0.000001, 10, 30, 60,Inf),
                                            labels=c('No Assessments', "<10", "10_30","30_60", ">60")))
  
  Output <- rbind(Output, CountryDetail)
}


setwd("C:/Users/OsgurM/OneDrive - WCMC(3)/WDPA/UN_Lists")
write.csv(Output, "PAME_Prop_Assessed.csv", row.names = F)
