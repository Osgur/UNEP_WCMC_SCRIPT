#load required libraries
library(dplyr)


#load the txt file exported from ArcGIS containing information on all points
AllSpecies <- read.csv("C:/Users/osgurm/Downloads/ALLSpecies.txt")
AllBackup <- AllSpecies

AllSpecies <- na.omit(AllSpecies)




##load the txt file exported from ArcGIS containing information on all points that intersect with the WDPA
SpeciesInt <- read.csv("C:/Users/osgurm/Downloads/SpeciesInt.txt")
IntBackup <- SpeciesInt

SpeciesInt <- na.omit(SpeciesInt)



#Summarise all points to understand how many points per species
TotalSummary <- AllSpecies %>%
  group_by(binomial) %>% # this line groups summary by individual species
  summarise(No_Pnt = n()) # number of observations per species

colnames(TotalSummary) <- c("species", "No_Pnts")

#Summarise intersected points to understand how many points per species
IntersectionSummary <- SpeciesInt %>%
  group_by(binomial) %>% # this line groups summary by individual species
  summarise(No_Pnt = length(unique(FID_AllSpe)), # number of pnts per species intersected
            No_WDPA = length(unique(WDPAID)) # number of WDPA sites each species intersects with
  )

colnames(IntersectionSummary) <- c("species", "No_Protected_Pnts", "No_Protected_Areas")

#identify the countries that each species intersects with
countries <- SpeciesInt %>%
  group_by(binomial) %>%
  distinct(Name)

#Group this country info into one column
countries <- aggregate(countries$Name, list(countries$binomial), paste, collapse=";")

#changing column names
colnames(countries) <- c("species", "Country")

#merge all species info with intersected species info to allow for comparisons to be made
TotalSummary <- merge(TotalSummary, IntersectionSummary, by = "species", all = T)

#changing column names
colnames(TotalSummary) <- c("species", "total_pnts", "protected_pnts", "number_of_Protected_areas")

#set NA values as 0 to allow for percentages to be calculated
TotalSummary[is.na(TotalSummary)] <- 0

#calculate percentage of points per species protected
TotalSummary$percentage_pnts_protected <- TotalSummary$protected_pnts/TotalSummary$total_pnts *100


#Merge summary data with countries the protected points are found in
TotalSummary <- merge(TotalSummary, countries , by = "species" , all = T)

TotalSummary$protected_status <- TotalSummary$protected_pnts > 0

#save the output
write.csv(TotalSummary, "BGCI_Output.csv", row.names = F)

