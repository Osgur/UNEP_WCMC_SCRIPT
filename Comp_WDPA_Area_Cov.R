##### Code to compare the change in coverage on land and oceans between two differing WDPA datasets

Older_FilePath <- "C:/Users/OsgurM/Downloads/Monthly_PA_National_Coverage__Aug2014.csv" ## set the path to the older dataset. Remember to use / rather than \
Older <- read.csv(Older_FilePath) ## read in the older wdpa file
Newer_FilePath <- "C:/Users/OsgurM/Downloads/Monthly_PA_National_Coverage__July2018.csv" ## set the path to the newer dataset. Remember to use / rather than \
Newer <- read.csv(Newer_FilePath) # read in the newer dataset

Output <- merge(Newer,Older, by = "ISO3") # combine both datasets for analysis
Output$LandChange <- Output$pa_land_area.x - Output$pa_land_area.y # identify the change in protected land area between the two datasets for each country
Output$LandChangePercentage <- Output$LandChange / Output$land_area.x *100 # make proportion as a percentage of the newer dataset possible land coverage. 

Output$MarineChange <- Output$pa_marine_area.x - Output$pa_marine_area.y# identify the change in protected ocean area between the two datasets for each country
Output$MarineChangePercentage <- Output$MarineChange / Output$marine_area.x *100 # make proportion as a percentage of the newer dataset possible ocean coverage. 

colnames(Output) <- c(gsub(".y", gsub(".csv", "",gsub(".*__","",Older_FilePath)), colnames(Output))) ## change the column names to reference the dataset being used
colnames(Output) <- c(gsub(".x", gsub(".csv", "",gsub(".*__","",Newer_FilePath)), colnames(Output))) ## change the column names to reference the dataset being used


setwd("C:/Users/OsgurM/OneDrive - WCMC/WDPA/Cover_Change") ## set the folder you would like the data saved to

write.csv(Output, "Change_Stats.csv", row.names = F) # write the output as a csv file 


