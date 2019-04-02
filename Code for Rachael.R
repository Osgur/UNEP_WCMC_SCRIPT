

Max <- read.csv("C:/Users/OsgurM/Downloads/ICRI Funding Analysis MASTER for analysis_MAX.csv")
Max <- subset(Max, Max$Start.Date != 2017)
  Max <- subset(Max, Max$Start.Date != "Data not available")
  Max <- subset(Max, Max$Start.Date != "")
  

  levels(Max$Regional.seas.update)
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)=="West and Central Africa " ] <- "West and Central Africa"
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)=="Pacific "] <- "Pacific"
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)=="North-West Atlantic "] <- "North-West Atlantic"
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)=="Red Sea and Gulf of Aden " ] <- "Red Sea and Gulf of Aden"
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)=="South-East Pacific " ] <- "South-East Pacific"
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)=="West and Central Africa " ] <- "West and Central Africa"
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)==" Pacific; Wider Caribbean" ] <- "Pacific; Wider Caribbean"
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)=="South-East Pacific; Wider Caribbean; North-East Pacific; " ] <- "South-East Pacific; Wider Caribbean; North-East Pacific"
  levels(Max$Regional.seas.update)[levels(Max$Regional.seas.update)=="South Asian Seas " ] <- "South Asian Seas"
  
  Max$Total.Project.Cost..new. <- as.character(Max$Total.Project.Cost..new.)
  Max$Total.Project.Cost..new. <- as.numeric(Max$Total.Project.Cost..new.)
  Max <- na.omit(Max)
  Max$Regional.seas.update <- droplevels(Max$Regional.seas.update)
  levels(Max$Regional.seas.update)
  Max_Cost_Region <- data.frame(tapply(Max$Total.Project.Cost..new.,Max$Regional.seas.update , sum))  
  

  
    
  Min <- read.csv("C:/Users/OsgurM/Downloads/ICRI Funding Analysis MASTER for analysis_MIN.csv")
  Min <- subset(Min, Min$Start.Date != 2017)
  Min <- subset(Min, Min$Start.Date != "Data not available")
  Min <- subset(Min, Min$Start.Date != "")
  
  Min$Total.Project.Cost..new. <- as.character(Min$Total.Project.Cost..new.)
  Min$Total.Project.Cost..new. <- as.numeric(Min$Total.Project.Cost..new.)
  
  
  levels(Min$Regional.seas.update)
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)=="West and Central Africa " ] <- "West and Central Africa"
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)=="Pacific "] <- "Pacific"
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)=="North-West Atlantic "] <- "North-West Atlantic"
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)=="Red Sea and Gulf of Aden " ] <- "Red Sea and Gulf of Aden"
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)=="South-East Pacific " ] <- "South-East Pacific"
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)=="West and Central Africa " ] <- "West and Central Africa"
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)==" Pacific; Wider Caribbean" ] <- "Pacific; Wider Caribbean"
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)=="South-East Pacific; Wider Caribbean; North-East Pacific; " ] <- "South-East Pacific; Wider Caribbean; North-East Pacific"
  levels(Min$Regional.seas.update)[levels(Min$Regional.seas.update)=="South Asian Seas " ] <- "South Asian Seas"
  levels(Min$Regional.seas.update)
  
  Min$Total.Project.Cost..new. <- as.character(Min$Total.Project.Cost..new.)
  Min$Total.Project.Cost..new. <- as.numeric(Min$Total.Project.Cost..new.)
  Min <- na.omit(Min)
  
  Min_Multiple <- Min[grep(";", Min$Regional.seas.update),]
  #Min <- Min[-c(grep(";", Min$Regional.seas.update)),]
  
  Min$Regional.seas.update <- droplevels(Min$Regional.seas.update)
  levels(Min$Regional.seas.update)
  
  Min_Cost_Region <- data.frame(tapply(Min$Total.Project.Cost..new.,Min$Regional.seas.update , sum))  

  colnames(Max_Cost_Region)<- "Total_Cost"
  colnames(Min_Cost_Region)<- "Total_Cost"
  
  write.csv(Max_Cost_Region, "Max_Cost_Region.csv")
  write.csv(Min_Cost_Region, "Min_Cost_Region.csv")  
  
  