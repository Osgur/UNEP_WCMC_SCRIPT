
Oversea <- read_excel("C:/Users/OsgurM/OneDrive - WCMC/Oversea_.xlsx")
Oversea <- Oversea[,1:2]

ISO <- read_excel("C:/Users/OsgurM/OneDrive - WCMC/ISO3List.xlsx")

Coral <- read_excel("C:/Users/OsgurM/Downloads/Coral_Area.xlsx")
for(i in 1:length(Coral$ISO)){
  Iso_to_Match <- Coral$ISO[i]
  if(Iso_to_Match %in% Oversea$ISO3){
    Coral$ISO[i] <-  Oversea[which(Oversea$ISO3== Iso_to_Match), 1]
  }
}
for(n in 1:length(Coral$ISO)){
  Country_to_Match <- Coral$ISO[n]
  Coral$`Country Name`[n] <-  ISO[which(ISO$ISO3== Country_to_Match), 1]
}



Funding <- read_csv("C:/Users/OsgurM/Downloads/Funding_Allocation.csv")
Funding <- Funding[ - c(which( grepl(";" , Funding$`Country (new)`) == TRUE)),]
Funding$`Total Project Cost (new)` <- as.numeric(as.character(Funding$`Total Project Cost (new)`))

Funding_Sum <- aggregate(Funding$`Total Project Cost (new)`, by=list(Category=Funding$`Country (new)`), FUN=sum, na.action = NULL)


Funding_Sum <- aggregate(Funding$`Total Project Cost (new)`, list(Funding$`Country (new)`), FUN=sum, na.rm = T)

Funding_Freq <- data.frame(table(Funding$`Country (new)`))


Funding_Data <- merge(Funding_Freq, Funding_Sum, by.x = "Var1", by.y = "Group.1")

#Fundin_Countries <- as.character(unique(Funding_Data$Var1))

Coral <- data.frame(tapply(Coral$`Reported area KM2`, unlist(Coral$`Country Name`), sum))


x<- merge(Funding_Data,Coral, by.x = "Var1", by.y = 0)
