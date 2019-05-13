library(dplyr)

Pame <- read.csv("C:/Users/OsgurM/Downloads/protectedplanet-pame-2019-03-18.csv")


#order by metadata id so that we remover anything after the first row
Pame <- Pame[order(Pame$metadata_id),]

#find unique rows based on particular columns
Pame <- Pame %>%
  distinct(url,year,methodology,wpda_id,designation, .keep_all = TRUE)


write.csv(Pame_Uni, "PAME_DUP_Rm.csv", row.names = F)
