table <- read_excel("C:/Users/OsgurM/OneDrive - WCMC/PAP/Copy of GDPAME_assessment_CHE_prefilled.xlsx")
tab_Sub <- table[table$Cycle %in% c(1,6),]
tab_add <- table[!table$Cycle %in% c(1,6),]
test <- data.frame()


for( i in 1:nrow(tab_Sub)){
  output <- tab_Sub[i,]
  row <- tab_Sub[i,]
  reduce <- row$Cycle
  reduction <- row$yr_ass
  while(reduction  > row$status_year){
    reduction <- reduction- reduce
    if(reduction >= row$status_year){
    print(reduction)}
    newline <- row
    newline$yr_ass <- reduction
    if(reduction >= row$status_year){
    output <- rbind(output, newline)}
  }
  test <- rbind(test, output)
  
}

Final_Output <- rbind(test, tab_add)
