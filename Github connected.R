library(readxl)
EP08<- read_excel("~/Desktop/IR/2008 Electorate Profile .xls")
 EP08 <- EP08[1:63,]                                                                                                   
 EP08 <- EP08[,1:20]
 