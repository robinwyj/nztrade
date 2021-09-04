EP08<- read.csv("https://raw.githubusercontent.com/robinwyj/nztrade/main/ElectorateProfile08")
 EP08 <- EP08[1:63,]                                                                                                   
 EP08 <- EP08[,1:20]
 
 EP08$Mining [EP08$Mining=="..C"] <- 0
 
 # Error in asnumeirc function
 EP08 <- for (i in 3:20) {
   EP08[,i] <- as.numeric(EP08[,i])
   EP08[,i] <-  EP08[,i]/ sum( EP08[,i])
 }
 
 
 EP17 <- read_excel("~/Downloads/Electorate-profiles---raw-data.xlsx")
 EP17 <- EP17 [,2:20]
 EP17 <- for (i in 2:22) {
   EP08[,i] <- as.numeric(EP17[,i])
   EP08[,i] <-  EP17[,i]/ sum( EP17[,i])
 }
 
 

 
 
 
 library(readxl)
 electorate_profiles_2020_data_file <- read_excel("~/Downloads/electorate_profiles_2020-data_file.xlsx", sheet=8)
 
 EP20<- electorate_profiles_2020_data_file
 
 EP20 <- EP20[c(2:66),]
 EP20 <- EP20[,c(0:20)]
 colnames(EP20)[1] <- "Electorate"
 
 library(readr)
 winning_electorate_candidates <- read_csv("~/Downloads/winning-electorate-candidates.csv", 
                                           skip = 1)
 vote2020<-(winning_electorate_candidates)
 
 vote2020<- vote2020[,c(1,3)]
 
 library(readr)
 winning_electorate_candidates_2 <- read_csv("~/Downloads/winning-electorate-candidates-2.csv", 
                                             skip = 1)
 vote2017 <- winning_electorate_candidates_2
 vote2017 <- vote2017[,c(1,3)]
 
 votechange<- merge (vote2017,vote2020, by="Electoral District")
 votechange$incumbent <- votechange$Party.x == votechange$Party.y
 colnames(votechange)[2]<- "2017"
 colnames(votechange)[3]<- "2020" 
 
 write.csv(votechange, "2017-2020 votechange")
 write.csv(EP17, "ElectorateProfile17")
 write.csv(EP20, "ElectorateProfile20")
 write.csv(EP08, "ElectorateProfile08")
 read.csv("https://raw.githubusercontent.com/robinwyj/nztrade/main/ElectorateProfile17")