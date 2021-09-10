EP08<- read.csv("https://raw.githubusercontent.com/robinwyj/nztrade/main/ElectorateProfile08")
 EP08 <- EP08[1:63,]                                                                                                   
 EP08 <- EP08[,2:20]
 
 EP08 <- for (i in 2:20) {
   EP08[,i] <- as.numeric(EP08[,i])
   EP08[,i] <-  EP08[,i]/ sum( EP08[,i])
 }
 colnames(EP08) <- paste0("2008",colnames(EP08))
 colnames(EP08)[1] <- "Electorate"
 
 EP17 <- read.csv("ep17.csv")
 EP17 <- EP17[,2:ncol(EP17)]

 as.numeric(EP17[,2])
  EP17[,2]/ sum( EP17[,2])
 
 EP17 <- for (i in 2:21) {
    EP17[,i] <- as.numeric(EP17[,i])
    EP17[,i] <-  EP17[,i]/ sum( EP17[,i])
 }
 
 EP17<-EP17[,1:19]
 colnames(EP17) <- paste0("2017",colnames(EP17))
 colnames(EP17)[1] <- "Electorate"
 

 EP20<-read.csv("ep20.csv")
 EP20 <- EP20[c(2:66),]
 EP20 <- EP20[,c(2:20)]
colnames(EP20)
EP20[,1]<-Electorate
 EP20 <- for (i in 2:21) {
    EP20[,i] <- as.numeric(EP20[,i])
    EP20[,i] <-  EP20[,i]/ sum( EP20[,i])
 }
colnames(EP20) <- paste0("2020",colnames(EP20))
colnames(EP20)[1] <- "Electorate"
 
 votechange<-read.csv("2017-2020 votechange")
 votechange<- votechange[,2:5]
 colnames(votechange)[1] <- "Electorate"
 colnames(votechange)[2]<- "2017"
 colnames(votechange)[3]<- "2020" 

 df<- merge (EP08, EP17,by="Electorate")
 df1<- merge (EP20, df,by="Electorate")
 finaldf<-merge (votechange, df1, by="Electorate")
 
 
 
 library(readr)
 
 e2014 <- read_csv("~/Downloads/e9_part6.csv", 
                      skip = 1)
 e2008 <- read_csv("~/Downloads/e9_part6-2.csv", 
                        skip = 1)
 vote2014<- e2014[,c(1,3,6)]
 vote2008<- e2008[,c(1,3,6)]
 colnames(vote2014)[1]<-"Electorate"
 colnames(vote2014)[2]<-"2014"
 colnames(vote2014)[3]<-"2014 percentage"
 colnames(vote2008)[1]<-"Electorate"
 colnames(vote2008)[2]<-"2008"
 colnames(vote2008)[3]<-"2008 percentage"
 votechange<- merge (vote2008, vote2014, by="Electorate")
 votechange$incumbent <- votechange$"2014" == votechange$"2008"
 
 winning_electorate_candidates <- read_csv("~/Downloads/winning-electorate-candidates.csv", 
                                           skip = 1)
 vote2020<-(winning_electorate_candidates)
 
 vote2020<- vote2020[,c(1,3,6)]
 
 
 winning_electorate_candidates_2 <- read_csv("~/Downloads/winning-electorate-candidates-2.csv", 
                                             skip = 1)
 vote2017 <- winning_electorate_candidates_2
 vote2017 <- vote2017[,c(1,3,6)]
 
 colnames(vote2017)[1]<-"Electorate"
 colnames(vote2017)[2]<-"2017"
 colnames(vote2017)[3]<-"2017 percentage"
 colnames(vote2020)[1]<-"Electorate"
 colnames(vote2020)[2]<-"2020"
 colnames(vote2020)[3]<-"2020 percentage"
 votechange1 <- merge (vote2014, vote2017, by="Electorate")
 votechange1$incumbent <- votechange1$"2014" == votechange1$"2017"
 votechange2 <- merge (vote2017, vote2020, by="Electorate")
 votechange2$incumbent <- votechange2$"2017" == votechange2$"2020"
 
 votefirst <- merge (votechange, votechange1, by="Electorate")
 votesecond <- merge (votefirst, votechange2, by="Electorate")
 FINALVOTE <- votesecond[,c(1:6,9:11,14:16)]
 colnames(FINALVOTE)[6]<- "Incumbent2008-2014"
   colnames(FINALVOTE)[9]<- "Incumbent2014-2017"
   colnames(FINALVOTE)[12]<- "Incumbent2017-2020"
  write.csv(FINALVOTE, "Votechange 2008-2020")
   
 Trend<-read.csv("Votechange 2008-2020")
 Trend <- Trend[,2:13]
 MASTERSHEET <- merge(finaldf, Trend, by="Electorate")
 
 #Below are codes comiling data and writing csv.
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
 colnames(votechange)[1] <- "Electorate"
 votechange$incumbent <- votechange$Party.x == votechange$Party.y
 colnames(votechange)[2]<- "2017"
 colnames(votechange)[3]<- "2020" 
 
 df<- merge (EP08, EP17,EP20, votechange, by="Electorate")
 
 write.csv(votechange, "2017-2020 votechange")
 write.csv(EP17, "ElectorateProfile17")
 write.csv(EP20, "ElectorateProfile20")
 write.csv(EP08, "ElectorateProfile08")
 read.csv("https://raw.githubusercontent.com/robinwyj/nztrade/main/ElectorateProfile17")
 
 rawEP17 <- read_excel("~/Downloads/Electorate-profiles---raw-data.xlsx")
 rawEP17 <- rawEP17 [,1:20]
 write.csv(rawEP17, "ep17.csv")
 
 library(readxl)
 electorate_profiles_2020_data_file <- read_excel("~/Downloads/electorate_profiles_2020-data_file.xlsx", sheet=8)
 rawEP20<- electorate_profiles_2020_data_file
 
 write.csv (rawEP20, "ep20.csv")