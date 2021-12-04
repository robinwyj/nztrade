#Electorate Profile for yeras 2006, 2013,2018
EP06<- read.csv("https://raw.githubusercontent.com/robinwyj/nztrade/main/ElectorateProfile08")
 EP06 <- EP06[1:63,]                                                                                                   
 EP06 <- EP06[,2:20]
 
 EP06 <- for (i in 2:20) {
   EP06[,i] <- as.numeric(EP06[,i])
   EP06[,i] <-  EP06[,i]/ sum( EP06[,i])
 }
 colnames(EP06) <- paste0("2006",colnames(EP06))
 colnames(EP06)[1] <- "Electorate"
 
 EP13 <- read.csv("ep17.csv")
 EP13 <- EP13[,2:ncol(EP13)]

 as.numeric(EP13[,2])
  EP13[,2]/ sum( EP13[,2])
 
 EP13 <- for (i in 2:21) {
    EP13[,i] <- as.numeric(EP13[,i])
    EP13[,i] <-  EP13[,i]/ sum( EP13[,i])
 }
 
 EP13<-EP13[,1:19]
 colnames(EP13) <- paste0("2013",colnames(EP13))
 colnames(EP13)[1] <- "Electorate"
 

 EP18<-read.csv("ep20.csv")
 EP18 <- EP18[c(2:66),]
 EP18 <- EP18[,c(2:20)]
colnames(EP18)
EP18[,1]<-Electorate
 EP18 <- for (i in 2:21) {
    EP18[,i] <- as.numeric(EP18[,i])
    EP18[,i] <-  EP18[,i]/ sum( EP18[,i])
 }
colnames(EP18) <- paste0("2018",colnames(EP18))
colnames(EP18)[1] <- "Electorate"
 
dataset2 <- EP06
write.csv(EP06, "Electorate Profile 2006")
 



# Vote results for the 2008, 2011, 2014 elections
library(readr)

e08 <- read_csv("~/Downloads/e9_part6-4.csv", 
                skip = 1)

e11 <- read_csv("~/Downloads/e9_part6-5.csv", 
                skip = 1)

e14 <- read_csv("~/Downloads/e9_part6.csv", 
                skip = 1)

e17 <- read_csv("~/Downloads/winning-electorate-candidates-3.csv", 
                skip = 1)

e08 <- e08 [,c(1,3,6)]
colnames(e08)[1]<-"Electorate"
e11 <- e11 [,c(1,3,6)]
colnames(e11)[1]<-"Electorate"
e14 <- e14 [,c(1,3,6)]
colnames(e14)[1]<-"Electorate"
e17 <- e17 [,c(1,3,6)]
colnames(e17)[1]<-"Electorate"

colnames(e08)[2] <- "Party 2008"
colnames(e11)[2] <- "Party 2011"
colnames(e14)[2] <- "Party 2014"
colnames(e17)[2] <- "Party 2017"

colnames(e08)[3] <- "Percentage of Vote Recieved 2008"
colnames(e11)[3] <- "Percentage of Vote Recieved 2011"
colnames(e14)[3] <- "Percentage of Vote Recieved 2014"
colnames(e17)[3] <- "Percentage of Vote Recieved 2017"

e08$`Percentage of Vote Recieved 2008` <- gsub("%", "", e08$`Percentage of Vote Recieved 2008`)
e08$`Percentage of Vote Recieved 2008`<- as.numeric(e08$`Percentage of Vote Recieved 2008`)

e11$`Percentage of Vote Recieved 2011` <- gsub("%", "", e11$`Percentage of Vote Recieved 2011`)
e11$`Percentage of Vote Recieved 2011`<- as.numeric(e11$`Percentage of Vote Recieved 2011`)

e14$`Percentage of Vote Recieved 2014` <- gsub("%", "", e14$`Percentage of Vote Recieved 2014`)
e14$`Percentage of Vote Recieved 2014`<- as.numeric(e14$`Percentage of Vote Recieved 2014`)

e17$`Percentage of Vote Recieved 2017` <- gsub("%", "", e17$`Percentage of Vote Recieved 2017`)
e17$`Percentage of Vote Recieved 2017`<- as.numeric(e17$`Percentage of Vote Recieved 2017`)


dataset1 <- merge (e08,e11, by="Electorate")
dataset1$"Incumbent 2008-2011" <- dataset1$"Party 2008" == dataset1$"Party 2011"
dataset1$"Winning Margin Percentage 2008-2011" <- dataset1[,5]- dataset1[,3]

dataset1 <- merge (dataset1,e14, by="Electorate")
dataset1$"Incumbent 2011-2014" <- dataset1$"Party 2011" == dataset1$"Party 2014"
dataset1$"Winning Margin Percentage 2011-2014" <- dataset1[,9]- dataset1[,5]

dataset1 <- merge (dataset1,e17, by="Electorate")
dataset1$"Incumbent 2014-2017" <- dataset1$"Party 2014" == dataset1$"Party 2017"
dataset1$"Winning Margin Percentage 2014-2017" <- dataset1[13]- dataset1[,9]
colnames(dataset1)[15] <- "Winning Margin Percentage 2014-2017"

write.csv(dataset1, "Election Results 2008-2017")







 
 
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
 
 df<- merge (EP06, EP13, by="Electorate")
 
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
 
 EP08_list <- split(EP08, seq(nrow(EP08)))
 EP08_list
 
 list(MASTERSHEET$Electorate)
 
 
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
 
 #Obsolete vote data
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
 
 
 
 # Test
 #load datasets. We are using three years but there are more. 
library(readr)
 
 icio_95 <- read.csv("ICIO2016_1995.csv")
 icio_00 <- read.csv("~/Documents/ICIO2016_2000.csv")
 icio_05 <- read.csv("icio_2016/ICIO2016_2005.csv")
 icio_07 <- read.csv("~/Documents/ICIO2016_2007.csv")
 icio_08 <- read.csv("~/Desktop/ICIO2021_2008.csv")
 icio_11 <- read.csv("~/Desktop/ICIO2021_2011.csv")
 icio_14 <- read.csv("~/Desktop/ICIO2021_2014.csv")
 
 
 
 
 all_cty <- unique(substr(colnames(icio_07), 1, 3))[-1]
 all_cty <- all_cty[-c(72:length(all_cty))]
 all_cty <- all_cty[!all_cty== "NZL"]
 

  icio_95_chn_cn2 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN2"), 
                                       substr(colnames(icio_95), 1, 3) == "NZL"])
 
 icio_95_chn_cn3 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN3"), 
                                       substr(colnames(icio_95), 1, 3) == "NZL"])
 
  icio_00_chn_cn2 <- data.frame(icio_00[substr(icio_00$X, 1, 3) %in% c ("CN2"), 
                                       substr(colnames(icio_00), 1, 3) == "NZL"])
 
 icio_00_chn_cn3 <- data.frame(icio_00[substr(icio_00$X, 1, 3) %in% c ("CN3"), 
                                       substr(colnames(icio_00), 1, 3) == "NZL"])
 
 icio_05_chn_cn2 <- data.frame(icio_05[substr(icio_07$X, 1, 3) %in% c ("CN2"), 
                                       substr(colnames(icio_05), 1, 3) == "NZL"])
 
 icio_05_chn_cn3 <- data.frame(icio_05[substr(icio_07$X, 1, 3) %in% c ("CN3"), 
                                       substr(colnames(icio_05), 1, 3) == "NZL"])
 
 icio_07_chn_cn2 <- data.frame(icio_07[substr(icio_07$X, 1, 3) %in% c ("CN2"), 
                                       substr(colnames(icio_07), 1, 3) == "NZL"])
 
 icio_07_chn_cn3 <- data.frame(icio_07[substr(icio_07$X, 1, 3) %in% c ("CN3"), 
                                       substr(colnames(icio_07), 1, 3) == "NZL"])
 
  icio_08_chn_cn1 <- data.frame(icio_08[substr(icio_08$X, 1, 3) %in% c ("CN1"), 
                                       substr(colnames(icio_08), 1, 3) == "NZL"])
 
 icio_08_chn_cn2 <- data.frame(icio_08[substr(icio_08$X, 1, 3) %in% c ("CN2"), 
                                       substr(colnames(icio_08), 1, 3) == "NZL"])
 icio_08_chn_cn1 <- icio_08_chn_cn1[,1:45]
 icio_08_chn_cn2 <- icio_08_chn_cn2[,1:45]
 
 icio_11_chn_cn1 <- data.frame(icio_11[substr(icio_11$X, 1, 3) %in% c ("CN1"), 
                                       substr(colnames(icio_11), 1, 3) == "NZL"])
 
 icio_11_chn_cn2 <- data.frame(icio_11[substr(icio_11$X, 1, 3) %in% c ("CN2"), 
                                       substr(colnames(icio_11), 1, 3) == "NZL"])
 icio_11_chn_cn1 <- icio_11_chn_cn1[,1:45]
 icio_11_chn_cn2 <- icio_11_chn_cn2[,1:45]
 
 
 icio_14_chn_cn1 <- data.frame(icio_14[substr(icio_14$X, 1, 3) %in% c ("CN1"), 
                                       substr(colnames(icio_14), 1, 3) == "NZL"])
 
 icio_14_chn_cn2 <- data.frame(icio_14[substr(icio_14$X, 1, 3) %in% c ("CN2"), 
                                       substr(colnames(icio_14), 1, 3) == "NZL"])
 
 icio_14_chn_cn1 <- icio_14_chn_cn1[,1:45]
 icio_14_chn_cn2 <- icio_14_chn_cn2[,1:45]
 
 
 

 icio_95_chn_cn2 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN2"), 
                                       substr(colnames(icio_95), 1, 3) == "NZL"])
 icio_95_chn_cn2_sum <- rowSums(icio_95_chn_cn2)
icio_95_chn_cn3 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN3"), 
                                      substr(colnames(icio_95), 1, 3) == "NZL"])
icio_95_chn_cn3_sum <- rowSums(icio_95_chn_cn3)
chn_nzl_exp_95 <- icio_95_chn_cn2_sum + icio_95_chn_cn3_sum
chn_nzl_exp_95 <- data.frame(industry = substr(colnames(icio_95_chn_cn2), 
                                               5, nchar(colnames(icio_95_chn_cn2))), 
                             exp_val = chn_nzl_exp_95)




icio_00_chn_cn2_sum <- rowSums(icio_00_chn_cn2)
icio_00_chn_cn3_sum <- rowSums(icio_00_chn_cn3)
chn_nzl_exp_00 <- icio_00_chn_cn2_sum + icio_00_chn_cn3_sum
chn_nzl_exp_00 <- data.frame(industry = substr(colnames(icio_00_chn_cn2), 
                                               5, nchar(colnames(icio_00_chn_cn2))), 
                             exp_val = chn_nzl_exp_00)


icio_05_chn_cn2_sum <- rowSums(icio_05_chn_cn2)
icio_05_chn_cn3_sum <- rowSums(icio_05_chn_cn3)
chn_nzl_exp_05 <- icio_05_chn_cn2_sum + icio_05_chn_cn3_sum
chn_nzl_exp_05 <- data.frame(industry = substr(colnames(icio_05_chn_cn2), 
                                               5, nchar(colnames(icio_05_chn_cn2))), 
                             exp_val = chn_nzl_exp_05)


icio_07_chn_cn2 <- data.frame(icio_07[substr(icio_07$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_07), 1, 3) == "NZL"])
icio_07_chn_cn2_sum <- rowSums(icio_07_chn_cn2)
icio_07_chn_cn3 <- data.frame(icio_07[substr(icio_07$X, 1, 3) %in% c ("CN3"), 
                                      substr(colnames(icio_07), 1, 3) == "NZL"])
icio_07_chn_cn3_sum <- rowSums(icio_07_chn_cn3)
chn_nzl_exp_07 <- icio_07_chn_cn2_sum + icio_07_chn_cn3_sum
chn_nzl_exp_07 <- data.frame(industry = substr(colnames(icio_07_chn_cn2), 
                                               5, nchar(colnames(icio_07_chn_cn2))), 
                             exp_val = chn_nzl_exp_07)


icio_08_chn_cn1_sum <- rowSums(icio_08_chn_cn1)
icio_08_chn_cn2_sum <- rowSums(icio_08_chn_cn2)
chn_nzl_exp_08 <- icio_08_chn_cn1_sum + icio_08_chn_cn2_sum
chn_nzl_exp_08 <- data.frame(industry = substr(colnames(icio_08_chn_cn2), 
                                               5, nchar(colnames(icio_08_chn_cn2))), 
                             exp_val = chn_nzl_exp_08)

icio_11_chn_cn1_sum <- rowSums(icio_11_chn_cn1)
icio_11_chn_cn2_sum <- rowSums(icio_11_chn_cn2)
chn_nzl_exp_11 <- icio_11_chn_cn1_sum + icio_11_chn_cn2_sum
chn_nzl_exp_11 <- data.frame(industry = substr(colnames(icio_11_chn_cn2), 
                                               5, nchar(colnames(icio_11_chn_cn2))), 
                             exp_val = chn_nzl_exp_11)

icio_14_chn_cn1_sum <- rowSums(icio_14_chn_cn1)
icio_14_chn_cn2_sum <- rowSums(icio_14_chn_cn2)
chn_nzl_exp_14 <- icio_14_chn_cn1_sum + icio_14_chn_cn2_sum
chn_nzl_exp_14 <- data.frame(industry = substr(colnames(icio_14_chn_cn2), 
                                               5, nchar(colnames(icio_14_chn_cn2))), 
                             exp_val = chn_nzl_exp_14)


# The names of the columns between 2016 datasets and 2021 datasets are differnt? Is there a way to map the names on the directory to this rather than changing it one by one?
tradechange <- merge (chn_nzl_exp_05, chn_nzl_exp_08, by ="industry")
colnames(tradechange)[2] <- "exp05"
colnames(tradechange)[3] <- "exp08"
tradechange$"change05-08" <- tradechange[,3] - tradechange[,2]

tradechange <- merge (tradechange, chn_nzl_exp_05, by="industry")
colnames(tradechange)[5] <- "exp05"
tradechange$"change00-05" <- tradechange[,5] - tradechange [,3]

tradechange <- merge (tradechange, chn_nzl_exp_07, by="industry")
colnames(tradechange)[7] <- "exp07"
tradechange$"change00-07" <- tradechange[,7] - tradechange [,5]

tradechange <- merge (tradechange, chn_nzl_exp_08, by="industry")
colnames(trade)


