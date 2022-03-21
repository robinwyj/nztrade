#installing packages
install.packages("estimatr")
install.packages('Rcpp') 
install.packages("ggplot")
install.packages("sf")
install.packages("raster")
install.packages("dplyr")
install.packages("spData")
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/',
                 type='source')
install.packages("tmap")
install.packages("cartography")
install.packages("tidyverse")
install.packages("data.table")
install.packages("matrixStats")
install.packages("stargazer")
install.packages("sandwich")



#pacakges used
library(readr)
library(sandwich)
library(stargazer)
library(estimatr)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)
library(tmaptools)
library(leaflet)
library(ggplot2)
library(cartography)
library(tidyverse)
library(data.table)
library(matrixStats)


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
 
write.csv(EP06, "Electorate Profile 2006",row.names=FALSE)
DATASET2 <- read.csv("Electorate Profile 2006")
 




#Corrected Election results
Election_2011 <- read_csv("Election 2011.csv", 
                          skip = 1)
colnames(Election_2011)[1]<- "Electorate"
Election_2011<- Election_2011[,c(1,3,5,7,9,11,13,15,17)]
Election_2011<- Election_2011[-1,]
Election_2011 <- Election_2011[1:63,]
Electoratenames11 <- Election_2011$Electorate
Election_2011 <- as.data.frame(apply(Election_2011, 2, as.numeric))
Election_2011$max <- apply(Election_2011[,2:9], 1, max)
Election_2011$Electorate<- Electoratenames11

Election_2011$"Party Succeded" <- colnames(Election_2011)[apply(Election_2011,1,which.max)]
  colnames(Election_2011) <- paste0("2011",colnames(Election_2011))
colnames(Election_2011)[1]<- "Electorate"
  

Election_2014 <- read_csv("Election 2014.csv", 
                          skip = 1)
colnames(Election_2014)[1]<- "Electorate"
Election_2014<-Election_2014[,c(1,3,5,7,9,11,13,15,17)]
Election_2014<- Election_2014[-1,]
Election_2014<- Election_2014[1:64,]
Electoratenames14 <- Election_2014$Electorate
Election_2014 <- as.data.frame(apply(Election_2014, 2, as.numeric))
Election_2014$max <- apply(Election_2014[,2:8], 1, max)
Election_2014$Electorate<- Electoratenames

Election_2014$"Party Succeded" <- colnames(Election_2014)[apply(Election_2014,1,which.max)]
colnames(Election_2014) <- paste0("2014",colnames(Election_2014))
colnames(Election_2014)[1]<- "Electorate"


Election <- merge(Election_2011,Election_2014, by="Electorate")
Election$"Winning Margin 2011-2014" <- Election$`2014max`- Election$`2011max`
Election$Incumbency <- Election$`2011Party Succeded`== Election$`2014Party Succeded`


Election[28,22] <- Election[28,16]- Election[28,7]
DATASET1 <- Election





# Relevant trade data 1995-2014
library(readr)

icio_95 <- read.csv("ICIO2016_1995.csv")
icio_00 <- read.csv("~/Desktop/ICIO2016_2000.csv")

icio_05 <- read.csv("~/Desktop/ICIO2021_2005.csv")
icio_07 <- read.csv("~/Desktop/ICIO2021_2007.csv")
icio_08 <- read.csv("~/Desktop/ICIO2021_2008.csv")
icio_11 <- read.csv("~/Desktop/ICIO2021_2011.csv")
icio_14 <- read.csv("~/Desktop/ICIO2021_2014.csv")
icio_17 <- read.csv("ICIO2021_2017.csv")





# optional?
all_cty <- unique(substr(colnames(icio_07), 1, 3))[-1]
all_cty <- all_cty[-c(72:length(all_cty))]
all_cty <- all_cty[!all_cty== "NZL"]
#

icio_95_chn_cn2 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_95), 1, 3) == "NZL"])

icio_95_chn_cn3 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN3"), 
                                      substr(colnames(icio_95), 1, 3) == "NZL"])

icio_00_chn_cn2 <- data.frame(icio_00[substr(icio_00$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_00), 1, 3) == "NZL"])

icio_00_chn_cn3 <- data.frame(icio_00[substr(icio_00$X, 1, 3) %in% c ("CN3"), 
                                      substr(colnames(icio_00), 1, 3) == "NZL"])

icio_05_chn_cn1 <- data.frame(icio_05[substr(icio_05$X, 1, 3) %in% c ("CN1"), 
                                      substr(colnames(icio_05), 1, 3) == "NZL"])

icio_05_chn_cn2 <- data.frame(icio_05[substr(icio_05$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_05), 1, 3) == "NZL"])

icio_05_chn_cn1 <- icio_05_chn_cn1[,1:45]
icio_05_chn_cn2 <- icio_05_chn_cn2[,1:45]


icio_07_chn_cn1 <- data.frame(icio_07[substr(icio_07$X, 1, 3) %in% c ("CN1"), 
                                      substr(colnames(icio_07), 1, 3) == "NZL"])

icio_07_chn_cn2 <- data.frame(icio_07[substr(icio_07$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_07), 1, 3) == "NZL"])
icio_07_chn_cn1 <- icio_07_chn_cn1[,1:45]
icio_07_chn_cn2 <- icio_07_chn_cn2[,1:45]

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


icio_17_chn_cn1 <- data.frame(icio_17[substr(icio_17$X, 1, 3) %in% c ("CN1"), 
                                      substr(colnames(icio_17), 1, 3) == "NZL"])

icio_17_chn_cn2 <- data.frame(icio_17[substr(icio_17$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_17), 1, 3) == "NZL"])

icio_17_chn_cn1 <- icio_17_chn_cn1[,1:45]
icio_17_chn_cn2 <- icio_17_chn_cn2[,1:45]


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
write.csv(chn_nzl_exp_95, "trade 1995.csv", row.names=FALSE)
test <- read.csv("trade 1995.csv")




icio_00_chn_cn2_sum <- rowSums(icio_00_chn_cn2)
icio_00_chn_cn3_sum <- rowSums(icio_00_chn_cn3)
chn_nzl_exp_00 <- icio_00_chn_cn2_sum + icio_00_chn_cn3_sum
chn_nzl_exp_00 <- data.frame(industry = substr(colnames(icio_00_chn_cn2), 
                                               5, nchar(colnames(icio_00_chn_cn2))), 
                             exp_val = chn_nzl_exp_00)

write.csv(chn_nzl_exp_00, "trade 2000.csv", row.names=FALSE)



icio_05_chn_cn1_sum <- rowSums(icio_05_chn_cn1)
icio_05_chn_cn2_sum <- rowSums(icio_05_chn_cn2)
chn_nzl_exp_05 <- icio_05_chn_cn1_sum + icio_05_chn_cn2_sum
chn_nzl_exp_05 <- data.frame(industry = substr(colnames(icio_05_chn_cn2), 
                                               5, nchar(colnames(icio_05_chn_cn2))), 
                             exp_val = chn_nzl_exp_05)
write.csv(chn_nzl_exp_05, "trade 2005.csv", row.names=FALSE)




icio_07_chn_cn1_sum <- rowSums(icio_07_chn_cn1)
icio_07_chn_cn2_sum <- rowSums(icio_07_chn_cn2)
chn_nzl_exp_07 <- icio_07_chn_cn1_sum + icio_07_chn_cn2_sum
chn_nzl_exp_07 <- data.frame(industry = substr(colnames(icio_07_chn_cn2), 
                                               5, nchar(colnames(icio_07_chn_cn2))), 
                             exp_val = chn_nzl_exp_07)
write.csv(chn_nzl_exp_07, "trade 2007.csv", row.names=FALSE)



icio_08_chn_cn1_sum <- rowSums(icio_08_chn_cn1)
icio_08_chn_cn2_sum <- rowSums(icio_08_chn_cn2)
chn_nzl_exp_08 <- icio_08_chn_cn1_sum + icio_08_chn_cn2_sum
chn_nzl_exp_08 <- data.frame(industry = substr(colnames(icio_08_chn_cn2), 
                                               5, nchar(colnames(icio_08_chn_cn2))), 
                             exp_val = chn_nzl_exp_08)
write.csv(chn_nzl_exp_08, "trade 2008.csv", row.names=FALSE)


icio_11_chn_cn1_sum <- rowSums(icio_11_chn_cn1)
icio_11_chn_cn2_sum <- rowSums(icio_11_chn_cn2)
chn_nzl_exp_11 <- icio_11_chn_cn1_sum + icio_11_chn_cn2_sum
chn_nzl_exp_11 <- data.frame(industry = substr(colnames(icio_11_chn_cn2), 
                                               5, nchar(colnames(icio_11_chn_cn2))), 
                             exp_val = chn_nzl_exp_11)
write.csv(chn_nzl_exp_11, "trade 2011.csv", row.names=FALSE)


icio_14_chn_cn1_sum <- rowSums(icio_14_chn_cn1)
icio_14_chn_cn2_sum <- rowSums(icio_14_chn_cn2)
chn_nzl_exp_14 <- icio_14_chn_cn1_sum + icio_14_chn_cn2_sum
chn_nzl_exp_14 <- data.frame(industry = substr(colnames(icio_14_chn_cn2), 
                                               5, nchar(colnames(icio_14_chn_cn2))), 
                             exp_val = chn_nzl_exp_14)
write.csv(chn_nzl_exp_14, "trade 2014.csv", row.names=FALSE)

icio_17_chn_cn1_sum <- rowSums(icio_17_chn_cn1)
icio_17_chn_cn2_sum <- rowSums(icio_17_chn_cn2)
chn_nzl_exp_17 <- icio_17_chn_cn1_sum + icio_17_chn_cn2_sum
chn_nzl_exp_17 <- data.frame(industry = substr(colnames(icio_17_chn_cn2), 
                                               5, nchar(colnames(icio_17_chn_cn2))), 
                             exp_val = chn_nzl_exp_17)
write.csv(chn_nzl_exp_17, "trade 2017.csv", row.names=FALSE)



#Trade
chn_nzl_exp_05<-read.csv("trade 2005.csv")
chn_nzl_exp_08 <-read.csv("trade 2008.csv")
chn_nzl_exp_07 <-read.csv("trade 2007.csv")
chn_nzl_exp_11 <-read.csv("trade 2011.csv")
chn_nzl_exp_14 <-read.csv("trade 2014.csv")
chn_nzl_exp_17 <-read.csv("trade 2017.csv")




tradechange <- merge (chn_nzl_exp_05, chn_nzl_exp_08, by ="industry")
colnames(tradechange)[2] <- "exp05"
colnames(tradechange)[3] <- "exp08"
tradechange$"change05-08" <- tradechange[,3] - tradechange[,2]

tradechange <- merge (tradechange, chn_nzl_exp_11, by="industry")
colnames(tradechange)[5] <- "exp11"
tradechange$"change08-11" <- tradechange[,5] - tradechange [,3]

tradechange <- merge (tradechange, chn_nzl_exp_14, by="industry")
colnames(tradechange)[7] <- "exp14"
tradechange$"change11-14" <- tradechange[,7] - tradechange [,5]

tradechange <- merge (tradechange, chn_nzl_exp_17, by="industry")
colnames(tradechange)[9] <- "exp17"
tradechange$"change14-17" <- tradechange[,9] - tradechange [,7]

write.csv(tradechange, "trade data.csv", row.names = FALSE)
DATATSET3 <- read.csv ("trade data.csv")

DATATSET3[1,1] <- "Agriculture, hunting, forestry"
DATATSET3[2,1] <- "Fishing and aquaculture"
DATATSET3[3,1] <-"Mining and quarrying, energy producing products"
DATATSET3[4,1] <-"Mining and quarrying, non-energy producing products"
DATATSET3[5,1] <-"Mining support service activities"
DATATSET3[6,1] <-"Food products, beverages and tobacco"
DATATSET3[7,1] <-"Textiles, textile products, leather and footwear"
DATATSET3[8,1] <-"Wood and products of wood and cork"
DATATSET3[9,1] <-"Paper products and printing"
DATATSET3[10,1] <-"Coke and refined petroleum products"
DATATSET3[11,1] <-"Chemical and chemical products"
DATATSET3[12,1] <-"Pharmaceuticals, medicinal chemical and botanical products"
DATATSET3[13,1] <-"Rubber and plastics products"
DATATSET3[14,1] <-"Other non-metallic mineral products"
DATATSET3[15,1] <-"Basic metals"
DATATSET3[16,1] <-"Fabricated metal products"
DATATSET3[17,1] <-"Computer, electronic and optical equipment"
DATATSET3[18,1] <-"Electrical equipment"
DATATSET3[19,1] <-"Machinery and equipment, nec "
DATATSET3[20,1] <-"Motor vehicles, trailers and semi-trailers"
DATATSET3[21,1] <-"Other transport equipment"
DATATSET3[22,1] <-"Manufacturing nec; repair and installation of machinery and equipment"
DATATSET3[23,1] <-"Electricity, gas, steam and air conditioning supply"
DATATSET3[24,1] <-"Water supply; sewerage, waste management and remediation activities"
  DATATSET3[25,1] <-"Construction"
  DATATSET3[26,1] <-"Wholesale and retail trade; repair of motor vehicles"
  DATATSET3[27,1] <-"Land transport and transport via pipelines"
  DATATSET3[28,1] <-"Water transport"
  DATATSET3[29,1] <-"Air transport"
  DATATSET3[30,1] <-"Warehousing and support activities for transportation"
  DATATSET3[31,1] <-"Postal and courier activities"
  DATATSET3[32,1] <-"Accommodation and food service activities"
  DATATSET3[33,1] <-"Publishing, audiovisual and broadcasting activities"
  DATATSET3[34,1] <-"Telecommunications"
  DATATSET3[35,1] <-"IT and other information services"
  DATATSET3[36,1] <-"Financial and insurance activities"
  DATATSET3[37,1] <-"Real estate activities"
  DATATSET3[38,1] <-"Professional, scientific and technical activities"
  DATATSET3[39,1] <-"Administrative and support services"
  DATATSET3[40,1] <-"Public administration and defence; compulsory social security"
  DATATSET3[41,1] <-"Education"
  DATATSET3[42,1] <-"Human health and social work activities"
  DATATSET3[43,1] <-"Arts, entertainment and recreation"
  DATATSET3[44,1] <-"Other service activities"
  DATATSET3[45,1] <-"Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use"
  DATASET3 <- DATATSET3
  
  
#Calculating trade shock
M_0 <- sum(DATASET3[c(7:22),2])
M_1 <- sum(DATASET3[c(7:22),3])

M_change <- sum(DATASET3[c(7:22),4])
tradechange <- M_1 - M_0

rowsum(EP06, EP06[,1])
EP06<- read.csv("https://raw.githubusercontent.com/robinwyj/nztrade/main/ElectorateProfile08")
EP06 <- EP06[1:63,]                                                                                                   
EP06 <- EP06[,2:21]

EP2006 <- EP06[,-1]
rownames(EP2006) <- EP06[,1]
as.numeric(EP2006)



EP2006$"Electorate Population" <- rowSums(EP2006)
L_i<- rowSums(EP2006)
as.numeric (L_i)
L_nz <- sum(EP2006$`Electorate Population`)
L_mi <- EP2006[,3]
as.numeric(L_mi)

tradeshock2005 <- (L_mi/L_nz)*(M_0/L_i)
trade2005 <- as.data.frame(tradeshock2005)
trade2005$"Electorate" <- row.names(trade2005)
row.names(trade2005) <- 1:63
trade2005<-trade2005[,c("Electorate","tradeshock2005")]


tradeshock <- (L_mi / L_nz)*(M_change/ L_i)
print(tradeshock)
DATASET4 <- as.data.frame(tradeshock)
DATASET4$"Electorate" <- row.names(DATASET4)
row.names(DATASET4) <- 1:63
as.numeric(DATASET4)

DATASET4<-DATASET4[,c("Electorate","tradeshock")]

tradeshock <- as.data.frame(tradeshock)
tradeshock$"Electorate" <- row.names(tradeshock)
row.names(tradeshock) <- 1:63

tradeshock<-tradeshock[,c("Electorate","tradeshock")]



L_ai <- EP2006[,1]
as.numeric(L_ai)
M.a_change <- DATASET3[1,4] + DATASET3[2,4]
tradeshock.a <- (L_ai / L_nz)*(M.a_change/ L_i)
tradeshock.a <- as.data.frame(tradeshock.a)
tradeshock.a$"Electorate" <- row.names(tradeshock.a)
row.names(tradeshock.a) <- 1:63

tradeshock.a<-tradeshock.a[,c("Electorate","tradeshock.a")]





#trade shock on electoral results
df<- merge(DATASET1, DATASET4, by="Electorate")
dfnew <- merge(ds1, DATASET4, by="Electorate")

ggplot(dfnew,aes(x=tradeshock, y=Final Margin Percentage)) + 
  geom_point()+geom_smooth(method='lm')

ggplot(df,aes(x=tradeshock, y=Winning.Margin.Percentage.2008.2011)) + 
   geom_point()+geom_smooth(method='lm')

ggplot(df,aes(x=tradeshock, y=Winning.Margin.Percentage.2011.2014)) + 
   geom_point()+geom_smooth(method='lm')

 ggplot(df,aes(x=tradeshock, y=Winning.Margin.Percentage.2014.2017)) + 
   geom_point()+geom_smooth(method='lm')

df2 <- merge(DATASET1, tradeshock.a, by="Electorate" )
plot(df2$tradeshock.a, df2$Winning.Margin.Percentage.2008.2011)
plot(df2$tradeshock.a, df2$Winning.Margin.Percentage.2011.2014)
plot(df2$tradeshock.a, df2$Winning.Margin.Percentage.2014.2017)


#Regression 
help(lm)
shocknew<- lm(dfnew[,6] ~ tradeshock, data=dfnew)
summary(shocknew)


shock1 <- lm(Winning.Margin.Percentage.2008.2011 ~ tradeshock, data=df)
plot(shock1)
shock2 <- lm(Winning.Margin.Percentage.2011.2014 ~ tradeshock, data=df)

shock3 <- lm(Incumbent.2008.2011 ~ tradeshock, data=df)
summary(shock3)
shock4 <- lm(Incumbent.2011.2014 ~ tradeshock, data=df)
summary(shock4)

shock4 <- lm(Winning.Margin.Percentage.2008.2011 ~0+ tradeshock.a,  data=df2)
summary(shock4)
shock5 <- lm(Winning.Margin.Percentage.2011.2014 ~0+ tradeshock.a, data=df2)
summary(shock5)



stargazer(shock1, type = "text")


lm_robust(Winning.Margin.Percentage.2008.2011 ~ tradeshock, data=df)
help("lm_robust")

#mapping

map <- st_read("D:/Louis/IR project/general-electoral-district-2002.csv")
map2007 <- st_read("D:/Louis/IR project/general-electoral-district-2007.csv")
map2014 <- st_read("D:/Louis/IR project/general-electoral-district-2014.csv")

colnames(map)[2] <- "Electorate"
colnames(map2007)[2] <- "Electorate"
colnames(map2014)[2] <- "Electorate"
map2014[26,2]<-	"Rangitikei"

map_and_data <- inner_join(tradeshock.a, map)
tradeshockmap2005 <- inner_join(trade2005, map)


map_and_data = st_as_sf(map_and_data)
tm_shape(map_and_data)+
  tm_polygons("tradeshock.a",id="Electorate")

tradeshockmap = st_as_sf(tradeshockmap)
tm_shape(tradeshockmap)+
  tm_polygons("tradeshock",id="Electorate")

tradeshockmap2005 = st_as_sf(tradeshockmap2005)
tm_shape(tradeshockmap2005)+
  tm_polygons("tradeshock2005",id="Electorate")


trade2005$Electorate==map$Electorate



tradeshockmap2014 = st_as_sf(tradeshockmap2014)
tm_shape(tradeshockmap2014)+
  tm_polygons("tradeshock",id="Electorate")

#trials
map_and_data$geometry <- "MULTIPOLYGON"

plot(st_geometry(map))
choroLayer(x=map_and_data, var="tradeshock.a", method="equal")



ggplot(map_and_data )+
  geom_sf(aes(fill="tradeshock.a"))
  rlang::last_error()

ggplot(general_electoral_district_2002) +
  geo_sf(aes(fill= "SHAPE"))



#Tradeshock and voting for 2011-2014 using the 2013 census, using 2011 as t0
M_0 <- sum(DATASET3[c(7:22),7])
EP13 <- read.csv("ep17.csv")
EP13 <- EP13[,2:ncol(EP13)]

rownames(EP13) <- EP13[,1]
EP13<- EP13[,-1]
EP13$"Electorate Population" <- rowSums(EP13)

L_i<- rowSums(EP13)
as.numeric (L_i)
L_nz <- sum(EP13$`Electorate Population`)
L_mi <- EP13[,3]
as.numeric(L_mi)

tradeshock2011 <- (L_mi/L_nz)*(M_0/L_i)
trade2011 <- as.data.frame(tradeshock2011)
trade2011$"Electorate" <- row.names(trade2011)
row.names(trade2011) <- 1:64
trade2011<-trade2011[,c("Electorate","tradeshock2011")]



tradeshockmap2011 <- inner_join(trade2011, map2014)
tradeshockmap2011 = st_as_sf(tradeshockmap2011)
tm_shape(tradeshockmap2011)+
  tm_polygons("tradeshock2011",id="Electorate")

                           



                     
#Looking at the 2014 election
M_change <- sum(DATASET3[c(7:22),8])

tradeshock2014 <- (L_mi/L_nz)*(M_change/L_i)
trade2014 <- as.data.frame(tradeshock2014)
trade2014$"Electorate" <- row.names(trade2014)
row.names(trade2014) <- 1:64
trade2014<-trade2014[,c("Electorate","tradeshock2014")]



tradeshockmap2014 <- inner_join(trade2014, map2014)
tradeshockmap2014 = st_as_sf(tradeshockmap2014)
tm_shape(tradeshockmap2014)+
  tm_polygons("tradeshock2014",id="Electorate")



#Regression
df1114<- merge(DATASET1, trade2014, by="Electorate")
shock1114 <- lm(df1114$`Winning Margin 2011-2014` ~ tradeshock2014, data=df1114)
summary(shock1114)
df1114robust<-lm_robust(df1114$`Winning Margin 2011-2014` ~ tradeshock2014, data=df1114)
summary(df1114robust)

(Covar1114 <- vcovHC(shock1114))
(Estvar1114 <- diag(result1114))

stargazer(shock1114,
          se = list(sqrt(Estvar1114)),
          type = "text")



 # Obsolete Below are codes comiling data and writing csv.
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
 s(vote2020)[1]<-"Electorate"
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
 
 # Vote results for the 2008, 2011, 2014 elections (NOW OBSOLETE)
 library(readr)
 
 e02 <- read_csv("~/Downloads/e9_part6-6.csv", 
                 skip = 1)
 e05 <-  read_csv("~/Downloads/e9_part6-7.csv", 
                  skip = 1)
 
 
 votechange0205 <- merge (vote2002, vote2005, by="Electoral District")
 
 
 colnames(votechange0205)[1] <- "Electorate"
 colnames(votechange0205)[2] <- "Electorate"
 votechange0205$incumbent <- votechange0205$Party.x == votechange0205$Party.y
 votechange0205$"Winning Margin Percentage 2002-2005" <- votechange0205[,] -votechange0205[,]
 
 
 e08 <- read_csv("~/Downloads/e9_part6-4.csv", 
                 skip = 1)
 
 e11 <- read_csv("~/Downloads/e9_part6-5.csv", 
                 skip = 1)
 
 e14 <- read_csv("~/Downloads/e9_part6.csv", 
                 skip = 1)
 
 e17 <- read_csv("~/Downloads/winning-electorate-candidates-3.csv", 
                 skip = 1)
 
 
 e02 <- e02[,c(1,3,6)]
 colnames(e02)[1]<-"Electorate"
 e05 <- e05[,c(1,3,6)]
 colnames(e05)[1]<-"Electorate"
 e08 <- e08 [,c(1,3,6)]
 colnames(e08)[1]<-"Electorate"
 e11 <- e11 [,c(1,3,6)]
 colnames(e11)[1]<-"Electorate"
 e14 <- e14 [,c(1,3,6)]
 colnames(e14)[1]<-"Electorate"
 e17 <- e17 [,c(1,3,6)]
 colnames(e17)[1]<-"Electorate"
 
 
 colnames(e02)[2] <- "Party 2002"
 colnames(e05)[2] <- "Party 2005"
 colnames(e08)[2] <- "Party 2008"
 colnames(e11)[2] <- "Party 2011"
 colnames(e14)[2] <- "Party 2014"
 colnames(e17)[2] <- "Party 2017"
 
 colnames(e02)[3] <- "Percentage of Vote Recieved 2002"
 colnames(e05)[3] <- "Percentage of Vote Recieved 2005"
 colnames(e08)[3] <- "Percentage of Vote Recieved 2008"
 colnames(e11)[3] <- "Percentage of Vote Recieved 2011"
 colnames(e14)[3] <- "Percentage of Vote Recieved 2014"
 colnames(e17)[3] <- "Percentage of Vote Recieved 2017"
 
 e02$`Percentage of Vote Recieved 2002` <- gsub("%", "", e02$`Percentage of Vote Recieved 2002`)
 e02$`Percentage of Vote Recieved 2002`<- as.numeric(e02$`Percentage of Vote Recieved 2002`)
 e02[62,2] <- "Progressive Party"
 
 
 e05$`Percentage of Vote Recieved 2005` <- gsub("%", "", e05$`Percentage of Vote Recieved 2005`)
 e05$`Percentage of Vote Recieved 2005`<- as.numeric(e05$`Percentage of Vote Recieved 2005`)
 e05[62,2] <- "Progressive Party"
 e05[c(65,66,67,69),2] <- "Māori Party"
 
 
 e08$`Percentage of Vote Recieved 2008` <- gsub("%", "", e08$`Percentage of Vote Recieved 2008`)
 e08$`Percentage of Vote Recieved 2008`<- as.numeric(e08$`Percentage of Vote Recieved 2008`)
 e08[e08== "Maori Party"] <- "Māori Party"
 
 
 e11$`Percentage of Vote Recieved 2011` <- gsub("%", "", e11$`Percentage of Vote Recieved 2011`)
 e11$`Percentage of Vote Recieved 2011`<- as.numeric(e11$`Percentage of Vote Recieved 2011`)
 
 e14$`Percentage of Vote Recieved 2014` <- gsub("%", "", e14$`Percentage of Vote Recieved 2014`)
 e14$`Percentage of Vote Recieved 2014`<- as.numeric(e14$`Percentage of Vote Recieved 2014`)
 
 e17$`Percentage of Vote Recieved 2017` <- gsub("%", "", e17$`Percentage of Vote Recieved 2017`)
 e17$`Percentage of Vote Recieved 2017`<- as.numeric(e17$`Percentage of Vote Recieved 2017`)
 
 ds1 <- merge (e02, e05, by="Electorate")
 ds1$"Incumbent 2002-2005" <- ds1$"Party 2002" == ds1$"Party 2005"
 ds1[,6] <- as.numeric (ds1[,6])
 ds1$"Winning Margin Percentage 2002-2005" <- ds1[,5]- ds1[,3]
 ds1 <- ds1[,c(1,3,5,6,7)]
 ds1$"Final Margin Percentage" <- ds1$`Incumbent 2002-2005`*ds1$`Winning Margin Percentage 2002-2005`
 
 
 dataset1 <- merge (e08,e11, by="Electorate")
 dataset1$"Incumbent 2008-2011" <- dataset1$"Party 2008" == dataset1$"Party 2011"
 dataset1[,6] <- as.numeric (dataset1[,6])
 dataset1$"Winning Margin Percentage 2008-2011" <- dataset1[,5]- dataset1[,3]
 write.csv(dataset1, "Elections",row.names = FALSE)
 
 dataset1 <- merge (dataset1,e14, by="Electorate")
 dataset1$"Incumbent 2011-2014" <- dataset1$"Party 2011" == dataset1$"Party 2014"
 dataset1[,10] <- as.numeric (dataset1[,10])
 dataset1$"Winning Margin Percentage 2011-2014" <- dataset1[,9]- dataset1[,5]
 
 dataset1 <- merge (dataset1,e17, by="Electorate")
 dataset1$"Incumbent 2014-2017" <- dataset1$"Party 2014" == dataset1$"Party 2017"
 dataset1[,14] <- as.numeric (dataset1[,14])
 dataset1$"Winning Margin Percentage 2014-2017" <- dataset1[,13]- dataset1[,9]
 colnames(dataset1)[15] <- "Winning Margin Percentage 2014-2017"
 
 dataset1 <- dataset1[,c(1,3,5,6,7,9,10,11,13,14,15)]
 rownames(dataset1) <- dataset1[,1]
 dataset1 <- dataset1[,c(2:11)]
 
 write.csv(dataset1, "Election Results2008-2017.csv")
 DATASET1 <- read.csv ("Election Results2008-2017.csv")
 colnames(DATASET1)[1] <- "Electorate"
 
 
 
 
 