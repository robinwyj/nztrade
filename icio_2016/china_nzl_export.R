#load datasets. We are using three years but there are more. 
icio_07 <- read.csv("ICIO2016_2007.csv")
icio_00 <- read.csv("ICIO2016_2000.csv")
icio_95 <- read.csv("ICIO2016_1995.csv")


all_cty <- unique(substr(colnames(icio_07), 1, 3))[-1]
all_cty <- all_cty[-c(72:length(all_cty))]
all_cty <- all_cty[!all_cty== "NZL"]

#two matrice for 1995
icio_95_chn_cn2 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN2"), 
                                  substr(colnames(icio_95), 1, 3) == "NZL"])

icio_95_chn_cn3 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN3"), 
                                      substr(colnames(icio_95), 1, 3) == "NZL"])



#two matrice for 2000
icio_00_chn_cn2 <- data.frame(icio_00[substr(icio_00$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_00), 1, 3) == "NZL"])

icio_00_chn_cn3 <- data.frame(icio_00[substr(icio_00$X, 1, 3) %in% c ("CN3"), 
                                      substr(colnames(icio_00), 1, 3) == "NZL"])

#two matrice for 2007
icio_07_chn_cn2 <- data.frame(icio_07[substr(icio_07$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_07), 1, 3) == "NZL"])

icio_07_chn_cn3 <- data.frame(icio_07[substr(icio_07$X, 1, 3) %in% c ("CN3"), 
                                      substr(colnames(icio_07), 1, 3) == "NZL"])



