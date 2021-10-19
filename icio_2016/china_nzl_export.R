#load datasets. We are using three years but there are more. 
icio_07 <- read.csv("ICIO2016_2007.csv")
icio_00 <- read.csv("ICIO2016_2000.csv")
icio_95 <- read.csv("ICIO2016_1995.csv")


all_cty <- unique(substr(colnames(icio_07), 1, 3))[-1]
all_cty <- all_cty[-c(72:length(all_cty))]
all_cty <- all_cty[!all_cty== "NZL"]

#two matrice for 1995
#generate cn2 
icio_95_chn_cn2 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN2"), 
                                      substr(colnames(icio_95), 1, 3) == "NZL"])
#sum up each row of cn2 for Chinese export to NZL for each industry 
icio_95_chn_cn2_sum <- rowSums(icio_95_chn_cn2)
#generate cn3
icio_95_chn_cn3 <- data.frame(icio_95[substr(icio_95$X, 1, 3) %in% c ("CN3"), 
                                      substr(colnames(icio_95), 1, 3) == "NZL"])
#same for cn3
icio_95_chn_cn3_sum <- rowSums(icio_95_chn_cn3)
#sum up cn2 and cn3 
chn_nzl_exp_95 <- icio_95_chn_cn2_sum + icio_95_chn_cn3_sum
#create a dataset `chn_nzl_exp_95`. It has two columns: the first column is the name of each Chinese industry; the second column is the corresponding Chinese exports to NZL 
chn_nzl_exp_95 <- data.frame(industry = substr(colnames(icio_95_chn_cn2), 
                                               5, nchar(colnames(icio_95_chn_cn2))), 
                             exp_val = chn_nzl_exp_95)


### you can replicate the same procedure for the years of 2000 and 2007 


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






