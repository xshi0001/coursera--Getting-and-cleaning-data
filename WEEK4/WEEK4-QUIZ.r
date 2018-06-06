
library("tidyverse")

data1<-  read_csv("F:/learningr/coursera_Getting and Cleaning Data/data/Fss06hid.csv")

names(data1)

strsplit(names(data1), "wgtp")[123]

GDPdata <- read_csv("F:/learningr/coursera_Getting and Cleaning Data/data/GDP.csv",
                    skip = 5,
                   col_names = FALSE)

commas <- function(str)  gsub(pattern = ",|\\s" ,"", str)
tidyData <- GDPdata[, c("X1","X2","X4","X5")] %>% drop_na("X5")
tidyData["X5"] <- sapply(tidyData["X5"],commas)

# # x[which(x$var>2)),]
# Data <- tidyData$X5[which(nchar(tidyData$X5) >3)]
# Data <- mean(as.numeric(Data))

data2<- tidyData[!grepl("\\.",tidyData$X5), ]

data2$X5 <- as.numeric(data$X5)
data2

sum(data2['X5'])/nrow(GDPdata )
summary(data2)

# for the 190 ranked countries in this data set
data2 <- data2[!is.na(data2$X2),]
summary(data2)

grep("^United", GDPdata$X4)

#Match the data based on the country shortcode
data4<-  read_csv("F:/learningr/coursera_Getting and Cleaning Data/data/EDSTATS_Country.csv")[,c("CountryCode", "Special Notes")]
head(data4)
# data4[!is.na(data4['Special Notes'])]

data4 <- na.omit(data4)
head(data4,6)

#子集选取 x[(x$var1<=3&X$var3>11), ]
data4 <- data4[c(grep('^Fiscal year end: June',data4[[2]])),]
head(data4,5)

nrow(data4)

#连接两个表CountryCode中排名在190名之内的
# c(2,4,5) %in% c(2,4,6,2)
nrow(data4[c(data4[[1]]%in% data2[[1]]),])

library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

tail(amzn,4)

head(sampleTimes)

# How many values were collected in 2012? How many values were collected on Mondays in 2012?
library("lubridate")
sum(sapply(sampleTimes,year)==2012)

data5 <- sampleTimes[c(sapply(sampleTimes,year)==2012)] 
sum(sapply(data5, weekdays)=="星期一")

