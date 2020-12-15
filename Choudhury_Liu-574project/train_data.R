library(dplyr)
library(tidyverse)
path = "C:/Users/kanak/ComS574/Project/Data/"
train = read.csv(file = paste(path, "time-series-19-covid-combined.csv", sep = ""))
train$Province.State = as.character(train$Province.State) 
train$Country.Region = as.character(train$Country.Region) 

train1 = train[(train$Province.State=="") | (train$Country.Region %in% c("Australia", "Canada",    "China")),]
train1 = train1[,-c(2:5)]
train2 = train1 %>% group_by(Date, Territory) %>% summarise_all(sum, na.rm=TRUE)
train2$Date = as.POSIXct(as.character(train2$Date),  format = "%m/%d/%Y")

train = read.csv(file = paste(path, "train.csv", sep = ""))
train$Date = as.POSIXct(train$Date,  format = "%m/%d/%Y")
train3 = merge(train2, train, by = c('Date', 'Territory'), all.x = TRUE)
train3 = train3[,-c(6:8)][,c("Date" ,  "Territory",   "Deaths", "Confirmed", "Recovered")]
names(train3) = c("Date" ,  "Territory",    "target", "cases", "Recover")
# train3 = train3 %>% drop_na()

unique(as.character(train3[is.na(train3$Confirmed),]$Territory))
unique(train3$Territory)
write.csv(train3, paste(path, "new_train.csv", sep = ""), row.names = F)
