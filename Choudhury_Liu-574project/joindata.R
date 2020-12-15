library(tidyverse)
library(data.table)
library(lubridate)

library(sp)
library(rgeos)
library(geosphere)

# Location
path = "C:/Users/kanak/ComS574/Project/Data/"
sir_predict = read.csv(file = paste(path, "sir_predict.csv", sep = ""))
sir_predict$Time = as.POSIXct(sir_predict$Time,  format = "%Y-%m-%d")
names(sir_predict) = c("Date" ,  "Sir_Susceptible",      "Sir_infacted",  
                       "Sir_deaths_recover", "Territory")

sird_predict = read.csv(file = paste(path, "sird_predict.csv", sep = ""))
sird_predict$Time = as.POSIXct(sird_predict$Time,  format = "%Y-%m-%d")
names(sird_predict) = c("Date" , "Sird_Susceptible", "Sird_infacted",
                       "Sird_recover",   "Sird_deaths",      "Territory" )

trend_predict = read.csv(file = paste(path, "trend_predict.csv", sep = ""))
trend_predict$Date = as.POSIXct(trend_predict$Date,  format = "%Y-%m-%d")

teri = read.csv(file = paste(path,"teri.csv", sep = ""))
names(teri) = c("Territory", "WDI_Code")

train = read.csv(file = paste(path, "new_train.csv", sep = ""))
train$Date = as.POSIXct(train$Date,  format = "%Y-%m-%d")
train$Territory = as.character(train$Territory)
train$tr_ts = 1

location = read.csv(file = paste(path, "location.csv", sep = ""))[,c(1,3:4)]
location = location[location$Territory %in% unique(train$Territory),]

nearest_country <- function(ncountry){
  sp.location <- location
  coordinates(sp.location) <- ~Long+Lat
  dis <- distm(sp.location)
  for (i in c(2:(ncountry+1))){
    nm = names(location)
    min.dis <- apply(dis, 1, function(x) order(x, decreasing=F)[i])
    location <- as.data.frame(cbind(location, location[min.dis,1], apply(dis, 1, function(x) sort(x, decreasing=F)[i])))
    names(location) <- c(nm, paste(c('neighbor', 'distance'), i-1, sep = "_"))  
  }
  return(location)
}

ncountry = 4
location = nearest_country(ncountry = ncountry)

# Cluster of country
cluster_territory = read.csv(file = paste(path,"Cluster_territoy.csv", sep = ""))[,2:3]
cluster_territory$Territory = as.character(cluster_territory$Territory)


abc = as.data.frame(c("Aruba"           , "Bermuda"          ,"Cayman Islands"  , 
                      "Curacao"          ,"Faroe Islands"   ,
                      "French Guiana"   , "French Polynesia" ,"Greenland"       , 
                      "Guadeloupe"      , "Mayotte"         ,
                      "Montserrat"      , "New Caledonia"    ,"Reunion"         , 
                      "Saint Barthelemy" ,"St Martin" ))
abc$Cluster_territory = 0
names(abc) = names(cluster_territory)
cluster_territory = as.data.frame(rbind(cluster_territory, abc))


# Data read

train = read.csv(file = paste(path,"new_train.csv", sep = ""))
train$Date = as.POSIXct(train$Date,  format = "%Y-%m-%d")
train$Territory = as.character(train$Territory)
train$tr_ts = 1


final_dt = read.csv(file = paste(path,"final_dt.csv", sep = ""))

# deaths = read.csv(file = "C:/Users/kanak/ComS574/Project/Data/deaths.csv")
# deaths$Territory = as.character(deaths$Territory)

# recover = read.csv(file = "C:/Users/kanak/ComS574/Project/Data/recover.csv")
# recover$Territory = as.character(recover$Territory)
# recover = recover[,-c(1:4)]
# recover = recover[,names(recover) != "WDI_Code"]
# recover1 = recover %>% group_by(Territory) %>% summarise_all(sum)
# cc1 = unique(train$Territory)[!(unique(train$Territory) %in% unique(recover1$Territory))]
# for (i in c(1:length(cc1))){
#   nrow1 = nrow(recover1)
#   recover1[nrow1+1,1] = cc1[i]
# }
# recover1[is.na(recover1)] = 0
# recover2 <- melt(setDT(recover1), id.vars = c("Territory"), 
#                  variable.name = "Date", value.name = "Recover")
# recover2$Date = as.POSIXct(str_replace(recover2$Date, "X", ""),  format = "%m.%d.%y")



# add train and test time period
day = seq(max(train$Date)+days(1), as.POSIXct("2020/06/07",  format = "%Y/%m/%d"), by = "day")
test = expand.grid(unique(train$Territory), day)
names(test) = c("Territory", "Date")
# test$Territory.X.Date = paste(test$Territory, paste(month(test$Date), day(test$Date), format(test$Date,'%y'), sep = "/"), sep = " X ")
test = test[,c("Territory", "Date" )]
test[,3:ncol(train)] = NA
names(test) = c("Territory", "Date" , names(train)[!(names(train) %in% c("Territory", "Date"))])
test = test[, names(train)]
test$tr_ts = 2
train = as.data.frame(rbind(train, test))


# train = merge(x = train, y = recover2, by= c("Territory", "Date"), 
#               all.x = TRUE, sort = FALSE)

train = merge(x = train, y = teri, by = "Territory", all.x = TRUE, sort = FALSE)

train = merge(x = train, y = location, by = "Territory", all.x = TRUE, sort = FALSE)

train = merge(x = train, y = sir_predict, by = c("Date","Territory"), all.x = TRUE, sort = FALSE)

train = merge(x = train, y = sird_predict, by = c("Date","Territory"), all.x = TRUE, sort = FALSE)

train = merge(x = train, y = trend_predict, by.x = c("Date","Territory"), by.y = c("Date","Country"), all.x = TRUE, sort = FALSE)

ak = c("Sir_Susceptible",      "Sir_infacted",  
       "Sir_deaths_recover", "Sird_Susceptible", "Sird_infacted",
       "Sird_recover",   "Sird_deaths","Trend_Confirmed", 
       "Trend_Deaths", "Trend_Recovered")

train11 = train[,ak]
train11[is.na(train11)] = 0  
train = cbind(train[,names(train)[!(names(train) %in% ak)]], train11)

recover_missing = train[is.na(train$Recover),]

for (i in c(1:ncountry)){
  train = train[order(train$Territory, train$Date), ]
  newname_d = c("n_near_country_death_", "n_near_country_case_", "n_near_country_recor")
  newname_d = paste(newname_d, i, sep = "")
  train[,newname_d] = NA
  abc = paste("neighbor_", i, sep = "")
  uni_con = unlist(unique(as.character(train$Territory)))
  uni_con1 = unlist(lapply(uni_con, function(x) unique(as.character(train[train$Territory == x, abc]))))

  for (j in c(1:length(uni_con))){
    train[train$Territory==uni_con[j], newname_d] = train[train$Territory==uni_con1[j],
                                                          c("Sird_deaths", "Sird_infacted", "Sird_recover")]
  }
    
}

train = train[order(train$tr_ts, train$Territory,  train$Date), ]

train = train[,!(names(train) %in% paste("neighbor_", 1:ncountry, sep = ""))]

train = merge(x = train, y = final_dt, by.x = "WDI_Code", by.y = "Country.Code", all.x = TRUE, sort = FALSE)
train = merge(x = train, y = cluster_territory, by = "Territory", all.x = TRUE, sort = FALSE)

train = train[!(train$Territory %in% c("Bahamas, The",  "Côte d'Ivoire", "Gambia, The",   "Sudan" )),]

colSums(is.na(train))
unique(train[is.na(train$WDI_Code),]$Territory)
names(train)

colSums(is.na(train))

# train = train %>% drop_na()

write.csv(train, file = paste(path, "train_data.csv", sep = ""), row.names = FALSE)


#########################################################
# write.csv(unique(train$Territory), file = "C:/Users/kanak/ComS574/Project/Data/teri.csv")
# write.csv(train, file = "C:/Users/kanak/ComS574/Project/Data/train_data.csv", row.names = FALSE)
