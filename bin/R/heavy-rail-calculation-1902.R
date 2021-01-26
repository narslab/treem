#necessary library
library(lubridate)
library(dplyr)
library(geosphere) 
#read files
df = read.csv("ttr. heavyraillocation1902.csv",header = TRUE, na.strings =0)
# add one column to save the day
df$Day=day(df$trxtime)
# divided by different lines
dr=subset(df,df$lineid==1)
db=subset(df,df$lineid==2)
do=subset(df,df$lineid==3)
# red line
datar = data.frame(time=dr$trxtime, Day=dr$Day, trainid=dr$trainid, line=dr$lineid, lat=dr$lat, lon=dr$lon)
nrow(datar)
datar = datar[!(is.na(datar$lon)) | !(is.na(datar$lat)),] #remove the rows with values of lat and lon are 0
datar = datar[order(datar$trainid, datar$time),]
datar = distinct(datar, time, trainid,  .keep_all = TRUE) # Remove the duplicated time record
nrow(datar)
# blue line
datab = data.frame(time=db$trxtime, Day=db$Day, trainid=db$trainid, line=db$lineid, lat=db$lat, lon=db$lon)
nrow(datab)
datab = datab[!(is.na(datab$lon)) | !(is.na(datab$lat)),] #remove the rows with values of lat and lon are 0
datab = datab[order(datab$trainid, datab$time),]
datab = distinct(datab, time, trainid,  .keep_all = TRUE) # Remove the duplicated time record
nrow(datab)
# orange line
datao = data.frame(time=do$trxtime, Day=do$Day, trainid=do$trainid, line=do$lineid, lat=do$lat, lon=do$lon)
nrow(datao)
datao = datao[!(is.na(datao$lon)) | !(is.na(datao$lat)),] #remove the rows with values of lat and lon are 0
datao = datao[order(datao$trainid, datao$time),]
datao = distinct(datao, time, trainid,  .keep_all = TRUE) # Remove the duplicated time record
nrow(datao)
# Function to compute distances (D), speeds (S) and acceleration (A) in meters, meters per second, km per hour and m s^-2
computeDSA <- function(d) {
  d$dist=NA
  d$timeinterval=NA
  d$mps=NA
  d$kph=NA
  d$acc=NA
  n <- nrow(d)
  if (n >= 2) {
    d$dist[2:n] = distHaversine(cbind(d$lon[1:n-1],d$lat[1:n-1]),cbind(d$lon[2:n],d$lat[2:n]))
    d$timeinterval[2:n] = as.numeric(difftime(d$time[2:n], d$time[1:n-1], units = "secs"))
    d$mps[2:n] = d$dist[2:n] / d$timeinterval[2:n]
    d$kph[2:n] = d$mps[2:n]*3.6
    d$acc[2:n] = (d$mps[2:n] - d$mps[1:n-1])/d$timeinterval[2:n]
    for(k in 2:n) { 
      # revise the incorrect values 
      if (!(is.na(d$kph[k]))) {
        if (d$kph[k] > 70 ) {
          d$kph[k] = d$kph[k-1]
          d$mps[k] = d$kph[k] / 3.6
          d$acc[k] = (d$mps[k]-d$mps[k-1]) / d$timeinterval[k]
          d$dist[k] = d$mps[k] * d$timeinterval[k] 
        }
      }
    }
  }    
  return(d)
}
# loop through one month 
# generate orange line results
results.dfo = data.frame() # empty dataframe
for(i in unique(datao$Day)) { 
  if (i <= 31) { 
    data.day <- datao[datao$Day == i, ]
    for (j in unique(data.day$trainid)) {# Put each train in one loop in a subset
      data.day.train = data.day[data.day$trainid == j, ]        
      results <- computeDSA(data.day.train) # Process data with only one row separately        
      results.dfo <- rbind(results.dfo, results) # put all the results in one table
    }
  }
}    
write.csv(x=results.dfo, file="heavyrailorange_distances_1902.csv")
# generate red line results
results.dfr = data.frame() # empty dataframe
for(i in unique(datar$Day)) { 
  if (i <= 31) { 
    data.day <- datar[datar$Day == i, ]
    for (j in unique(data.day$trainid)) {# Put each train in one loop in a subset
      data.day.train = data.day[data.day$trainid == j, ]        
      results <- computeDSA(data.day.train) # Process data with only one row separately        
      results.dfr <- rbind(results.dfr, results) # put all the results in one table
    }
  }
}    
write.csv(x=results.dfr, file="heavyrailred_distances_1902.csv")
# generate blue line results
results.dfb = data.frame() # empty dataframe
for(i in unique(datab$Day)) { 
  if (i <= 31) { 
    data.day <- datab[datab$Day == i, ]
    for (j in unique(data.day$trainid)) {# Put each train in one loop in a subset
      data.day.train = data.day[data.day$trainid == j, ]        
      results <- computeDSA(data.day.train) # Process data with only one row separately        
      results.dfb <- rbind(results.dfb, results) # put all the results in one table
    }
  }
}    
write.csv(x=results.dfb, file="heavyrailblue_distances_1902.csv")


