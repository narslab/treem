#necessary library
library(data.table) #fread
library(lubridate)
library(reshape2)
library(dplyr)
library(scales)
library(stringr)
library(tidyr) # spread function

#memory.limit(size=900000) #Windows-specific #JO
# Select the month you want to investigate
YEARLIST =("19")
MONTHLIST = c("12")
DISTANCE_FILEPATH = "../../data/tidy/vehicle-trajectory-computation/"
COMPUTATION_FILEPATH = "../../data/tidy/"
energy_df = fread("../../data/raw/energy-consumption-08-20.csv") # Read in energy data
d_ridership = fread("../../data/raw/ridership-2019-2020.csv")# Read in ridership data

# aggregrate_trajectory_table
line_aggregation = function(year,month){
    assign("dg",fread(paste0(DISTANCE_FILEPATH, paste(paste("green", "trajectory", year, month, sep = "-", collapse = ""), ".csv", sep=""))))
    assign("dh",fread(paste0(DISTANCE_FILEPATH, paste(paste("heavy", "trajectory", year, month, sep = "-", collapse = ""), ".csv", sep=""))))
    dg$lineid = 4
    dg = subset(dg, select = c(trxtime, year, month, day, lineid, lat, lon , speed_kph , accel_mps2 , interval_seconds , dist_meters , vehicleid))
    dh = subset(dh, select = c(trxtime, year, month, day, lineid, lat, lon , speed_kph , accel_mps2 , interval_seconds , dist_meters , vehicleid))
    df = rbind(dg, dh) 
    return(df)
}

# Unit conversion
unit_transfer = function(df){
   df$hour = hour(df$trxtime)
   df$speed_mph = df$speed_kph*0.621371 #kph to mph
   df$distance_mile = df$dist_meters*0.000621371 #convert from meters to mile
   df$time_hr = df$interval_seconds/3600.0 #convert from seconds to hour
  return(df)
}    

# Calculate the speed bins 
bin_speeds <- function (dataframe, num_bins, test = FALSE) {
  print("Computing speed bins")
  dataframe = data.table(dataframe)
  dummy_cols = c(paste0("speed_bin_",1:num_bins,"_dummy"))
  bin_time_cols = c(paste0("speed_bin_",1:num_bins,"_time_hr"))
  cutpoints <- quantile(dataframe$speed_mph,seq(0, 1, 1/num_bins),na.rm=TRUE) 
  print(paste0("The speed bins are: ", cutpoints))
  for(n in seq(1, num_bins)) {
    if(n == 1){
     dataframe[, dummy_cols[n] := sapply(speed_mph,  function(x) ifelse (x < cutpoints[n+1], 1, 0))]
    }
    else if (n == num_bins){
     dataframe[, dummy_cols[n] := sapply(speed_mph,  function(x) ifelse (x >= cutpoints[num_bins], 1, 0))]
    }
    else {
    dataframe[, dummy_cols[n] := sapply(speed_mph,  function(x) ifelse (x >= cutpoints[n] & x < cutpoints[n + 1], 1, 0))]
    }
  }
 dataframe[, (bin_time_cols) := lapply(.SD, function(x) x * dataframe$time_hr ), .SDcols = dummy_cols]
 if (test) {
    print(paste0("Percentage error of summed speed bin times = ", 
              round(100*(sum(colSums(dataframe %>% select(starts_with("speed_bin_") & ends_with("_time_hr")),na.rm=TRUE)) 
                         - sum(dataframe$time_hr, na.rm=TRUE))/sum(dataframe$time_hr, na.rm=TRUE),2), "%"))
 }
 print("Done")
 return(dataframe) 
}

# Calculate the acceleration bins 
bin_accelerations <- function (dataframe, num_bins, test = FALSE) {
  print("Computing acceleration bins")
  dataframe = data.table(dataframe)
  dummy_cols = c(paste0("accel_bin_",1:num_bins,"_dummy"))
  bin_time_cols = c(paste0("accel_bin_",1:num_bins,"_time_hr"))
  cutpoints <- quantile(dataframe$accel_mps2,seq(0, 1, 1/num_bins),na.rm=TRUE) #read in the list from a saved file of cutpoints
  print(paste0("The acceleration bins are: ", cutpoints))
  for(n in seq(1, num_bins)) {
    if(n == 1){
      dataframe[, dummy_cols[n] := sapply(accel_mps2,  function(x) ifelse (x < cutpoints[n+1], 1, 0))]
    }
    else if (n == num_bins){
      dataframe[, dummy_cols[n] := sapply(accel_mps2,  function(x) ifelse (x >= cutpoints[num_bins], 1, 0))]
    }
    else {
      dataframe[, dummy_cols[n] := sapply(accel_mps2,  function(x) ifelse (x >= cutpoints[n] & x < cutpoints[n + 1], 1, 0))]
    }
  }
  dataframe[, (bin_time_cols) := lapply(.SD, function(x) x * dataframe$time_hr ), .SDcols = dummy_cols]
  if (test) {
      print(paste0("Percentage error of summed acceleration bin times = ", 
               round(100*(sum(colSums(dataframe %>% select(starts_with("accel_bin_") & ends_with("_time_hr")),na.rm=TRUE)) 
                          - sum(dataframe$time_hr, na.rm=TRUE))/sum(dataframe$time_hr, na.rm=TRUE),2), "%"))                                                  
  }
  print("Done")
  return(dataframe)
}

# add speed-acceleration bin interaction terms
bin_interaction_terms = function(df, num_speed_bins, num_accel_bins){
    print("Computing speed-acceleration interaction times")
    df = data.table(df)
    dummy_interaction_cols = c()
    for (i in seq(1, num_speed_bins)){
        speed_dummy = paste0("speed_bin_", i, "_dummy")
        for (j in seq(1, num_accel_bins)){
            # add interaction dummy variables
            accel_dummy = paste0("accel_bin_", j, "_dummy") 
            dummy_interaction_col = paste0("speed_bin_", i, "_", "accel_bin_", j)
            dummy_interaction_cols = c(dummy_interaction_cols, dummy_interaction_col) #update list of interaction columns
            set(df, j = dummy_interaction_col, value = df[[speed_dummy]]*df[[accel_dummy]])
        }
    }
    df[, paste0(dummy_interaction_cols, "_time_hr") := lapply(.SD, function(x) x * df$time_hr ), .SDcols = dummy_interaction_cols]
    print("Done")
    return(df)
}

# Aggregate dataframe at hour level
hour_aggregate <- function (dataframe, num_speed_bins, num_accel_bins) {
    print("Aggregating observations by hour")
    #dataframe = data.table(dataframe)
    dataframe$month = as.character(dataframe$month)
    dataframe$hour = as.character(dataframe$hour)
    dataframe$day = as.character(dataframe$day)
    # create another data table to summarize the number of trains running in each hour
    d_num_trains <- dataframe[, c("month",'hour',"day","lineid","vehicleid")]
    agg_d_num_trains = d_num_trains[, .(count = length(unique(vehicleid))), by = .(month,day,hour,lineid)]
    agg_d_num_trains_wide = spread(agg_d_num_trains, lineid,count)
   # interaction term name preparation for aggregating by hour
    speed_name = paste0("speed_bin_", 1:num_speed_bins)
    accel_name = paste0("_","accel_bin_", 1:num_accel_bins,"_time_hr")
    interaction_name = outer(speed_name,accel_name, paste, sep="")
    # aggregate by hour
    sum_cols = c("distance_mile","time_hr",paste0("speed_bin_",1:num_speed_bins,"_time_hr"), paste0("accel_bin_", 1:num_accel_bins,"_time_hr"), interaction_name)
    agg_df = dataframe[, lapply( .SD, sum , na.rm=TRUE), by = c("year","month",'hour',"day"), .SDcols = sum_cols]
    avg_interval_speed_mph_df = dataframe[, lapply( .SD, mean , na.rm=TRUE), by = c("year","month","hour","day"), .SDcols = 'speed_mph']
    agg_df[, 'avg_interval_speed_mph'] = avg_interval_speed_mph_df$speed_mph
    agg_df[, 'avg_hour_speed_mph'] = agg_df$distance_mile/agg_df$time_hr
    merged_agg_df = merge(agg_d_num_trains_wide,agg_df,all=T) 
    return(merged_agg_df)
}

# Combine ridership data
merge_ridership = function(merged_dt,d_ridership){
    print("Merging ridership data")
    merged_dt$year = as.character(merged_dt$year)
    d_ridership$year = as.character(year(d_ridership$servicedate))
    d_ridership$month = as.character(month(d_ridership$servicedate))
    d_ridership$day = as.character(day(d_ridership$servicedate))
    d_ridership$hour = as.character(hour(d_ridership$halfhour))
    d_ridership = d_ridership[,sum(rawtaps_split),by = .(year, month, day,hour)] 
    names(d_ridership)[names(d_ridership) == 'V1'] <- 'ridership'
    merged_db = merge(merged_dt,d_ridership,by = c("year","month","day","hour"),all=F) 
    return(merged_db)
}

# Combine energy consumption data
merge_energy <- function (energy_df,hour_dt) {
    print("Merging energy consumption data")
    # Melt by hour 
    melted_energy_df = melt(energy_df, id.vars=c('Year','Month','Day of Month','WJ','TAVG'), measure.vars = paste0("Hour ",1:24))
        colnames(melted_energy_df) = c('year', 'month', 'day', 'weekends', 'TAVG', 'Hour', 'energy_MWh')
    hour_energy_dt <- setDT(melted_energy_df)
    hour_energy_dt[, Hour := str_replace(Hour, "Hour ", "")]
    hour_energy_dt$Hour = as.numeric(hour_energy_dt$Hour)
    hour_energy_dt$Hour = hour_energy_dt$Hour - 1
    colnames(hour_energy_dt)[6] <- 'hour'
    hour_energy_dt$year = as.character(hour_energy_dt$year)
    hour_energy_dt$month = as.character(hour_energy_dt$month)
    hour_energy_dt$day = as.character(hour_energy_dt$day)
    hour_energy_dt$hour = as.character(hour_energy_dt$hour)
    merged_dt = merge(hour_dt,hour_energy_dt,by = c("year","month","day","hour") , all = F)
    return(merged_dt)
}

main <- function (num_speed_bins, num_accel_bins, energy_df,d_ridership,YEARLIST,MONTHLIST) {
    for (y in YEARLIST) {
        for (m in MONTHLIST) {
             interval_df = line_aggregation(y,m)
             interval_agg <- interval_df %>% unit_transfer() %>% bin_speeds(num_speed_bins) %>% 
             bin_accelerations(num_accel_bins) %>% bin_interaction_terms(num_speed_bins, num_accel_bins) %>% hour_aggregate(num_speed_bins, num_accel_bins)
             # merge with ridership
             merge_ridership = merge_ridership(interval_agg,d_ridership)
             # merge with energy table
             merge_energy = merge_energy(energy_df, merge_ridership)
             write.csv(merge_energy,file.path(paste0(COMPUTATION_FILEPATH, paste(paste("trajectory", "aggregation" , y , m , sep = "-", collapse = ""), ".csv", sep=""))))
            }
      }
   
}

ptm <- proc.time()
main(6, 6, energy_df, d_ridership, YEARLIST, MONTHLIST)
proc.time() - ptm
