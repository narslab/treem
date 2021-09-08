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
YEARLIST = c("20")
MONTHLIST = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
DISTANCE_FILEPATH = "../../data/tidy/vehicle-trajectory-computation/"
COMPUTATION_FILEPATH = "../../data/tidy/"
energy_df = fread("../../data/raw/energy-consumption-08-20.csv") # Read in energy data
d_ridership = fread("../../data/raw/ridership-2019-2020.csv")# Read in ridership data
NUM_SPEED_BINS = 6
NUM_ACCEL_BINS = 6
SPEED_CUTS = data.frame(read.csv(paste0("../../data/tidy/speed-19-cutpoints-bins-", NUM_SPEED_BINS, ".csv")))$cutpoints
ACCEL_CUTS = data.frame(read.csv(paste0("../../data/tidy/acceleration-19-cutpoints-bins-", NUM_ACCEL_BINS, ".csv")))$cutpoints

# aggregrate_trajectory_table
line_aggregation = function(year,month){
    assign("dg",fread(paste0(DISTANCE_FILEPATH, paste(paste("green", "trajectory", year, month, sep = "-", collapse = ""), ".csv", sep=""))))
    assign("dh",fread(paste0(DISTANCE_FILEPATH, paste(paste("heavy", "trajectory", year, month, sep = "-", collapse = ""), ".csv", sep=""))))
    dg$lineid = 4
    dg = subset(dg, select = c(trxtime, year, month, day, lineid, lat, lon , speed_kph , accel_mps2 , interval_seconds , dist_meters , vehicleid))
    dh = subset(dh, select = c(trxtime, year, month, day, lineid, lat, lon , speed_kph , accel_mps2 , interval_seconds , dist_meters , vehicleid))
    df = rbind(dg, dh) 
    remove(dg)
    remove(dh)
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
  dummy_cols = c(paste0("speed_bin_",1:num_bins,"_dummy"))
  bin_time_cols = c(paste0("speed_bin_",1:num_bins,"_time_hr"))
  print("Reading speed bins")
  cutpoints = SPEED_CUTS
  print(round(cutpoints,2))
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
    dummy_cols = c(paste0("accel_bin_",1:num_bins,"_dummy"))
    bin_time_cols = c(paste0("accel_bin_",1:num_bins,"_time_hr"))
    print("Reading acceleration bins")
    cutpoints = ACCEL_CUTS 
    print(round(cutpoints,2))    
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
hour_aggregate <- function (dt, num_speed_bins, num_accel_bins) {
    print("Aggregating observations by hour")
    #dt = data.table(dt)
    dt$month = as.character(dt$month)
    dt$hour = as.character(dt$hour)
    dt$day = as.character(dt$day)
    # create another data table to summarize the number of trains running in each hour
    d_num_trains <- dt[, c("month",'hour',"day","lineid","vehicleid")]
    agg_d_num_trains = d_num_trains[, .(count = length(unique(vehicleid))), by = .(month,day,hour,lineid)]
    agg_d_num_trains_wide = spread(agg_d_num_trains, lineid,count)
   # interaction term name preparation for aggregating by hour
    speed_name = paste0("speed_bin_", 1:num_speed_bins)
    accel_name = paste0("_","accel_bin_", 1:num_accel_bins,"_time_hr")
    interaction_name = outer(speed_name,accel_name, paste, sep="")
    # aggregate by hour
    sum_cols = c("distance_mile","time_hr",paste0("speed_bin_",1:num_speed_bins,"_time_hr"), paste0("accel_bin_", 1:num_accel_bins,"_time_hr"), interaction_name)
    agg_dt = dt[, lapply( .SD, sum , na.rm=TRUE), by = c("year","month",'hour',"day"), .SDcols = sum_cols]
    avg_interval_speed_mph_dt = dt[, lapply( .SD, mean , na.rm=TRUE), by = c("year","month","hour","day"), .SDcols = 'speed_mph']
    agg_dt[, 'avg_interval_speed_mph'] = avg_interval_speed_mph_dt$speed_mph
    agg_dt[, 'avg_hour_speed_mph'] = agg_dt$distance_mile/agg_dt$time_hr
    merged_agg_dt = merge(agg_d_num_trains_wide, agg_dt, all=T) 
    remove(dt)
    remove(agg_d_num_trains_wide)
    remove(agg_dt)
    return(merged_agg_dt)
}

# Combine ridership data
merge_ridership = function(merged_dt, d_ridership){
    print("Merging ridership data")
    merged_dt$year = as.character(merged_dt$year)
    d_ridership$year = as.character(year(d_ridership$servicedate))
    d_ridership$month = as.character(month(d_ridership$servicedate))
    d_ridership$day = as.character(day(d_ridership$servicedate))
    d_ridership$hour = as.character(hour(d_ridership$halfhour))
    d_ridership = d_ridership[,sum(rawtaps_split),by = .(year, month, day,hour)] 
    names(d_ridership)[names(d_ridership) == 'V1'] <- 'ridership'
    merged_db = merge(merged_dt, d_ridership, by = c("year","month","day","hour"),all=F) 
    remove(merged_dt)
    remove(d_ridership)
    return(merged_db)
}

# Combine energy consumption data
merge_energy <- function (energy_df, hour_dt) {
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
    merged_dt = merge(hour_dt, hour_energy_dt, by = c("year","month","day","hour") , all = F)
    remove(energy_df)
    remove(hour_dt)
    remove(hour_energy_dt)
    return(merged_dt)
}

main <- function (num_speed_bins, num_accel_bins, energy_df,d_ridership, year_list, month_list) {
    for (y in year_list) {
        for (m in month_list) {
             interval_df = line_aggregation(y, m)
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

main(NUM_SPEED_BINS, NUM_ACCEL_BINS, energy_df, d_ridership, YEARLIST, MONTHLIST)
