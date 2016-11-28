


# calculate time phone on / off
on_off_function <- function(sensor_screen_temp){
  #sensor_screen_temp <- db_work[["screen"]]
  sensor_screen <- sensor_screen_temp[order(sensor_screen_temp$timestamp),]
  
  #assign(paste0("sensor", sensor[["screen"]]), db_work[["screen"]])
  #sensor$timestamp  <- as.POSIXct(sensor$timestamp/1000,origin="1970-01-01",tz="CET")
  
  # declare values
  info_temp <- data.frame()
  n_rows <- nrow(sensor_screen)
  analysis_on_off = data.frame()
  value <- NaN
  double_end_timestamp <- NaN
  cor_time_diff <- NaN
  cor_time <- TRUE
  timestamp <- NaN
  device_id <- NaN
  id <- NaN
  X_id <- NaN
  i <-0
  
  # Screen is also tracked when smartphone user is not at smarpthone (for exmample charging in the night)
  # Set filter with max "on" time, closed Screen event away from maximal on time is captured and new on-off time calculated out of it
  max_screen_dt <-5*60*60 # deactivate filter (s) with Inf

  # set info_temp frame
  for(dev in 1:dev_N) {
    info_temp[dev, "id"] <- dev
    info_temp[dev,"device_id"] <- db_work[["aware_device"]]$device_id[dev]
    info_temp[dev,"time"] <- NaN
    info_temp[dev,"status"] <- 1
  }
    


  for (pos in 1:n_rows) {
      dev <- which(info_temp$device_id == sensor_screen$device_id[pos])
      if(abs(sensor_screen$screen_status[pos] - info_temp$status[dev]) == 1) {
        # if first screen entry
        if(is.nan(info_temp[dev,"time"])) {
          info_temp$status[dev] <- sensor_screen$screen_status[pos] 
          info_temp$time[dev] <- sensor_screen$timestamp[pos]
          
        }else{
          info_temp$status[dev] <- sensor_screen$screen_status[pos] 
          device_id <- c(device_id, sensor_screen$device_id[pos])
          X_id <- c(X_id, sensor_screen$X_id[pos])
          id <- c(id, info_temp$id[dev])
          # diff = as.numeric(sensor_screen$timestamp[pos]) - as.numeric(time)
          if (info_temp$status[dev]==1) {
            value <- c(value,"off")
          } else {
            value <- c(value,"on")
          }
          
          #timediff <- c(timediff, c(diff))
          
          #value over max tolerance
          cor_time <- c(cor_time,TRUE)
          cor_time_diff <- c(cor_time_diff,(sensor_screen$timestamp[pos]-info_temp$time[dev])/1000)
          if (sensor_screen$timestamp[pos]-info_temp$time[dev] > max_screen_dt*1000 & value[length(value)] == "on" ){
            cor_time[length(cor_time)] <- FALSE
            #correct old value of ending off time
            #print("hallo")
            #print(sensor_screen$X_id[pos])
            corrected_value <- sensor_screen$timestamp[pos]-max_screen_dt*1000
            #take last sensor report if it is a better approximation of last value
            if (sensor_screen$timestamp[pos-1]> corrected_value) {
              # print("device")
              # print(sensor_screen$device_id[pos])
              # print("x_id")
              # print(sensor_screen$X_id[pos])
              # print("old")
              # print(sensor_screen$timestamp[pos-1])
              # print("new")
              # print(sensor_screen$timestamp[pos])
              corrected_value <- sensor_screen$timestamp[pos-1]
            }
            info_temp$time[dev] <- corrected_value
            
          }
          
          timestamp <- c(timestamp, c(info_temp$time[dev]))
          info_temp$time[dev] <- sensor_screen$timestamp[pos]
          double_end_timestamp <- c(double_end_timestamp, c(info_temp$time[dev]))


          #print(X_id)
        } 
        
        }else{
          if(sensor_screen$screen_status[pos] == info_temp$status[dev] & (sensor_screen$screen_status[pos]==1 || sensor_screen$screen_status[pos]==0)){
            info_temp$time[dev] <- sensor_screen$timestamp[pos]
            info_temp$status[dev] <- sensor_screen$screen_status[pos] 
            #i <- i+1

           }
        }

  }
  
  #timediff <- timediff/1000

  #timestamp  <- as.POSIXct(timestamp/1000,origin="1970-01-01",tz="CET")
  #double_end_timestamp  <- as.POSIXct(double_end_timestamp/1000,origin="1970-01-01",tz="CET")
  
  analysis_on_off <-data.frame(timestamp,double_end_timestamp,value,device_id,id,X_id,cor_time_diff,cor_time)
  # Delete first two (calculated) rows, as no useful info_temprmation
  analysis_on_off <- analysis_on_off[-c(1), ]
  rownames(analysis_on_off) <- seq(length=nrow(analysis_on_off))
  #analysis_on_off[,"diff"] <- analysis_on_off$double_end_timestamp-analysis_on_off$timestamp
  return(analysis_on_off)
  
}

#test <- on_off_function(db_work[["screen"]])