


# calculate time phone on / off
on_off_function <- function(sensor_screen_temp){
  #sensor_screen_temp <- db_work[["screen"]]
  sensor_screen <- sensor_screen_temp[order(sensor_screen_temp$timestamp),]
  
  #assign(paste0("sensor", sensor[["screen"]]), db_work[["screen"]])
  #sensor$timestamp  <- as.POSIXct(sensor$timestamp/1000,origin="1970-01-01",tz="CET")
  
  # declacre values
  info <- data.frame()
  n_rows <- nrow(sensor_screen)
  analysis_on_off = data.frame()
  value <- NaN
  double_end_timestamp <- NaN
  timestamp <- NaN
  device_id <- NaN
  id <- NaN
  X_id <- NaN
  

  # set info frame
  for(dev in 1:dev_N) {
    info[dev, "id"] <- dev
    info[dev,"device_id"] <- db_work[["aware_device"]]$device_id[dev]
    info[dev,"time"] <- NaN
    info[dev,"status"] <- 1
  }
    


  for (pos in 1:n_rows) {
      dev <- which(info$device_id == sensor_screen$device_id[pos])
      if(abs(sensor_screen$screen_status[pos] - info$status[dev]) == 1) {
        # if first screen entry
        if(is.nan(info[dev,"time"])) {
          info$status[dev] <- sensor_screen$screen_status[pos] 
          info$time[dev] <- sensor_screen$timestamp[pos]
          
        }else{
          info$status[dev] <- sensor_screen$screen_status[pos] 
          device_id <- c(device_id, sensor_screen$device_id[pos])
          X_id <- c(X_id, sensor_screen$X_id[pos])
          id <- c(id, info$id[dev])
          # diff = as.numeric(sensor_screen$timestamp[pos]) - as.numeric(time)
          if (info$status[dev]==1) {
            value <- c(value,"off")
          } else {
            value <- c(value,"on")
          }
          #timediff <- c(timediff, c(diff))
          timestamp <- c(timestamp, c(info$time[dev]))
          info$time[dev] <- sensor_screen$timestamp[pos]
          double_end_timestamp <- c(double_end_timestamp, c(info$time[dev]))
        }

      }
  }
  
  #timediff <- timediff/1000

  #timestamp  <- as.POSIXct(timestamp/1000,origin="1970-01-01",tz="CET")
  #double_end_timestamp  <- as.POSIXct(double_end_timestamp/1000,origin="1970-01-01",tz="CET")
  
  analysis_on_off <-data.frame(timestamp,double_end_timestamp,value,device_id,id,X_id)
  # Delete first two (calculated) rows, as no useful information
  analysis_on_off <- analysis_on_off[-c(1), ]
  rownames(analysis_on_off) <- seq(length=nrow(analysis_on_off))
  return(analysis_on_off)
  
}

