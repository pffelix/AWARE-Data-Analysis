
#calculate on/off usage
db_temp <- data.frame()
db_temp <- db
onoff <- data.frame()
for (dev in 1:dev_N) {
  search_frame <- subset(db_temp, variable == "screen" & id == dev & value == "on") 
  row_names = row.names(search_frame)
  for(i in 1:length(row_names)) {
  }
  
  onoff[dev, "id"] <- dev
  onoff[dev, "N"] <- nrow(search_frame)
  onoff[dev, "tot"] <- sum(search_frame$time_diff)
  onoff[dev, "tot_mean"] <- search_frame$timestamp_end_diff[onoff[dev, "N"]]/onoff[dev, "tot"]

  }
  
