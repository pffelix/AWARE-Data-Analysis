
# data preparation for multi-level model  

db_temp <- db_init
delete_dt <- c()
delete_arousal_wrong_dt <- c()
delete_arousal_wrong_off <- c()
old_time <- -Inf
# window factor usage
dt <- (dt_min+dt_max)/60/60

# Dele wrong db entries (mood state often answered more than one time at interval dt_t)

for (dev in 1:dev_N){
  sub <- subset(db_temp, variable == "esm_boredom_stress" & id == dev)
  
  for(j in 1:nrow(sub)){
    if(abs(old_time - sub$timestamp_end_diff[j])<=dt_min + dt_max){
      #print(old_time)
      # wrong AWARE Plugin database entry, arousal measurements too close, not dt hour intervalls -> delete (delete_arousal_wrong_dt)
      delete_arousal_wrong_dt <-c(delete_arousal_wrong_dt, row.names(sub)[j])
    } else{
      old_time <- sub$timestamp_end_diff[j]
    }
  }
  old_time <- -Inf
}

db_temp <- db_temp[!rownames(db_temp) %in% delete_arousal_wrong_dt, ]
db_temp <- dplyr::arrange(db_temp, timestamp)
db_temp <- db_temp[order(db_temp[,"id"],db_temp[,"timestamp_end"]),]
db_N= nrow(db_temp)
rownames(db_temp) <- seq(length=db_N)


# calculate on/off usage to arousal event

usage_time <-  c()
usage_freq <-  c()
sub_screen <- subset(db_temp, variable == "screen" & value == "on")
sub_arousal <- subset(db_temp, variable == "esm_boredom_stress")
name_row <- rownames(sub_arousal)
for (dev in 1:dev_N){
  #dev <- 31
  sub_arousal_dev <- subset(sub_arousal, id == dev)
  name_row_dev <- rownames(sub_arousal_dev)
  for (i in 1:nrow(sub_arousal_dev)){
    if (sub_arousal_dev$X_id[i] ==1183){
      #print("hall")
    }
    time_sub <- sub_arousal_dev[name_row_dev[i],"timestamp_end_diff"]
    time_min <- time_sub - dt_min
    #if(time_min < 0){
      #time_min <- 0
    #}
    time_max <- time_sub + dt_max
    sub_screen_time <- subset(sub_screen, id == dev & timestamp_end_diff >= time_min & timestamp_end_diff <= time_diff+time_max, )
    
    if (nrow(sub_screen_time) <1){
      delete_arousal_wrong_off <-c(delete_arousal_wrong_off, name_row_dev[i])
      usage_time <- c(usage_time, 0)
      usage_freq <- c(usage_freq, 0)
      next
    }
    
    # correct overlapping intervalls
    for (j in 1:nrow(sub_screen_time)){
      # wrong AWARE database entry, arousal entry wherby smartphone was off -> delete (delete_arousal_wrong_off)
      timestamp_diff <- sub_screen_time$timestamp_end_diff[j]-sub_screen_time$time_diff[j]
      if(sub_screen_time$timestamp_end_diff[j] > time_max){
        #print(sub_screen_time$timestamp_end_diff[j]-time_max)
        sub_screen_time$time_diff[j] <- sub_screen_time$time_diff[j]-(sub_screen_time$timestamp_end_diff[j]-time_max)
      }
      if(timestamp_diff < time_min){
        #print(time_min-timestamp_diff)
        sub_screen_time$time_diff[j] <- sub_screen_time$time_diff[j]-(time_min-timestamp_diff)
      }
    }

    usage_freq <- c(usage_freq,nrow(sub_screen_time))
    # correct AWARE error in database high frequency [366] in milliseconds change 
    if (sub_arousal_dev$X_id[i] == 6041 & usage_freq[length(usage_freq)] >100){
      usage_freq[length(usage_freq)] <- usage_freq[length(usage_freq)] - (351-17)
    }    
    usage_time <- c(usage_time, sum(sub_screen_time$time_diff))
    #print (position)
  }
  
}
db_temp[name_row,"usage_time"] <- usage_time
db_temp[name_row,"usage_freq"] <- usage_freq

db_temp <- db_temp[!rownames(db_temp) %in% delete_arousal_wrong_off, ]
db_temp <- dplyr::arrange(db_temp, timestamp)
db_temp <- db_temp[order(db_temp[,"id"],db_temp[,"timestamp_end"]),]
db_N= nrow(db_temp)
rownames(db_temp) <- seq(length=db_N)
db <- db_temp


#calculate on/off usage
db_temp <- data.frame()
db_temp <- db
onoff <- data.frame()
for (dev in 1:dev_N) {
  search_frame <- subset(db_temp, variable == "screen" & id == dev & value == "on") 
  row_names <- row.names(search_frame)
  
  onoff[dev, "id"] <- dev
  onoff[dev, "N"] <- nrow(search_frame)
  onoff[dev, "tot"] <- sum(search_frame$time_diff)
  onoff[dev, "freq"] <- search_frame$timestamp_end_diff[onoff[dev, "N"]]/onoff[dev, "tot"]
  onoff[dev,"usage_time_day"] <- onoff[dev, "tot"]/(search_frame$timestamp_end_diff[onoff[dev, "N"]])*24*60*60
  onoff[dev,"usage_freq_day"] <- onoff[dev, "N"]/(search_frame$timestamp_end_diff[onoff[dev, "N"]])*24*60*60
}
