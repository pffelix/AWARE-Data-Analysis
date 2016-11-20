#  Arousal descriptive Analysis functions

 
#calculate normalized arousal level
db_temp <- data.frame()
db_temp <- db
db_temp[,"arousal_z"] <- NaN
db_temp[,"arousal"] <- NaN
arousal <- data.frame()

#mean <- list()
for (dev in 1:dev_N) {
  stat[["dev"]] <- data.frame()
  # transform arousal values to positive scale
  search_frame <- subset(db_temp, variable == "esm_boredom_stress" & id == dev) 
  row_names = row.names(search_frame)
  for(i in 1:length(row_names)) {
    pos <- as.numeric(row_names[i])
    value <- as.numeric(as.character(db_temp$value[pos])) + 2
    db_temp[pos,"arousal"] <- value
  }
  
  # calc descriptive values
  search_frame <- subset(db_temp, variable == "esm_boredom_stress" & id == dev) 
  arousal[dev, "id"] <- dev
  arousal[dev, "mean"] <- mean(search_frame$arousal)
  arousal[dev, "median"] <- median(search_frame$arousal)
  arousal[dev, "sd"] <- sd(search_frame$arousal)
  arousal[dev, "N"] <- nrow(search_frame)
  arousal[dev, "duration"] <-max(search_frame$timestamp_end_diff)
  # normalize arousal values to z-scale
  
  #print(mean)
  #print(sd)
  for(i in 1:length(row_names)) {
    pos <- as.numeric(row_names[i])
    #print(row_names[i])
    arousal_z <- (db_temp$arousal[pos] - arousal$mean[dev])/arousal$sd[dev]
    #print(arousal_z)
    db_temp[pos,"arousal_z"] <- arousal_z
  }
  #search_frame <- subset(db_temp, variable == "esm_boredom_stress" & id == dev) 
  #search_frame$value <- as.numeric(as.character(search_frame$arousal_z))
  #mean <- mean(search_frame$arousal_z)
  #sd <- sd(search_frame$arousal_z)
  #print(dev)
  #print(mean)
  #print(sd)  
}
arousal_N <- sum(arousal$N)


# interpolate arousal level
type = 1
reset = 1
db_temp[,"arousalp"] <- NA
db_temp[,"arousalp_z"] <- NA
#value_1 <-NaN
#value_2 <-NaN
#time_1 <- NaN
#time_2 <- NaN

search_frame <- list()
search_frame_N <- list()
for(dev in 1:dev_N) {
  search_frame[[dev]] <- subset(db_temp, variable == "esm_boredom_stress" & id == dev) 
  rownames(search_frame[[dev]]) <- seq(length=nrow(search_frame[[dev]]))
  search_frame_N[[dev]] <- nrow(search_frame[[dev]])
}


for (pos in 1:db_N) {
  
  time <- as.numeric(db_temp$timestamp_end[pos])
  dev <- db_temp$id[pos]
  if(is.na(dev)){
    next
  }
  #arousal_vector <- arousal_vector[order(arousal_vector$X_id),]

  #n_rows <- nrow(arousal_vector)
  #print(time)
  #print(db_temp$timestamp_end[i])
  #print("new")
  #print(reset)
  # new id -> reset
  if (reset == 1){
    t = -1
    pos_search=0
    reset = 0
  }
  #print(t)
  while((t < 0 & pos_search<search_frame_N[[dev]])){
    pos_search <- pos_search + 1
    #print(pos_search)
    #print(t)

    t = as.numeric(search_frame[[dev]]$timestamp_end[pos_search]) - time
    # if all values of the same id iterated
    # print(pos_search)
  }
  #print(pos_search)
  # if invalid value
  if (pos_search==1 || pos_search==search_frame_N[[dev]]) {
    if(is.nan(db_temp[pos,"arousal"])) {
      arousalp = NaN
      arousalp_z = NaN
    } else{
      arousalp <- db_temp[pos,"arousal"]
      arousalp_z <- db_temp[pos,"arousal_z"]
    }
  } else{
    value_1 <- search_frame[[dev]][pos_search-1,"arousal"]
    time_1 <- as.numeric(search_frame[[dev]][pos_search-1,"timestamp_end"])
    value_2 <- search_frame[[dev]][pos_search,"arousal"]
    time_2 <- as.numeric(search_frame[[dev]][pos_search,"timestamp_end"])
    
    # Linear interpolation
    if (type==1) {
      arousalp = value_1 + (time-time_1)*(value_2-value_1)/(time_2-time_1)
    }
  }
  db_temp[pos,"arousalp"] <- arousalp
  arousalp_z <- (arousalp - arousal$mean[dev])/arousal$sd[dev]
  db_temp[pos,"arousalp_z"] <- arousalp_z
  
  #print(value)
  reset = 1
  #print(dev)
  #print(pos)
}

col_idx <- grep("\\barousal_z\\b", names(db_temp))
db_temp <- db_temp[, c(col_idx, (1:ncol(db_temp))[-col_idx])]
col_idx <- grep("\\barousalp_z\\b", names(db_temp))
db_temp <- db_temp[, c(col_idx, (1:ncol(db_temp))[-col_idx])]
col_idx <- grep("\\barousal\\b", names(db_temp))
db_temp <- db_temp[, c(col_idx, (1:ncol(db_temp))[-col_idx])]
col_idx <- grep("\\barousalp\\b", names(db_temp))
db_temp <- db_temp[, c(col_idx, (1:ncol(db_temp))[-col_idx])]
col_idx <- grep("\\bid\\b", names(db_temp))
db <- db_temp[, c(col_idx, (1:ncol(db_temp))[-col_idx])]

#return(value)


