library(RMySQL)
library(ggplot2)
library(RColorBrewer)
options(scipen = 10)
library(plyr)
library(nlme)
# Bei Fragen senden über Dashboard:
#Immer erst Devices auswählen, dann frage erstellen / queuen
# View options
# options(digits.secs=6)      
# Initializations

# Remote Database Server Settings:
host_string='80.69.77.149'
user_string='Panel_928'
password_string='GowgQRKC'
dbname_string='Panel_928'
port_string = 3306
database_path = "C:/Users/Felix/Dropbox/Apps/Aware/Database"


# Load database from Remote Aware server 
db_sql <- list()
db_work <- list()
db_full <- list()
date <- Sys.Date()
db_sql = dbConnect(MySQL(), user=user_string, password=password_string, dbname=dbname_string, host=host_string)
tables_database = dbListTables(db_sql)
dbListTables(db_sql)

# Sensors in MySQL Database that should be evaluated
#sensor = list("screen", "applications_history", "applications_foreground", "applications_notifications", "network_traffic", "processor", "calls", "messages", "esms", "mqtt_history", "aware_device")
sensor = list("esms", "aware_device","screen")


# Load full database to list of data frames
for (i in 1:length(tables_database)) {
  # Load database to work with
  if (tables_database[[i]] %in% sensor) {
    db_work[[match(tables_database[[i]], sensor)]] <- dbReadTable(conn = db_sql, name = sensor[[match(tables_database[[i]], sensor)]])
  }
}
names(db_work) <- sensor


#Disconnect from Aware Remote Database
cons <- dbListConnections(MySQL())
for(con in cons) {
  dbDisconnect(con)
}


# Pre-Process

# General Pre-Processing



# add missing device id value of  participant 4015af15-2673-4d7d-b9e7-3586f7bba2f9 that AWARE didn't tracked
db_work[["aware_device"]] <- rbind(db_work[["aware_device"]], db_work[["aware_device"]][1, ])
db_work[["aware_device"]][nrow(db_work[["aware_device"]]), 1] <- 1
db_work[["aware_device"]][nrow(db_work[["aware_device"]]), 2] <- 1478551762000
db_work[["aware_device"]][nrow(db_work[["aware_device"]]), 3] <- "4015af15-2673-4d7d-b9e7-3586f7bba2f9"

# remove researcher device and remove further participants (data cleansing) first 2 - technical problems
remove_devices_researcher <- c("460f0293-0d5a-44a0-b236-d883bb6dbe55")
remove_devices_technical_error <- ("55213ca5-77d0-4563-9c61-5f1670c25d73")
remove_devices_feedback <- c()
remove_devices_variance_smaller_3 <- c()
remove_devices_answers_less_7 <- c()
#remove_devices_answers_less_10 <- c("1ec126bf-f5a6-48d9-8b12-34de44873304","d7d399c0-0e78-4f09-9407-b3ce8e7b98ef","35dbd554-ee93-4dc9-8991-90c9a457c76f")
remove_devices_answers_less_10 <-c()
remove_devices_10_times_balanced <- c()
data_cleansing_feedback <- FALSE
data_cleansing_all <- TRUE
# remove participants with feedback showing no involvement too
if (data_cleansing_feedback==TRUE) {
  #4
  remove_devices_feedback <- c("f5f14668-c994-4f02-b837-bde6dfde6493","b7781340-8544-4eea-897e-01d840e6da5d","4623288b-cf81-4ae3-88ea-f70244b2773d","6c0a744a-3196-4b3a-ae7b-6b81362a9f37")
  data_cleansing <- TRUE
  }
if (data_cleansing_all==TRUE) {
  #6
  remove_devices_variance_smaller_3 <- c("c2448cff-c742-4f43-b1e8-31892ab6d8c0","ccd64e4a-20ad-4c86-967d-1dc73ab9c800","03c78b44-c72a-411e-b25b-8c497e45b5ad","81b6ca40-4e92-4420-ae53-777fa55d01e1","6bf9a272-aa61-4563-9947-edaad0373f3f","fe20aab3-0b36-4499-8ce9-a034de2dd94d")
  #4
  remove_devices_feedback <- c("f5f14668-c994-4f02-b837-bde6dfde6493","b7781340-8544-4eea-897e-01d840e6da5d","4623288b-cf81-4ae3-88ea-f70244b2773d","6c0a744a-3196-4b3a-ae7b-6b81362a9f37")
  #1
  remove_devices_10_times_balanced <- c("f24832ed-f45a-45aa-b001-d4f1b92168eb")
  data_cleansing <- TRUE
  }

remove_devices <- c(remove_devices_researcher,remove_devices_technical_error,remove_devices_feedback,remove_devices_answers_less_7,remove_devices_variance_smaller_3,remove_devices_answers_less_10,remove_devices_10_times_balanced)
for (j in 1:length(remove_devices)) {
  for (i in 1:length(sensor)) {
    db_work[[i]] <- db_work[[i]][!db_work[[i]]$device_id == remove_devices[j],]
  }
}


# delte invalid automatically generated aware device_id=53fc0613-b7e3-4e8e-a9d3-67182b3d3c42 
db_work[["aware_device"]] <- db_work[["aware_device"]][-c(which(db_work[["aware_device"]]$device_id == "53fc0613-b7e3-4e8e-a9d3-67182b3d3c42")), ] 
# delte invalid automatically generated aware device_id=55213ca5-77d0-4563-9c61-5f1670c25d73
#db_work[["aware_device"]] <- db_work[["aware_device"]][-c(which(db_work[["aware_device"]]$device_id == "55213ca5-77d0-4563-9c61-5f1670c25d73")), ] 

# give new unique and simple ids
db_work[["aware_device"]][, "id"] <- NA
for (i in 1:nrow(db_work[["aware_device"]])) {
  db_work[["aware_device"]]$id[i] <- i
}
# set standard device dataframe
dev_N=nrow(db_work[["aware_device"]])
info <- data.frame()
for(dev in 1:dev_N) {
  info[dev, "id"] <- db_work[["aware_device"]]$id[dev]
  info[dev,"device_id"] <- db_work[["aware_device"]]$device_id[dev]
}
  
# calculate on/off time of smartphone
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/Data_Analysis.R")
db_work[["screen"]] <- on_off_function(db_work[["screen"]])


# rename esm columns

names(db_work[["esms"]] )[names(db_work[["esms"]])=="esm_user_answer"] <- "value"
names(db_work[["esms"]] )[names(db_work[["esms"]])=="esm_trigger"] <- "variable"
names(db_work[["esms"]] )[names(db_work[["esms"]])=="double_esm_user_answer_timestamp"] <- "double_end_timestamp"

# delete not answered esm questions
# db_work[["esms"]] <- db_work[["esms"]][!(is.na(db_work[["esms"]]$value) | db_work[["esms"]]$value==""), ]
db_work[["esms"]] <- db_work[["esms"]][!(db_work[["esms"]]$value==""), ]


#assign(paste0("sensor", sensor[["screen"]]), db_work[["screen"]])

# Specific Preprocessing for every sensor data frame

for (i in 1:length(sensor)) {
  frame <- db_work[[i]]
  
  # add time difference to signup date and every id
  # id
  if(("id" %in% colnames(db_work[[i]])))! {
    db_work[[i]][,"id"] <- NaN
  }
  
  # id
  for (j in 1:nrow(frame)) {
    db_work[[i]][j,"id"] <- which(info$device_id == frame$device_id[j])
  }

  # change begin date time format
  try(frame_column <- frame[,c("timestamp")], silent = TRUE)
  try(db_work[[i]]$timestamp <- as.POSIXct((frame_column/1000),origin="1970-01-01",tz="CET"), silent = TRUE)
  
  
  # if answer date is tracked for sensor change time format
  #if("double_esm_user_answer_timestamp" %in% colnames(db_work[[i]])) {
  # reformat time format  
  #try(frame_column <- db_work[[i]]$double_esm_user_answer_timestamp, silent = TRUE)
  #try(db_work[[i]]$double_esm_user_answer_timestamp <- as.POSIXct((frame_column/1000),origin="1970-01-01",tz="CET"), silent = TRUE)
  
  # Calculate time difference double_esm_user_answer_timestamp - timestamp
  #try(temp <-db_work[[i]]$double_esm_user_answer_timestamp, silent = TRUE)
  #try(temp1 <- db_work[[i]]$timestamp, silent = TRUE)
  #try(temp2 <- as.numeric(db_work[[i]]$timestamp[1]), silent = TRUE)
  #try(db_work[[i]]$time_diff <-  as.numeric(db_work[[i]]$double_esm_user_answer_timestamp) - as.numeric(db_work[[i]]$timestamp), silent = TRUE)
#}
#else {
# set standard values to prevent rbindlist merge error
#try(db_work[[i]]["double_esm_user_answer_timestamp"] <- NaN, silent = TRUE)
#try(db_work[[i]]$double_esm_user_answer_timestamp  <- as.POSIXct(db_work[[i]]$double_esm_user_answer_timestamp,origin="1970-01-01",tz="CET"), silent = TRUE)
#}
  
  # if end date is tracked for sensor change time format
  if("double_end_timestamp" %in% colnames(db_work[[i]])) {
    # reformat time format  
    names(db_work[[i]])[names(db_work[[i]])=="double_end_timestamp"] <- "timestamp_end"
    try(frame_column <- db_work[[i]]$timestamp_end, silent = TRUE)
    try(db_work[[i]]$timestamp_end <- as.POSIXct((frame_column/1000),origin="1970-01-01",tz="CET"), silent = TRUE)
    
    # Calculate time difference timestamp_end - timestamp
    try(temp <-db_work[[i]]$timestamp_end, silent = TRUE)
    try(temp1 <- db_work[[i]]$timestamp, silent = TRUE)
    try(temp2 <- as.numeric(db_work[[i]]$timestamp[1]), silent = TRUE)
    try(db_work[[i]]$time_diff <-  as.numeric(db_work[[i]]$timestamp_end) - as.numeric(db_work[[i]]$timestamp), silent = TRUE)
  }
  else {
    # set standard values to prevent rbindlist merge error
    try(db_work[[i]]["timestamp_end"] <- db_work[[i]]["timestamp"], silent = TRUE)
    #try(db_work[[i]]$timestamp_end  <- as.POSIXct(db_work[[i]]$timestamp_end,origin="1970-01-01",tz="CET"), silent = TRUE)
  }
  
  # time diffence
  for (j in 1:nrow(db_work[[i]])) {
    # time difference
    try(db_work[[i]][j,"timestamp_end_diff"] <- as.numeric(db_work[[i]]$timestamp_end[j])/1000 - as.numeric(db_work[["aware_device"]]$timestamp[db_work[[i]][j,"id"]])/1000, silent=TRUE)
  }
  
  # Generate Process info for applications_history  with Google codes: https://developer.android.com/reference/android/app/ActivityManager.RunningAppProcessInfo.html
  if (sensor[[i]] == "applications_history") {
    
    process_info = c("400"="BACKGROUND", "500"="EMPTY", "100"="FOREGROUND", "125"="FOREGROUND_SERVICE", "1000"="GONE", "130"="PERCEPTIBLE", "300"="SERVICE", "150"="TOP_SLEEPING", "200"="VISIBLE", "1"="ROVIDER_IN_USE", "2"="SERVICE_IN_USE", "0"="UNKNOWN")
    db_work[[i]]$process_importance <- plyr::revalue(as.character(db_work[[i]]$process_importance), process_info)
  }
  
  if (sensor[[i]] == "applications_foreground") {
    db_work[[i]]["process_importance"] <- "FOREGROUND*"
  }
  
  
  
  # Build data frame with column sensor name
  db_work[[i]]["sensor"] <- rep(sensor[[i]],nrow(db_work[[i]]))
  
  # Declare AWARE sensors to esm_triggers for easier comparision
  if (sensor[[i]] != "esms") {
    db_work[[i]]["variable"] <- db_work[[i]]["sensor"]
  }
  
  # reset rownames
  rownames(db_work[[i]]) <- seq(length=nrow(db_work[[i]]))
  # Build separate sensor data frames
  assign(paste0("sensor", sensor[[i]]), db_work[[i]])
  
  
  
  merge
  
  
}


# Build combined database
db = as.data.frame(data.table::rbindlist(db_work, fill=TRUE))

# Post processing

db_test_1 <- db
# delete all values later than 2 weeks
delete_study_week<- c()
study_period <- 14*24*60*60
one_week <- 7*24*60*60
# Length of window in s to calculate dependent variable before measurement of independent variable delta t in seconds
dt_min <- 3/4*60*60
# Length of window in s to calculate dependent variable after measurement of independent variable delta t in seconds
dt_max <- 0*60*60
# time interval of participants answers
one_interval <- 3/4*60*60
# delete participants who extend study_period
study_period_extend <- TRUE

for (pos in 1:nrow(db)) {
  # calculate time difference after sign nup
  time_after_signup <- as.numeric(db$timestamp_end[pos]) - as.numeric(db_work[["aware_device"]]$timestamp[db[pos,"id"]])
  # calculate current week of participant
  db[pos,"week"] <- ceiling(time_after_signup/one_week)
  # calculate intervall_id of the measurment 
  db[pos,"interval"] <- ceiling(time_after_signup/one_interval)
  db[pos,"timestamp_end_diff"] <- time_after_signup
  if (time_after_signup > study_period){
    delete_study_week <- c(delete_study_week,pos)
  }
}
if (study_period_extend == TRUE){
  db <- db[-delete_study_week, ]
}


# add colour to each device for plot
#color_palette <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col <- sample(col_vector, dev_N)
for (dev in 1:dev_N) {
  db[db$id == dev, "color"] <- col[dev]
}

# import new databases after preprocessing
db_test_2 <- db
# import feedback answers inputed by the participants on Sosci Survey
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/import_sosci.R")
db <- sosci_import_function(db)

# remove data after 7 days if participant finished
db_test_3 <- db
delete_participants_over_7days <- TRUE
sub_7days_id <- subset(db, variable=="feedback_continue" & as.character(value)=="No, I will leave the study")
delete_7days_id <- sub_7days_id$id
info[info$id %in%  delete_7days_id,"weeks"] <- 1
sub_14days_id <- subset(db, variable=="feedback_continue" & as.character(value)=="Yes, I want to continue for 7 more days")
delete_14days_id <- sub_14days_id$id
info[info$id %in%  delete_14days_id,"weeks"] <- 2
#delete_7days_timestamp_end_diff <- sub_7days_id$timestamp_end_diff
#for (dev in delete_7days_id){
#  a <- db_temp[db_temp$id==dev,][,"timestamp_end_diff"][!is.na(db_temp[db_temp$id==dev,][,"timestamp_end_diff"])]- delete_7days_timestamp_end_diff[which(delete_7days_id==dev)] +60*60*24*7
#  b <-   db_temp[db_temp$id==dev,][,"timestamp_end_diff"][!is.na(db_temp[db_temp$id==dev,][,"timestamp_end_diff"])]
#  b <- a
#  }
sub_7days_time <- subset(db, variable %in% c("screen","esm_boredom_stress") & timestamp_end_diff >60*60*24*7)
sub_7days_time <- sub_7days_time[sub_7days_time$id %in% delete_7days_id,]
delete_7days <- row.names(sub_7days_time)

if (delete_participants_over_7days == TRUE){
  db <- db[!rownames(db) %in% delete_7days, ]
}


# Reorder columns

col_idx <- grep("\\btimestamp\\b", names(db))
db <- db[, c(col_idx, (1:ncol(db))[-col_idx])]
col_idx <- grep("\\bsensor\\b", names(db))
db <- db[, c(col_idx, (1:ncol(db))[-col_idx])]
col_idx <- grep("\\bvalue\\b", names(db))
db <- db[, c(col_idx, (1:ncol(db))[-col_idx])]
col_idx <- grep("\\bvariable\\b", names(db))
db <- db[, c(col_idx, (1:ncol(db))[-col_idx])]
col_idx <- grep("\\btime_diff\\b", names(db))
db <- db[, c(col_idx, (1:ncol(db))[-col_idx])]
col_idx <- grep("\\btimestamp_end_diff\\b", names(db))
db <- db[, c(col_idx, (1:ncol(db))[-col_idx])]
col_idx <- grep("\\btimestamp_end\\b", names(db))
db <- db[, c(col_idx, (1:ncol(db))[-col_idx])]
col_idx <- grep("\\bid\\b", names(db))
db <- db[, c(col_idx, (1:ncol(db))[-col_idx])]

#  Reorder rows
# sort by datejui

db <- dplyr::arrange(db, timestamp)
db <- db[order(db[,"id"],db[,"timestamp_end"]),]
db_N= nrow(db)
rownames(db) <- seq(length=db_N)
db_init <- db


# generate general statistics dataframe
stat <- data.frame()

# start other calc sources
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/nr_2_onoff_calc.R")
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/nr_3_arousal_calc.R")

