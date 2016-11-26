
sosci_import_function <- function(db_input){
  # Insert CSV from SosciSurvey into R dataframe data
  
  #data_file = file.choose()
  # setwd("./")
  data_file = "C:/Users/Felix/Dropbox/Apps/Aware/Database/Sosci_Survey/rdata_feedback_aware_2016-11-26_17-13.csv"
  
  data = read.table(
    file=data_file, encoding="UTF-8",
    header = FALSE, sep = "\t", quote = "\"",
    dec = ".", row.names = "CASE",
    col.names = c(
      "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","A014_01","A001_01","A002_01",
      "A003_01","A004_01","A005_01","A006_01","A007_01","A008_01","A009_01","A010_01",
      "A011_01","A012_01","A013","TIME001","TIME_SUM","MAILSENT","LASTDATA",
      "FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE","MISSING","MISSREL","TIME_RSI",
      "DEG_TIME"
    ),
    as.is = TRUE,
    colClasses = c(
      "numeric","character","character","character","character","POSIXct","character",
      "numeric","numeric","numeric","character","character","numeric","numeric",
      "numeric","numeric","numeric","numeric","character","numeric","integer",
      "integer","POSIXct","POSIXct","logical","logical","numeric","numeric","numeric",
      "numeric","numeric","numeric"
    ),
    skip = 1,
    check.names = TRUE, fill = TRUE,
    strip.white = FALSE, blank.lines.skip = TRUE,
    comment.char = "",
    na.strings = ""
  )
  
  rm(data_file)
  
  attr(data, "project") = "feedback_aware"
  attr(data, "description") = "feedback_aware"
  attr(data, "date") = "2016-11-20 11:30:49"
  attr(data, "server") = "https://www.soscisurvey.de"
  
  # Variable und Value Labels
  data$A013 = factor(data$A013, levels=c("1","2","-9"), labels=c("Yes, I want to continue for 7 more days","No, I will leave the study","[NA] Not answered"), ordered=FALSE)
  attr(data$A001_01,"1") = "strongly disagree"
  attr(data$A001_01,"5") = "strongly agree"
  attr(data$A002_01,"1") = "strongly disagree"
  attr(data$A002_01,"5") = "strongly agree"
  attr(data$A003_01,"1") = "bad"
  attr(data$A003_01,"5") = "great"
  attr(data$A006_01,"1") = "strongly disagree"
  attr(data$A006_01,"5") = "strongly agree"
  attr(data$A007_01,"1") = "strongly disagree"
  attr(data$A007_01,"5") = "strongly agree"
  attr(data$A008_01,"1") = "strongly disagree"
  attr(data$A008_01,"5") = "strongly agree"
  attr(data$A009_01,"1") = "strongly disagree"
  attr(data$A009_01,"5") = "strongly agree"
  attr(data$A010_01,"1") = "strongly disagree"
  attr(data$A010_01,"5") = "strongly agree"
  attr(data$A011_01,"1") = "strongly disagree"
  attr(data$A011_01,"5") = "strongly agree"
  attr(data$FINISHED,"F") = "Canceled"
  attr(data$FINISHED,"F") = "Finished"
  attr(data$Q_VIEWER,"F") = "Respondent"
  attr(data$Q_VIEWER,"F") = "Spectator"
  comment(data$SERIAL) = "Serial number (if provided)"
  comment(data$REF) = "Reference (if provided in link)"
  comment(data$QUESTNNR) = "Questionnaire that has been used in the interview"
  comment(data$MODE) = "Interview mode"
  comment(data$STARTED) = "Time the interview has started"
  comment(data$A014_01) = "feedback_id: [01]"
  comment(data$A001_01) = "week_stress: strongly disagree/strongly agree"
  comment(data$A002_01) = "week_boredom: strongly disagree/strongly agree"
  comment(data$A003_01) = "feedback_overall: [No Description] 01"
  comment(data$A004_01) = "feedback_app: [01]"
  comment(data$A005_01) = "feedback_questions: [01]"
  comment(data$A006_01) = "feedback_esm_identify: strongly disagree/strongly agree"
  comment(data$A007_01) = "feedback_esm_frequency: strongly disagree/strongly agree"
  comment(data$A008_01) = "feedback_esm_reliability: strongly disagree/strongly agree"
  comment(data$A009_01) = "feedback_esm_intuitive: strongly disagree/strongly agree"
  comment(data$A010_01) = "feedback_monitoring: strongly disagree/strongly agree"
  comment(data$A011_01) = "feedback_reactivity: strongly disagree/strongly agree"
  comment(data$A012_01) = "feedback_extra: [01]"
  comment(data$A013) = "feedback_continue"
  comment(data$TIME001) = "Time spent on page 1"
  comment(data$TIME_SUM) = "Time spent overall (except outliers)"
  comment(data$MAILSENT) = "Time when the invitation mailing was sent (non-anonymous recipients, only)"
  comment(data$LASTDATA) = "Time when the data was most recently updated"
  comment(data$FINISHED) = "Has the interview been finished (reached last page)?"
  comment(data$Q_VIEWER) = "Did the respondent only view the questionnaire, omitting mandatory questions?"
  comment(data$LASTPAGE) = "Last page that the participant has handled in the questionnaire"
  comment(data$MAXPAGE) = "Hindmost page handled by the participant"
  comment(data$MISSING) = "Missing answers in percent"
  comment(data$MISSREL) = "Missing answers (weighted by relevance)"
  comment(data$TIME_RSI) = "Degradation points for being very fast"
  comment(data$DEG_TIME) = "Degradation points for being very fast"
  
  
  
  # Assure that the comments are retained in subsets
  as.data.frame.avector = as.data.frame.vector
  `[.avector` <- function(x,i,...) {
    r <- NextMethod("[")
    mostattributes(r) <- attributes(x)
    r
  }
  data_tmp = data.frame(
    lapply(data, function(x) {
      structure( x, class = c("avector", class(x) ) )
    } )
  )
  mostattributes(data_tmp) = attributes(data)
  data = data_tmp
  rm(data_tmp)
  
  
  # Integrate data in exsiting AWARE database structure
  
  data_temp <- data
  
  # conversion_character <- c("week_stress", "week_boredom", "feedback_overall", "feedback_app","feedback_questions","feedback_esm_identify","feedback_esm_frequency","feedback_esm_reliability","feedback_esm_intuitive","feedback_monitoring","feedback_reactivity","feedback_continue")
  # for (con in 1:length(conversion_character)) {
  #   rows <- which(db_input$variable == conversion_character[con])
  #   if (con ==4 || con==5 || con==12) {
  #     #db_input[rows,"value"] <- as.character(db_input[rows,"value"])
  #   } else if(con == 3) {
  #     print(db_input[rows,"value"])
  #     temp_Value <- as.character((db_input[rows,"value"]))
  #     print(temp_Value)
  #     db_input[rows,"value"] <- temp_Value
  #   }
  #   else{
  #   #db_input[rows,"value"] <- as.character(as.numeric(as.character(db_input[rows,"value"]))+2)
  #   }
  # }
  
  
  # week_stress
  # week_boredom
  # #feedback_overall
  # #feedback_app
  # #feedback_questions
  # feedback_esm_identify
  # feedback_esm_frequency
  # feedback_esm_reliability
  # feedback_esm_intuitive
  # feedback_monitoring
  # feedback_reactivity
  # #feedback_continue
  
  variable_name <- c("week_stress","week_boredom","feedback_overall","feedback_app","feedback_questions","feedback_esm_identify","feedback_esm_frequency","feedback_esm_reliability","feedback_esm_intuitive","feedback_monitoring","feedback_reactivity","feedback_extra","feedback_continue")
  
  names(data_temp)[names(data_temp)=="A014_01"] <- "device_id"
  names(data_temp)[names(data_temp)=="A001_01"] <- "week_stress"
  names(data_temp)[names(data_temp)=="A002_01"] <- "week_boredom"
  names(data_temp)[names(data_temp)=="A003_01"] <- "feedback_overall"
  names(data_temp)[names(data_temp)=="A004_01"] <- "feedback_app"
  names(data_temp)[names(data_temp)=="A005_01"] <- "feedback_questions"
  names(data_temp)[names(data_temp)=="A006_01"] <- "feedback_esm_identify"
  names(data_temp)[names(data_temp)=="A007_01"] <- "feedback_esm_frequency"
  names(data_temp)[names(data_temp)=="A008_01"] <- "feedback_esm_reliability"
  names(data_temp)[names(data_temp)=="A009_01"] <- "feedback_esm_intuitive"
  names(data_temp)[names(data_temp)=="A010_01"] <- "feedback_monitoring"
  names(data_temp)[names(data_temp)=="A011_01"] <- "feedback_reactivity"
  names(data_temp)[names(data_temp)=="A012_01"] <- "feedback_extra"
  names(data_temp)[names(data_temp)=="A013"] <- "feedback_continue"
  names(data_temp)[names(data_temp)=="LASTDATA"] <- "timestamp_end"
  names(data_temp)[names(data_temp)=="STARTED"] <- "timestamp"
  
  no_id <- list()
  rownames(data_temp) <- seq(length=nrow(data_temp))
  
  # delete particpants who were ignored because of data cleansing
  if (data_cleansing==TRUE){
    remove_vector <- c()
    for (j in 1:nrow(data_temp)) {
      id <- which(substr(as.character(remove_devices),1,7) == substr(as.character(data_temp$device_id[j]),start=1,stop=7))
      #print(id)
      if (length(id)!=0) {
        remove_vector <- c(remove_vector,c(j))
      }
    }
  data_temp <- data_temp[-remove_vector, ]
  }
  rownames(data_temp) <- seq(length=nrow(data_temp))
  
  
  # id aware ids to rest of data
  for (j in 1:nrow(data_temp)) {
    id <- which(substr(as.character(info$device_id),1,7) == substr(as.character(data_temp$device_id[j]),start=1,stop=7))
    if(length(id)==0) {
      no_id <- c(no_id,data_temp$device_id[j])
      if (length(no_id) >5) {
        warning("no id list is longer than on 26.11.2016")
        
      }
      data_temp[j,"device_id"] <- NA
      data_temp[j,"id"] <- NA
  
      
    }else{
      data_temp[j,"id"] <- id
      data_temp[j,"device_id"] <- info$device_id[id]
      
    }
  }
  
  data_temp[,"timestamp"] <- as.POSIXct(data_temp$timestamp,origin="1970-01-01",tz="CET")
  data_temp[,"timestamp_end"] <- as.POSIXct(data_temp$timestamp_end,origin="1970-01-01",tz="CET")
  data_temp[,"time_diff"] <-  as.numeric(data_temp$timestamp_end) - as.numeric(data_temp$timestamp)
  
  for (j in 1:nrow(data_temp)) {
    # time difference
    data_temp[j,"timestamp_end_diff"] <- as.numeric(data_temp$timestamp_end[j])- as.numeric(db_work[["aware_device"]]$timestamp[data_temp[j,"id"]])
  }
  
  
  
  data_pre <- data.frame("device_id")
  data_pre <- data.frame(matrix(0, ncol = 0, nrow = nrow(data_temp)*length(variable_name)))
  data_pre_temp <- data.frame("device_id")
  #data_pre_temp <- data.frame(matrix(0, ncol = 0, nrow = nrow(data_temp)))
  data_pre_temp <- subset(db_input, variable == "demographic_gender" & id==1)
  data_pre_temp$value <- as.character(data_pre_temp$value)
  data_pre_temp <- data_pre_temp[rep(seq_len(nrow(data_pre_temp)), each=nrow(data_temp)),]
  data_pre_temp <- data_pre_temp[0, ]
  
  for (variable_pos in 1:length(variable_name)) {
    for(pos in 1: nrow(data_temp)) {
      pos_temp <- (variable_pos-1)*nrow(data_temp) + pos
      data_pre_temp[pos_temp,"device_id"] <- data_temp$device_id[pos]
      data_pre_temp[pos_temp,"id"] <- data_temp$id[pos]
      data_pre_temp[pos_temp,"sensor"] <- "esms"
      data_pre_temp[pos_temp,"X_id"] <- 0
      data_pre_temp[pos_temp,"timestamp_end"] <- data_temp$timestamp_end[pos]
      data_pre_temp[pos_temp,"timestamp"] <- data_temp$timestamp[pos]
      data_pre_temp[pos_temp,"time_diff"] <- data_temp$time_diff[pos]
      data_pre_temp[pos_temp,"timestamp_end_diff"] <- data_temp$timestamp_end_diff[pos]
      data_pre_temp[pos_temp,"variable"] <- variable_name[variable_pos]
      
      if (variable_pos ==4 || variable_pos==5 || variable_pos==12 || variable_pos==13) {
        char_temp <- (as.character(data_temp[pos,variable_name[variable_pos]]))
      } else if(variable_pos == 3) {
        #as.character(db_input[rows,"value"])
        #print(db_input[rows,"value"])
        #temp_Value <- as.character((db_input[rows,"value"]))
        #print(temp_Value)
        #db_input[rows,"value"] <- temp_Value
        char_temp <- (as.character(data_temp[pos,variable_name[variable_pos]]))
      }
      else{
        char_temp <-as.numeric(as.character(data_temp[pos,variable_name[variable_pos]]))-3
        if (variable_pos ==6 ||  variable_pos==11){
          if (char_temp==2){
            warning(as.character(data_temp$device_id[pos]))
          }
        }
        if (variable_pos==8){

          if (char_temp==-2){
            warning(as.character(data_temp$device_id[pos]))
          }        
        }
        char_temp <-as.character(char_temp)
      }
      data_pre_temp[pos_temp,"value"] <- char_temp
    }
  }
  db_input <- as.data.frame(rbind(db_input, data_pre_temp))
  
  
  db_input <- dplyr::arrange(db_input, timestamp)
  db_input <- db_input[order(db_input[,"id"],db_input[,"timestamp_end"]),]
  rownames(db_input) <- seq(length=nrow(db_input))
  
  return(db_input)
}

#db_temp <- sosci_import_function(db)

