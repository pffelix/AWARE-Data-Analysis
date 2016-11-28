# for bored arousal
sub <- subset(db, variable == "esm_boredom_stress"& arousal == c(0,1,2))
v_usage_freq <- ave(sub$usage_freq, factor(sub$id), FUN=function(x) c(NA,diff(x)))
v_usage_time <- ave(sub$usage_time, factor(sub$id), FUN=function(x) c(NA,diff(x)))
v_arousal <- ave(sub$arousal, factor(sub$id), FUN=function(x) c(NA,diff(x)))
sub_ag <- data.frame("id"=sub$id,"arousal"=v_arousal,"usage_time"=v_usage_time,"usage_freq"=v_usage_freq)

sub_ag <- sub_ag[!is.na(sub_ag$arousal),]
for (z in 1:nrow(sub_ag)){
  if((sub_ag$arousal[z] <0 & sub_ag$usage_time[z] >0) || (sub_ag$arousal[z] >=0 & sub_ag$usage_time[z] <=0) || is.na(sub_ag$arousal[z]) || is.na(sub_ag$usage_time[z])){
    if((is.na(sub_ag$arousal[z]) || is.na(sub_ag$usage_time[z]))){
      
    }else if((sub_ag$arousal[z] <0 & sub_ag$usage_time[z] >0)){
    sub_ag[z,"Check_time"] <- TRUE
    }else if((sub_ag$arousal[z] >0 & sub_ag$usage_time[z] <0)){
    sub_ag[z,"Check_time"] <- TRUE
    }else if(sub_ag$arousal[z] ==0 & sub_ag$usage_time[z] ==0){
      sub_ag[z,"Check_time"] <- TRUE
    }else{
    sub_ag[z,"Check_time"] <- FALSE
    }
  }
}
for (z in 1:nrow(sub_ag)){
  if (is.na(sub_ag$arousal[z]) || is.na(sub_ag$usage_freq[z])){
  }else if(sub_ag$arousal[z] <0 & sub_ag$usage_freq[z] >0){
    sub_ag[z,"Check_freq"] <- TRUE
  }else if(sub_ag$arousal[z] >0 & sub_ag$usage_freq[z] <0){
    sub_ag[z,"Check_freq"] <- TRUE
  }else if(sub_ag$arousal[z] ==0 & sub_ag$usage_freq[z] ==0){
    sub_ag[z,"Check_freq"] <- TRUE
  }else{
    sub_ag[z,"Check_freq"] <- FALSE
  }
}
describeBy(sub_ag$Check_time,sub_ag$Check_time)
describeBy(sub_ag$Check_freq,sub_ag$Check_freq)

# for stressed arousal
sub <- subset(db, variable == "esm_boredom_stress"& arousal == c(2,3,4))
v_usage_freq <- ave(sub$usage_freq, factor(sub$id), FUN=function(x) c(NA,diff(x)))
v_usage_time <- ave(sub$usage_time, factor(sub$id), FUN=function(x) c(NA,diff(x)))
v_arousal <- ave(sub$arousal, factor(sub$id), FUN=function(x) c(NA,diff(x)))
sub_ag <- data.frame("id"=sub$id,"arousal"=v_arousal,"usage_time"=v_usage_time,"usage_freq"=v_usage_freq)

sub_ag <- sub_ag[!is.na(sub_ag$arousal),]
for (z in 1:nrow(sub_ag)){
  if (is.na(sub_ag$arousal[z]) || is.na(sub_ag$usage_time[z])){
  }else if(sub_ag$arousal[z] >0 & sub_ag$usage_time[z] >0){
    sub_ag[z,"Check_time"] <- TRUE
  }else if(sub_ag$arousal[z] <0 & sub_ag$usage_time[z] <0){
    sub_ag[z,"Check_time"] <- TRUE
  }else if(sub_ag$arousal[z] ==0 & sub_ag$usage_time[z] ==0){
    sub_ag[z,"Check_time"] <- TRUE
  }else{
    sub_ag[z,"Check_time"] <- FALSE
  }
}
for (z in 1:nrow(sub_ag)){
  if (is.na(sub_ag$arousal[z]) || is.na(sub_ag$usage_freq[z])){
  }else if(sub_ag$arousal[z] >0 & sub_ag$usage_freq[z] >0){
    sub_ag[z,"Check_freq"] <- TRUE
  }else if(sub_ag$arousal[z] <0 & sub_ag$usage_freq[z] <0){
    sub_ag[z,"Check_freq"] <- TRUE
  }else if(sub_ag$arousal[z] ==0 & sub_ag$usage_freq[z] ==0){
    sub_ag[z,"Check_freq"] <- TRUE
  }else{
    sub_ag[z,"Check_freq"] <- FALSE
  }
}
describeBy(sub_ag$Check_time,sub_ag$Check_time)
describeBy(sub_ag$Check_freq,sub_ag$Check_freq)