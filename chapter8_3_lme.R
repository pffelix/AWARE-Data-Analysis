############################################################################################################
#load the nlme package
library(nlme)
library(plyr)
library(data.table)
library(psych)
library(ggplot2)
library(cowplot)
setwd("C:/Users/Felix/Dropbox/Exchange/Universitaet/WS 16_17 KW/BA/Ausarbeitung/7. Thesis/Plots")
library(lme4)
#dt_max <- 0*60
#dt_min <- 60*60
# im moment nur alle interval=2 stunden werte genommen (init ändern), nur für 2 wochen, is na entfern - korrekt?
############################################################################################################
#Integrate data from AWARE database:
variable_names <- c("dep", "indep", "third")
AWARE <- db
#AWARE <- subset(AWARE, timestamp_end_diff < 60*60*24*7)
# omit ids: less then 9 answers at boredom - balanced (change)
omit_id <- c()#c(56,42,39,6) 

# modify arousal of init database
# Stressed Setting
AWARE$arousal <- as.integer(mapvalues(as.character(AWARE$arousal), c("0","1","2","3","4",NA), c(NA,NA,"0",NA,"1",NA)))
label_name <- c("balanced","stressed")

# Slightly under pressure Setting
#AWARE$arousal <- as.integer(mapvalues(as.character(AWARE$arousal), c("0","1","2","3","4",NA), c(NA,NA,"0","1",NA,NA)))
#label_name <- c("balanced","slightly\nunder\npressure")

# Little to do Setting
#AWARE$arousal <- as.integer(mapvalues(as.character(AWARE$arousal), c("0","1","2","3","4",NA), c(NA,"1","0",NA,NA,NA)))
#label_name <- c("balanced","little to do")

# Bored Setting
#AWARE$arousal <- as.integer(mapvalues(as.character(AWARE$arousal), c("0","1","2","3","4",NA), c("1",NA,"0",NA,NA,NA)))
#label_name <- c("balanced","bored")

# Extended Stressed Setting
#AWARE$arousal <- as.integer(mapvalues(as.character(AWARE$arousal), c("0","1","2","3","4",NA), c(NA,NA,"0","1","1",NA)))

# Extended Bored Setting
#AWARE$arousal <- as.integer(mapvalues(as.character(AWARE$arousal), c("0","1","2","3","4",NA), c("1","1","0",NA,NA,NA)))


# arousal_0 <- 2
# arousal_1 <- 4

# calculate new variables with database (change)
#third:################################################################################################
# gender
AWARE <- setDT(AWARE)[, gender := value[variable == "demographic_gender"], by = id]
# boredom proneness
AWARE <- setDT(AWARE)[, bored_pro := as.numeric(as.character(value[variable == "personality_boredom"])), by = id]
# stress proneness
AWARE <- setDT(AWARE)[, stressed_pro := as.numeric(as.character(value[variable == "personality_stress"])), by = id]
# smartphone usage affection
AWARE <- setDT(AWARE)[, smartphone_usage_affection := as.numeric(as.character(value[variable == "smartphone_usage_affection"])), by = id]
# personality emotionsharing
AWARE <- setDT(AWARE)[, personality_emotionsharing := as.numeric(as.character(value[variable == "personality_emotionsharing"])), by = id]
# usage time low/high
AWARE <- setDT(AWARE)[, usage_time_lohi := sum(usage_time[variable == "esm_boredom_stress"]), by = id]
# usage frequency low/high
AWARE <- setDT(AWARE)[, usage_freq_lohi := sum(usage_freq[variable == "esm_boredom_stress"]), by = id]


# Personality conscientiousness
AWARE <- setDT(AWARE)[, personality_conscientiousness_1_r := value[variable == "personality_conscientiousness_1_r"], by = id]
AWARE <- setDT(AWARE)[, personality_conscientiousness_2 := value[variable == "personality_conscientiousness_2"], by = id]
AWARE[,"personality_conscientiousness"] <- (as.numeric(as.character(AWARE$personality_conscientiousness_1_r))*-1 +as.numeric(as.character(AWARE$personality_conscientiousness_2)))/2

# Personality neuroticism
AWARE <- setDT(AWARE)[, personality_neuroticism_2 := value[variable == "personality_neuroticism_2"], by = id]
AWARE <- setDT(AWARE)[, personality_neuroticism_1_r := value[variable == "personality_neuroticism_1_r"], by = id]
AWARE[,"personality_neuroticism"] <- (as.numeric(as.character(AWARE$personality_neuroticism_1_r))*-1 +as.numeric(as.character(AWARE$personality_neuroticism_2)))/2

# Personality extraversion
AWARE <- setDT(AWARE)[, personality_extraversion_1_r := value[variable == "personality_extraversion_1_r"], by = id]
AWARE <- setDT(AWARE)[, personality_extraversion_2 := value[variable == "personality_extraversion_2"], by = id]
AWARE[,"personality_extraversion"] <- (as.numeric(as.character(AWARE$personality_extraversion_1_r))*-1 +as.numeric(as.character(AWARE$personality_extraversion_2)))/2

# Personality agreeableness
AWARE <- setDT(AWARE)[, personality_agreeableness_1 := value[variable == "personality_agreeableness_1"], by = id]
AWARE <- setDT(AWARE)[, personality_agreeableness_2_r := value[variable == "personality_agreeableness_2_r"], by = id]
AWARE[,"personality_agreeableness"] <- (as.numeric(as.character(AWARE$personality_agreeableness_2_r))*-1 +as.numeric(as.character(AWARE$personality_agreeableness_1)))/2

# Personality openness
AWARE <- setDT(AWARE)[, personality_openness_1_r := value[variable == "personality_openness_1_r"], by = id]
AWARE <- setDT(AWARE)[, personality_openness_2 := value[variable == "personality_openness_2"], by = id]
AWARE[,"personality_openness"] <- (as.numeric(as.character(AWARE$personality_openness_1_r))*-1 +as.numeric(as.character(AWARE$personality_openness_2)))/2

# start preprocess (no change)
AWARE <- subset(AWARE,variable == "esm_boredom_stress" & (arousal == 0 | arousal == 1) & !(id %in% omit_id))
#describeBy(subset(AWARE, select=c("id")), group="id", mat=TRUE)
AWAREsub <- AWARE
AWAREsub[,"id_old"] <- AWAREsub$id

# number_0_1 <- c()
# for (i in 1:50){
#   number_0_1 <- c(number_0_1,describeBy(process$third,process$id)[[i]]$mean)
# }
# describeBy(number_0_1,number_0_1)

# configure variables from exiting variables (change)
#independent:##############################################################################################
AWAREsub$arousal_0_1 <- as.integer(ifelse(AWAREsub$arousal==0,0,1))

#dependent:################################################################################################
# usage_time 0:10 interval
AWAREsub$usage_time_0_10 <- AWAREsub$usage_time/max(AWAREsub$usage_time)*10
# usage_time metric
AWAREsub$usage_time <- AWAREsub$usage_time/dt/60
# usage_freq 0:10 interval
AWAREsub$usage_freq_0_10 <- AWAREsub$usage_freq/max(AWAREsub$usage_freq)*10
# usage_freq metric
AWAREsub$usage_freq <- AWAREsub$usage_time

#third:################################################################################################
# gender
AWAREsub$gender_female0_male1 <- as.integer(ifelse(as.character(AWAREsub$gender)=="Female",0,1))
# week
AWAREsub$week_0_1 <- as.integer(ifelse(AWAREsub$timestamp_end_diff < 60*60*24*7,0,1))
# boredom proneness
AWAREsub$bored_pro_0_1 <- as.integer(mapvalues(as.character(AWAREsub$bored_pro), c("-2","-1","0","1","2",NA), c("0","0",NA,"1","1",NA))) 
# boredom proneness non extended
AWAREsub$bored_pro_0_1_non_extended <- as.integer(mapvalues(as.character(AWAREsub$bored_pro), c("-2","-1","0","1","2",NA), c("0","0",NA,NA,"1",NA))) 
# stress proneness
AWAREsub$stressed_pro_0_1 <- as.integer(mapvalues(as.character(AWAREsub$stressed_pro), c("-2","-1","0","1","2",NA), c("0","0",NA,"1","1",NA))) 
# stress proneness non extended
AWAREsub$stressed_pro_0_1_non_extended <- as.integer(mapvalues(as.character(AWAREsub$stressed_pro), c("-2","-1","0","1","2",NA), c("0","0",NA,NA,"1",NA))) 
# smartphone usage affection
AWAREsub$smartphone_usage_affection_0_1 <- as.integer(mapvalues(as.character(AWAREsub$smartphone_usage_affection), c("-2","-1","0","1","2",NA), c(NA,NA,"0",NA,"1",NA))) 
# personality emotionsharing
AWAREsub$personality_emotionsharing_0_1 <- as.integer(mapvalues(as.character(AWAREsub$personality_emotionsharing), c("-2","-1","0","1","2",NA), c("0","0",NA,NA,"1",NA))) 
# Personality conscientiousness
AWAREsub$personality_conscientiousness_0_1 <- as.integer(mapvalues(as.character(AWAREsub$personality_conscientiousness), c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2",NA), c("0","0","0",NA,NA,NA,"1","1","1",NA)))
# Personality neuroticism
AWAREsub$personality_neuroticism_0_1 <- as.integer(mapvalues(as.character(AWAREsub$personality_neuroticism), c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2",NA), c("0","0","0",NA,NA,NA,"1","1","1",NA)))
# Personality extraversion
AWAREsub$personality_extraversion_0_1 <- as.integer(mapvalues(as.character(AWAREsub$personality_extraversion), c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2",NA), c("0","0","0",NA,NA,NA,"1","1","1",NA)))
# Personality agreeableness
AWAREsub$personality_agreeableness_0_1 <- as.integer(mapvalues(as.character(AWAREsub$personality_agreeableness), c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2",NA), c("0","0","0",NA,NA,NA,"1","1","1",NA)))
# Personality openness
AWAREsub$personality_openness_0_1 <- as.integer(mapvalues(as.character(AWAREsub$personality_openness), c("-2","-1.5","-1","-0.5","0","0.5","1","1.5","2",NA), c("0","0","0",NA,NA,NA,"1","1","1",NA)))
# Personality window
#window_0_1
# usage time low/high
AWAREsub$usage_time_lohi_0_1 <-  as.integer(ifelse(AWAREsub$usage_time_lohi<=median(unique(AWAREsub$usage_time_lohi),na.rm=TRUE),0,1))
# usage frequency low/high
AWAREsub$usage_freq_lohi_0_1 <-  as.integer(ifelse(AWAREsub$usage_freq_lohi<=median(unique(AWAREsub$usage_freq_lohi),na.rm=TRUE),0,1))



# set variables:################################################################################################
AWAREsub$indep <- AWAREsub$arousal_0_1
AWAREsub$dep <- AWAREsub$usage_time # usage_time
AWAREsub$third <- AWAREsub$usage_time_lohi_0_1

# set max number of measurement points
#nr_mea <- 60 #27 # automatic
nr_mea <- max(AWAREsub$interval) # automatic
# set max number of devices
#nr_par <- 57 #66 
nr_par <- length(unique(AWAREsub$id)) # automatic

# delete entries if participant answered more than once in interval
AWAREsub <- ddply(AWAREsub, c("id","third"), function(x) x[!duplicated(x$interval),])
# delete last devices if too much measurement points (no change)
AWAREsub <- transform(AWAREsub, id=match(id, unique(id)))
AWAREsub <- subset(AWAREsub, id <= nr_par)
# delete last measurement points if too much measurement points (no change)
AWAREsub <- ddply(AWAREsub, "id", function(x) head(x[order(x$timestamp_end_diff) , ],n=nr_mea))
# append NAs if not enough measurement points  (no change)
AWAREsub <- as.data.table(AWAREsub)[, lapply(.SD, `length<-`, nr_mea), by = id]
# extend to size nr_par*nr_mea  (no change)
AWAREsub <- as.data.table(AWAREsub)[, lapply(.SD, `length<-`, nr_mea*nr_par)]
# make new sequency of ids (no change)
AWAREsub[,"id"] <- rep(seq.int(from=1, to= nr_par, by=1), each = nr_mea)
# give NAs a intervall number night used yet by real measurement points
#AWAREsub <- ddply(AWAREsub, "id", function(x) for(i in nrow(x)){if(is.na(x$interval[i])){x$interval[i] <- seq[-x$interval[!is.na(x$interval)]][1]}})
interval_pos <- c()
seq <- rep(seq.int(from=1, to= nr_mea, by=1))
for (i in 1:nr_par){
  x <- subset(AWAREsub, id == i)
  temp1 <- x$interval[!is.na(x$interval)]
  temp2 <- seq[-x$interval[!is.na(x$interval)]]
  interval_pos <- c(interval_pos, x$interval[!is.na(x$interval)], seq[-x$interval[!is.na(x$interval)]])
}
AWAREsub$interval_new <- interval_pos -1
AWAREsub <- AWAREsub[order(id,interval_new),]

process <- data.frame(id = integer(nr_par*nr_mea), time = integer(nr_par*nr_mea), time7c = numeric(nr_par*nr_mea), dep = numeric(nr_par*nr_mea), indep = numeric(nr_par*nr_mea), indepc = numeric(nr_par*nr_mea), indepcb = numeric(nr_par*nr_mea), indepcw = numeric(nr_par*nr_mea), third = integer(nr_par*nr_mea))
process[,"id"] <- AWAREsub$id
process[,"time"] <- AWAREsub$interval_new
process[,"time7c"] <- (process$time/(nr_mea/2)-1)*7+7 # centered around 0 (day 1), 14 (day 14)
process[,"dep"] <- AWAREsub$dep
process[,"indep"] <- AWAREsub$indep
#process[is.na(process)] <- 0
process[,"indepc"] <- process$indep-mean(process$indep, na.rm=TRUE) # not used later
process[,"indepcw"] <- process$indep-aggregate(process$indep, list(process$id), mean, na.rm=TRUE)[rep(seq_len(nrow(aggregate(process$indep, list(process$id), mean, na.rm=TRUE))), each=nr_mea),2]
process[,"indepcb"] <- process$indepc - process$indepcw
process[,"third"] <- AWAREsub$third #rep((sample.int(2,nr_par,replace=TRUE)-1), each = nr_mea)

############################################################################################################
#Read a csv file containing the data
#process <- read.csv('C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/ch5R/process.csv')
colnames(process)[4] <- "dep"
colnames(process)[5] <- "indep"
colnames(process)[6] <- "indepc"
colnames(process)[7] <- "indepcb"
colnames(process)[8] <- "indepcw"
colnames(process)[9] <- "third"
process_pre <- process
############################################################################################################
#edit because of error
graphics.off()
############################################################################################################
#Graphic parameters

id_dep_0 <- unique(process$id[process$third==0])[!is.na(unique(process$id[process$third==0]))]
id_dep_1 <- unique(process$id[process$third==0])[!is.na(unique(process$id[process$third==1]))]
# test if it is working: ok
process <- process[!is.na(process$indep),] # if third much smaller N
#########################################################################################################

############################################################################################################
#Run linear growth model with AR(1) errors 
#tryCatch(cpmodel <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
#tryCatch(cpmodel <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
cpmodel <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~indepcw | id,na.action=na.omit,correlation = corAR1(),control = lmeControl(msMaxIter = 200, msMaxEval = 500, msVerbose = TRUE, sing.tol=1e-20))
#cpmodel <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit,control = lmeControl(msMaxIter = 200, msMaxEval = 500, sing.tol=1e-20))
summary_save <- summary(cpmodel)

############################################################################################################
#cpmodel <- lmer(dep ~ time7c + indepcw*third + indepcb*third +(indepcw|id) , data=process)
#summary_save <- summary(cpmodel)
#Put the EBLUPs of the random effects into a separate dataet
cfs<-ranef(cpmodel)
cfs$id<-1:nrow(cfs) #Add in id numbers
#Fix the names of the EBLUPs
names(cfs) <- make.names(names(cfs))
names(cfs)[c(2,1)] <- c("ebslope","ebintercept")
#create a upper-level data frame with third and add to data frame with EBLUPs
cfs$third<-aggregate(third ~ id, data=process, mean)[, 2]
#Add the fixed effects to the EBLUPs
fix_Intercept <- as.numeric(summary_save[[4]]$fixed["(Intercept)"])
fix_third <- as.numeric(summary_save[[4]]$fixed["third"])
fix_indepcw <- as.numeric(summary_save[[4]]$fixed["indepcw"])
fix_indepcw_third <- as.numeric(summary_save[[4]]$fixed["indepcw:third"])
cfs$intercept<- (fix_Intercept + fix_third*cfs$third + cfs$ebintercept) 
cfs$fixinter<- (fix_Intercept + fix_third*cfs$third) #not used below
cfs$slope<- (fix_indepcw + fix_indepcw_third*cfs$third + cfs$ebslope)
cfs$fixslope<- (fix_indepcw + fix_indepcw_third*cfs$third) #not used below
############################################################################################################
process_temp <- process
#Merge upper-level variables with the process data frame
process <- merge(process, cfs, all=TRUE, by="id")
# added because of error if process.third(x) == cfs.third(y)  -> third
if(all(c(process$third.x==as.integer(process$third.y) & sum(process$third.x, na.rm = TRUE)== sum(process$third.y, na.rm = TRUE)))){
  warning("error 1")
  process[,"third"] <- process[,"third.x"]
  drops <- c("third.x","third.y")
  process <- process[ , !(names(process) %in% drops)]
} else {
  # added because of further error if process.third(x) != cfs.third(y)  -> third(x)
  warning("error 2")
  process[,"third"] <- process[,"third.x"]
  drops <- c("third.x","third.y")
  process <- process[ , !(names(process) %in% drops)]
}


#Create predicted values based on within-subject causal model (adjusting for time
#and removing between-subjects variation)
process$pred<-(process$intercept + process$slope*process$indep)

############################################################################################################

#To create graphs based on observed data only: Sort the dataset by relationship quality, ID, and daily conflict
ordprocess<-process[order(process$third, process$slope, process$id, process$indep),]
#Sort the upper-level data frame by relationship quality, conflict slope and ID
ordcfs<-cfs[order(cfs$third, cfs$slope, cfs$id),]

############################################################################################################


############################################################################################################

#Spaghetti Plots 

#edit because of error
#pdf(file="spag.pdf", width=14, height=10)
# par(mar=c(1,1,1,1))
# 
# par(mfcol=c(1,2))
# par(lwd=.5)

# #low third
# plot(process$indep[process$third==0], process$dep[process$third==0], 
#      ylab="dep", xlab="indep", type="n", ylim=c(0,max(process$dep[process$third==0], na.rm=TRUE)), main="Low Relationship Quality")
# for (i in cfs$id[cfs$third==0]){
#   lines(ordprocess$indep[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
# }
# predl<-fix_Intercept + fix_indepcw*process$indep[process$third==0] #fixed line for low group
# lines(process$indep[process$third==0], predl, col="Red", lwd=4)
# 
# high third
# plot(process$indep[process$third==1], process$dep[process$third==1], 
#      ylab="dep", xlab="time", type="n", pch=4, ylim=c(0,max(process$dep[process$third==1], na.rm=TRUE)), main="High Relationship Quality")
# for (i in cfs$id[cfs$third==1]){
#   lines(ordprocess$indep[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
# }
# predh<-(fix_Intercept + fix_third) + (fix_indepcw + fix_indepcw_third)*process$indep[process$third==1]  #fixed line for high group
# lines(process$indep[process$third==1], predh, col="Red", lwd=4)
# #dev.off()

# high smartphone affection group : third=1 
id_vec <- c()
x_vec <- c()
y_vec <- c()
for (i in cfs$id[cfs$third==1]){
  x_vec <-c(x_vec,ordprocess$indep[ordprocess$id==i])
  y_vec <-c(y_vec,ordprocess$pred[ordprocess$id==i])
  id_vec <- c(id_vec,rep(i,length(ordprocess$indep[ordprocess$id==i])))
}
spag_data_high <- data.frame(Participant=id_vec,x_v=x_vec,y_v=y_vec)
predh_high<-(fix_Intercept + fix_third) + (fix_indepcw + fix_indepcw_third)*process$indep[process$third==1]  
spag_data_pre_high <- data.frame(Participant=51,x_v=process$indep[process$third==1],y_v=predh_high)
y_max <-ceiling(max(spag_data_high$y_v))+1

# low smartphone affection group : third=0
id_vec <- c()
x_vec <- c()
y_vec <- c()
for (i in cfs$id[cfs$third==0]){
  x_vec <-c(x_vec,ordprocess$indep[ordprocess$id==i])
  y_vec <-c(y_vec,ordprocess$pred[ordprocess$id==i])
  id_vec <- c(id_vec,rep(i,length(ordprocess$indep[ordprocess$id==i])))
}
predl_low <-fix_Intercept + fix_indepcw*process$indep[process$third==0]
spag_data_low <- data.frame(Participant=id_vec,x_v=x_vec,y_v=y_vec)
spag_data_pre_low <- data.frame(Participant=51,x_v=process$indep[process$third==0],y_v=predl_low)


p1 <- ggplot() +
  geom_line(data=spag_data_low,aes(x=x_v, y=y_v, group = Participant, colour = Participant),size=0.5)+
  geom_line(data=spag_data_pre_low, aes(x=x_v, y=y_v,size="solid"),colour="red")+
  #theme_grey()+
  #theme(axis.title.x = element_blank())+
  #axis.title.y = element_text(face="bold",angle=90))+  #coord_trans(y="log10", limy=c(1000,6000)) +
  labs(list(title = "AUC", y = paste("Predicted smartphone usage time in min/h"))) + 
  #geom_line(size=1) + theme(legend.position="none")+
  ggtitle("Group: Low smartphone affection")+
  xlab("arousal state")+
  scale_x_continuous(,limits=c(0,1), breaks = c(0,1),labels=label_name) +
  scale_y_continuous(expand=c(0,0), limits=c(0,y_max), breaks = seq(-20,40,1)) +
  theme(plot.title = element_text(hjust = +0.5))+ 
  scale_size_manual(name = 'Average participant', breaks=c("solid"),values =2, labels = c(''))+
  #labs(linetype = "solid",Position)
  #theme(legend.position=c(1,0))+
  labs(colour = "Participant", shape = "                  Cut") +
  theme(legend.position="none")
#guides(linetype = guide_legend(order = 1))
print(p1)



p2 <- ggplot() +
  geom_line(data=spag_data_high,aes(x=x_v, y=y_v, group = Participant, colour = Participant),size=0.5)+
  geom_line(data=spag_data_pre_high, aes(x=x_v, y=y_v,size="solid"),colour="red")+
  #theme_grey()+
  #theme(panel.grid.minor = element_line(colour="grey", size=0.5)) + 
  #theme(axis.title.x = element_blank())+
  #axis.title.y = element_text(face="bold",angle=90))+  #coord_trans(y="log10", limy=c(1000,6000)) +
  labs(list(title = "AUC", y = paste("Predicted smartphone usage time in min/h"))) + 
  #geom_line(size=1) + theme(legend.position="none")+
  ggtitle("Group: High smartphone affection")+
  xlab("arousal state")+
  scale_x_continuous(,limits=c(0,1), breaks = c(0,1),minor_breaks = seq(0, 1, 1),labels=label_name) +
  scale_y_continuous(expand=c(0,0), limits=c(0,y_max), breaks = seq(-20,40,1)) +
  theme(plot.title = element_text(hjust = +0.5))+ 
  scale_size_manual(name = 'Average participant', breaks=c("solid"),values =2, labels = c(''))+
  #labs(linetype = "solid",Position)
  #theme(legend.position=c(1,0))+
  labs(colour = "Participant", shape = "                  Cut")
#guides(linetype = guide_legend(order = 1))

print(p2)





boxplot_grid <- plot_grid(p1,p2, ncol=2, labels = c(),hjust=-21,rel_widths=c(0.79,1.1))
save_plot(file=paste0("8.4_spaghetti_",label_name[1],"_",gsub("\n"," ",label_name[2]),"_dt_min_",dt_min,".emf"),plot=boxplot_grid, base_width=12, base_height=4)

####################################
summary(cpmodel)
summary_save <- summary(cpmodel)

#summary(cpmodel_lmer)
#rand(cpmodel)
#confint(cpmodel_lmer, oldNames=FALSE, level = 0.95)
#temp <- difflsmeans(cpmodel_lmer)
#step(cpmodel)
# # likelihood ratio tests
# #fixed effects
# anova(cpmodel)
# # random effects
# tryCatch(cpmodel_nr <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~1 | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
# cpmodel_nr <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~1 | id, correlation = corAR1(),na.action=na.omit,control = lmeControl(msMaxIter = 200, msMaxEval = 500, msVerbose = TRUE, sing.tol=1e-20))})
# #summary(cpmodel_nr)
# #summary_save_nr <- summary(cpmodel_nr)
# anova(cpmodel, cpmodel_nr)
# 

# p_values <- data.frame(test=c(""))
# #Test time7c
# tryCatch(cpmodel_nr <- lme(fixed=dep ~ indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
# cpmodel_nr <- lme(fixed=dep ~ indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit,control = lmeControl(msMaxIter = 200, msMaxEval = 500, msVerbose = TRUE, sing.tol=1e-20))})
# p_values[,"time7c"] <- anova(cpmodel_nr,cpmodel)[2,"p-value"]
# 
# #Test indepcw*third
# tryCatch(cpmodel_nr <- lme(fixed=dep ~ time7c  + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
# cpmodel_nr <- lme(fixed=dep ~ time7c + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit,control = lmeControl(msMaxIter = 200, msMaxEval = 500, msVerbose = TRUE, sing.tol=1e-20))})
# p_values[,"indepcw*third"] <- anova(cpmodel_nr,cpmodel)[2,"p-value"]
# 
# #Test indepcb*third
# tryCatch(cpmodel_nr <- lme(fixed=dep ~ time7c + indepcw*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
# cpmodel_nr <- lme(fixed=dep ~ time7c + indepcw*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit,control = lmeControl(msMaxIter = 200, msMaxEval = 500, msVerbose = TRUE, sing.tol=1e-20))})
# p_values[,"indepcb*third"] <- anova(cpmodel_nr,cpmodel)[2,"p-value"]
# 
# p_values
# #Test third
# tryCatch(cpmodel_nr <- lme(fixed=dep ~ time7c, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
# cpmodel_nr <- lme(fixed=dep ~ time7c, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit,control = lmeControl(msMaxIter = 200, msMaxEval = 500, msVerbose = TRUE, sing.tol=1e-20))})
# 
# #Test indepcw*third
# tryCatch(cpmodel_nr <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
# cpmodel_nr <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit,control = lmeControl(msMaxIter = 200, msMaxEval = 500, msVerbose = TRUE, sing.tol=1e-20))})
# 
# #Test indepcw*third
# tryCatch(cpmodel_nr <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit), error=function(e){ warning= warning("!!!!!!!!!!convergence limit reached!!!!!!!!!!!!!")
# cpmodel_nr <- lme(fixed=dep ~ time7c + indepcw*third + indepcb*third, data=process, random=~indepcw | id, correlation = corAR1(),na.action=na.omit,control = lmeControl(msMaxIter = 200, msMaxEval = 500, msVerbose = TRUE, sing.tol=1e-20))})
# 

number_0_1 <- c()
for (i in 1:length(unique(process$id))){
  number_0_1 <- c(number_0_1,describeBy(process$third,process$id)[[i]]$mean)
}
describeBy(number_0_1,number_0_1)

