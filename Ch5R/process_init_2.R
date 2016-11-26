############################################################################################################
#load the nlme package
library(nlme)
library(plyr)
library(data.table)
############################################################################################################
#Integrate data from AWARE database:
nr_par <- 66
nr_mea <- 28
AWARE <- subset(db,variable == "esm_boredom_stress" & (arousal == 0 | arousal == 2) & id != 10)
Alme_sub <- AWARE

# delete last devices if too much measurement points
Alme_sub[,"id_old"] <- Alme_sub$id
Alme_sub <- transform(Alme_sub, id=match(id, unique(id)))
Alme_sub <- subset(Alme_sub, id <= nr_par)
# delete last measurement points if too much measurement points
Alme_sub <- ddply(Alme_sub, "id", function(x) head(x[order(x$timestamp_end_diff) , ],n=nr_mea))
# calculate 1:10 intervall
Alme_sub$usage_time <- Alme_sub$usage_time/60/(max(Alme_sub$usage_time/60)-min(Alme_sub$usage_time/60))
# append NAs if not enough measurement points
Alme_sub <- as.data.table(Alme_sub)[, lapply(.SD, `length<-`, nr_mea), by = id]
# extend to size nr_par*nr_mea
Alme_sub <- as.data.table(Alme_sub)[, lapply(.SD, `length<-`, nr_mea*nr_par)]
# change ids to fit the Alme id 
Alme_sub[,"id"] <- rep(seq.int(from=1, to= nr_par, by=1), each = nr_mea)


Alme <- data.frame(id = integer(nr_par*nr_mea), time = integer(nr_par*nr_mea), time7c = numeric(nr_par*nr_mea), intimacy = numeric(nr_par*nr_mea), conflict = numeric(nr_par*nr_mea), confc = numeric(nr_par*nr_mea), confcb = numeric(nr_par*nr_mea), confcw = numeric(nr_par*nr_mea), relqual = integer(nr_par*nr_mea))
Alme[,"id"] <- Alme_sub$id
Alme[,"time"] <- rep(seq.int(from=0, to= nr_mea-1, by=1), nr_par)
Alme[,"time7c"] <- (Alme[,"time"]-(nr_mea-1)/2)/7
Alme[,"intimacy"] <- Alme_sub$usage_time
Alme[,"conflict"] <- Alme_sub$arousal/2
#Alme[is.na(Alme)] <- 0
Alme[,"confc"] <- Alme$conflict-mean(Alme$conflict, na.rm=TRUE)
Alme[,"confcw"] <- Alme$conflict-aggregate(Alme$conflict, list(Alme$id), mean, na.rm=TRUE)[rep(seq_len(nrow(aggregate(Alme$conflict, list(Alme$id), mean, na.rm=TRUE))), each=nr_mea),2]
Alme[,"confcb"] <- Alme[,"confc"] - Alme[,"confcw"]
Alme[,"relqual"] <- rep((sample.int(2,nr_par,replace=TRUE)-1), each = nr_mea)
process <- Alme

############################################################################################################
#Read a csv file containing the data
#process <- read.csv('C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/ch5R/process.csv')
############################################################################################################
#edit because of error
graphics.off()
############################################################################################################

#Figure 5.2: Time Course Plots for Intimacy for the Low Relationship Quality Group

#edit because of error
#pdf(file="lrq-intimacy-time.pdf", width=15, height=8)
par(mar=c(1,1,1,1))

par(mfrow=c(4,8))
for (i in process$id[process$time==0 & process$relqual==0]){
  print(i)
  plot(process$time[process$id==i], process$intimacy[process$id==i], 
       ylab="Intimacy", xlab="Time", type="l", xlim=c(-1, 29), ylim=c(0,8), main=paste("id =", i, sep = " "))
}
mtext("Low Relationship Quality", side=3, outer=TRUE, line=-1.2)
#dev.off()

#Figure 5.2: Time Course Plots for Intimacy for the High Relationship Quality Group

#edit because of error
#pdf(file="hrq-intimacy-time.pdf", width=15, height=10)
par(mar=c(1,1,1,1))

par(mfrow=c(5,8))
for (i in process$id[process$time==0 & process$relqual==1]){
  plot(process$time[process$id==i], process$intimacy[process$id==i], 
       ylab="Intimacy", xlab="Time", type="l", xlim=c(-1, 29), ylim=c(0,8), main=paste("id =", i, sep = " "))
}
mtext("High Relationship Quality", side=3, outer=TRUE, line=-1.2)
#dev.off()
############################################################################################################

############################################################################################################

#Figure 5.2: Time Course Plots for Conflict for the Low Relationship Quality Group

#edit because of error
#pdf(file="lrq-conflict-time.pdf", width=15, height=8)
par(mar=c(1,1,1,1))

par(mfrow=c(4,8))
for (i in process$id[process$time==0 & process$relqual==0]){
  plot(process$time[process$id==i], process$conflict[process$id==i], 
       ylab="Conflict", xlab="Time", type="l", xlim=c(-1, 29), ylim=c(-0.1, 1.1), main=paste("id =", i, sep = " "))
}
mtext("Low Relationship Quality", side=3, outer=TRUE, line=-1.2)
#dev.off()

#Figure 5.2: Time Course Plots for Conflict for the High Relationship Quality Group

#edit because of error
#pdf(file="hrq-conflict-time.pdf", width=15, height=10)
par(mar=c(1,1,1,1))

par(mfrow=c(5,8))
for (i in process$id[process$time==0 & process$relqual==1]){
  plot(process$time[process$id==i], process$conflict[process$id==i], 
       ylab="Conflict", xlab="Time", type="l", xlim=c(-1, 29), ylim=c(-0.1, 1.1), main=paste("id =", i, sep = " "))
}
mtext("High Relationship Quality", side=3, outer=TRUE, line=-1.2)
#dev.off()
############################################################################################################


############################################################################################################
#Run linear growth model with AR(1) errors 
cpmodel <- lme(fixed=intimacy ~ time7c + confcw*relqual + confcb*relqual, data=process, random=~confcw | id, correlation = corAR1(),na.action=na.omit)#na.action=na.omit
summary(cpmodel)
summary_save <- summary(cpmodel)
############################################################################################################


############################################################################################################

#Put the EBLUPs of the random effects into a separate dataet
cfs<-ranef(cpmodel)
cfs$id<-1:nrow(cfs) #Add in id numbers
#Fix the names of the EBLUPs
names(cfs) <- make.names(names(cfs))
names(cfs)[c(2,1)] <- c("ebslope","ebintercept")
#create a upper-level data frame with relqual and add to data frame with EBLUPs
cfs$relqual<-aggregate(relqual ~ id, data=process, mean)[, 2]
#Add the fixed effects to the EBLUPs
fix_Intercept <- as.numeric(summary_save[[4]]$fixed["(Intercept)"])
fix_relqual <- as.numeric(summary_save[[4]]$fixed["relqual"])
fix_confcw <- as.numeric(summary_save[[4]]$fixed["confcw"])
fix_confcw_relqual <- as.numeric(summary_save[[4]]$fixed["confcw:relqual"])
cfs$intercept<- (fix_Intercept + fix_relqual*cfs$relqual + cfs$ebintercept) 
cfs$fixinter<- (fix_Intercept + fix_relqual*cfs$relqual) #not used below
cfs$slope<- (fix_confcw + fix_confcw_relqual*cfs$relqual + cfs$ebslope)
cfs$fixslope<- (fix_confcw + fix_confcw_relqual*cfs$relqual) #not used below
############################################################################################################
process_temp <- process
#Merge upper-level variables with the process data frame
process <- merge(process, cfs, all=TRUE, by="id")
# added because of error if process.relqual(x) == cfs.relqual(y)  -> relqual
if(all(c(process$relqual.x==process$relqual.y))){
  warning("error 1")
  process[,"relqual"] <- process[,"relqual.x"]
  drops <- c("relqual.x","relqual.y")
  process <- process[ , !(names(process) %in% drops)]
} else {
  # added because of further error if process.relqual(x) != cfs.relqual(y)  -> relqual(x)
  warning("error 2")
  process[,"relqual"] <- process[,"relqual.x"]
  drops <- c("relqual.x","relqual.y")
  process <- process[ , !(names(process) %in% drops)]
}


#Create predicted values based on within-subject causal model (adjusting for time
#and removing between-subjects variation)
process$pred<-(process$intercept + process$slope*process$conflict)

############################################################################################################

#To create graphs based on observed data only: Sort the dataset by relationship quality, ID, and daily conflict
ordprocess<-process[order(process$relqual, process$slope, process$id, process$conflict),]
#Sort the upper-level data frame by relationship quality, conflict slope and ID
ordcfs<-cfs[order(cfs$relqual, cfs$slope, cfs$id),]

############################################################################################################

#Figure 5.4: Intimacy as a Function of Conflict: Raw Data and Model Predictions for the Low Relationship Quality Group
#Note: Confidence Bounds for Predictions are not Available for lme() models

#edit because of error
#pdf(file="lrq-pred-panels.pdf", width=14, height=8)
par(mar=c(1,1,1,1))

par(mfrow=c(4,8))
for (i in c(48, 45, 58,  4, 43, 26,  8, 27, 52, 65,  7, 28, 56, 
            47, 17, 35, 62,  9, 22, 46, 18, 34, 30, 60, 41, 21)){   #Subjects in low rq group
  plot(process$conflict[process$id==i], process$intimacy[process$id==i], 
       ylab="intimacy", xlab="conflict (0,1)", type="p", pch=1, xlim=c(-.2, 1.2), ylim=c(0,10), 
       main=paste(round(cfs$slope[cfs$id==i], digits=3)))
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i]) 
}
mtext("Low Relationship Quality", side=3, outer=TRUE, line=-1.2)
#dev.off()


#Figure 5.4: Intimacy as a Function of Conflict: Raw Data and Model Predictions for the High Relationship Quality Group
#Note: Confidence Bounds for Predictions are not Available for lme() models

#edit because of error
#pdf(file="hrq-pred-panels.pdf", width=14, height=10)
par(mar=c(1,1,1,1))

par(mfrow=c(5,8))
for (i in c(38, 11, 59, 42, 63, 19, 54,  6, 49, 61,  5, 66, 16, 10, 36, 14, 39, 44, 51, 
     55, 64, 25, 57, 33, 20, 50, 37,  1, 53, 24,  2, 15, 32, 40, 23, 12, 13, 29,  3, 31)){ #Subjects in high rq group
  plot(process$conflict[process$id==i], process$intimacy[process$id==i], 
       ylab="intimacy", xlab="conflict (0,1)", type="p", pch=1, xlim=c(-.2, 1.2), ylim=c(0,10),
       main=paste(round(cfs$slope[cfs$id==i], digits=3)))
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
}
mtext("High Relationship Quality", side=3, outer=TRUE, line=-1.2)
#dev.off()
############################################################################################################


############################################################################################################

#Figure 5.5: Spaghetti Plots for Low and High Relationship Quality Groups

#edit because of error
#pdf(file="spag.pdf", width=14, height=10)
par(mar=c(1,1,1,1))

par(mfcol=c(1,2))
par(lwd=.5)
#low relqual
plot(process$conflict[process$relqual==0], process$intimacy[process$relqual==0], 
     ylab="Intimacy", xlab="Conflict", type="n", ylim=c(0,8), main="Low Relationship Quality")
for (i in cfs$id[cfs$relqual==0]){
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
}
predl<-4.532200 -2.010619*process$conflict[process$relqual==0] #fixed line for low group
lines(process$conflict[process$relqual==0], predl, col="Red", lwd=4)
#high relqual
plot(process$conflict[process$relqual==1], process$intimacy[process$relqual==1], 
     ylab="intimacy", xlab="time", type="n", pch=4, ylim=c(0,8), main="High Relationship Quality")
for (i in cfs$id[cfs$relqual==1]){
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
}
predh<-(4.532200 + 0.64726123) + (-2.010619 + 1.01641068)*process$conflict[process$relqual==1]  #fixed line for high group
lines(process$conflict[process$relqual==1], predh, col="Red", lwd=4)
#dev.off()

############################################################################################################



############################################################################################################
#Find percentiles for slope distribution for Low Relationship Quality Group

quantile(cfs$slope[cfs$relqual==0], c(0.0, .05, .25, .50, .75, .95, 1.0))

############################################################################################################



############################################################################################################

#Figure 2 of example write-up for Chapter 5: Panel plots for five selected IDs in Low RQ Group
#Note: In order to match the book, ID=48 is chosen for the 5th percentile, whereas the more correct
#choice would have been ID=45. Similarly ID=65 is chosen over ID=47 for the 50th percentile.

#edit because of error
#pdf(file="lrq-five.pdf", width=14, height=3)
par(mar=c(1,1,1,1))

# Error 2: change 65 to 47
par(mfcol=c(1,5))
for (i in c(48, 8, 47, 46, 41)){
  plot(ordprocess$conflict[ordprocess$id==i], ordprocess$intimacy[ordprocess$id==i], 
       ylab="Intimacy", xlab="Conflict", type="p", pch=1, ylim=c(0,8), main=paste("id =", i, sep = " "))
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
}
mtext("Low Relationship Quality", side=3, outer=TRUE, line=-1.2)
#dev.off()
############################################################################################################



############################################################################################################
#Find percentiles for slope distribution for High Relationship Quality Group

quantile(cfs$slope[cfs$relqual==1], c(0.0, .05, .25, .50, .75, .95, 1.0))

############################################################################################################


############################################################################################################

#Figure 2 of example write-up for Chapter 5: Panel plots for five selected IDs in High Relationship Quality Group
#Note: In order to match the book, ID=11 is chosen for the 5th percentile, whereas the more correct
#choice would have been ID=59. Similarly ID=14 is chosen over ID=64 for the 50th percentile. Finally, ID=50 is
#chosen over ID=24 for the 75th percentile.

#edit because of error
#pdf(file="hrq-five.pdf", width=14, height=3)
par(mar=c(1,1,1,1))
#windows(14,3)

par(mfcol=c(1,5))
for (i in c(11, 5, 14, 50, 29)){
  plot(ordprocess$conflict[ordprocess$id==i], ordprocess$intimacy[ordprocess$id==i], 
       ylab="Intimacy", xlab="Conflict", type="p", pch=1, ylim=c(0,8), main=paste("id =", i, sep = " "))
  lines(ordprocess$conflict[ordprocess$id==i], ordprocess$pred[ordprocess$id==i])
}
mtext("High Relationship Quality", side=3, outer=TRUE, line=-1.2)
#dev.off()
############################################################################################################


summary(cpmodel)

