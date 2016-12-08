## analyze chapter 8.1 - influence of measurement on smartphone usage pattern
setwd("C:/Users/Felix/Dropbox/Exchange/Universitaet/WS 16_17 KW/BA/Ausarbeitung/7. Thesis/Plots")
library(prettyR)
library(psych)
library(agricolae)
library(ggplot2)
library(cowplot)
library(clinfun)
library(pgirmess)
library(FSA)
library(car)
library('reshape2')
library(knitr)
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/tools.R")
setwd("C:/Users/Felix/Dropbox/Exchange/Universitaet/WS 16_17 KW/BA/Ausarbeitung/7. Thesis/Plots")
options(scipen = 10)

# dt_min <- 1*60*60
# dt_max <- 1*60*60
# source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/onoff_calc.R")
# source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/arousal_calc.R")

# variable definition
blu <- "#1F497D"
graphics.off()
color_arousal <- c(blu,'#43a2ca','#7bccc4','#bae4bc','#f0f9e8')
color_2 <- c('#ffffd4','#fed98e','#fe9929','#d95f0e','#993404')
#### Chapter Correlation boredom, smartphone usage
print("Chapter Evaluation Study method")
sub_state <- list()
group <- factor(c("Bored","Little to do","Balanced","Slightly under pressure","Stressed"), levels = c("Bored","Little to do","Balanced","Slightly under pressure","Stressed"))
group_N <- rep("",5)

### let's go
for (ar in 1:5){
  sub_state[[ar]] <- subset(dbe, variable == "esm_boredom_stress" & arousal == ar-1, select=c("arousal","time_diff","timestamp_end_diff","time_diff_mea","timestamp_end_diff_mea","time_diff_mea_plus","timestamp_end_diff_mea_plus","time_diff_mea_minus","timestamp_end_diff_mea_minus"))
}
state_info <- data.frame("N"=c(nrow(sub_state[[1]]),nrow(sub_state[[2]]),nrow(sub_state[[3]]),nrow(sub_state[[4]]),nrow(sub_state[[5]])))
for (ar in 1:5){
  sub_state[[ar]][,"Group"] <- as.factor(paste0(group[ar],"\n (N=",state_info$N[ar],")"))
  group_N[ar] <- as.character(paste0(group[ar],"\n (N=",state_info$N[ar],")"))
}
#sub_arousal <- subset(dbe, variable == "esm_boredom_stress", select = c("arousal","usage_time","usage_freq"))
state.data <- rbind(sub_state[[1]], sub_state[[2]], sub_state[[3]], sub_state[[4]],sub_state[[5]])

res_frame <- data.frame(a=rep(NA,6*5),b=rep(NA,6*5),c=rep(NA,6*5),d=rep(NA,6*5),e=rep(NA,6*5),f=rep(NA,6*5))
colnames(res_frame) <- c(as.character(group),"Kruskal")

# Notification response time
state.data$nrt <- state.data$time_diff_mea
tab <- 1
stat_result <- describeBy(state.data$nrt,state.data$Group)
print(stat_result)
by(state.data$nrt, state.data$Group, IQR)

for(i in 1:length(group)){
  res_frame[tab,as.character(group[i])] <- stat_result[[as.character(group_N[i])]]$median
}
kruskal_result <- kruskal.test(formula = nrt ~ Group, data = state.data)
print(kruskal_result)
res_frame[c(tab,tab+1),"Kruskal"] <- c(as.numeric(kruskal_result$statistic), kruskal_result$p.value)
dunn_result <- dunnTest(nrt ~ Group, data = state.data,method="bh") # or non
print(dunn_result)
res_frame[tab+2,c(1)] <- c(dunn_result$res$P.adj[1])
res_frame[tab+3,c(1,2)] <- c(dunn_result$res$P.adj[2],dunn_result$res$P.adj[3])
res_frame[tab+4,c(1,2,3)] <- c(dunn_result$res$P.adj[4],dunn_result$res$P.adj[5],dunn_result$res$P.adj[6])
res_frame[tab+5,c(1,2,3,4)] <- c(dunn_result$res$P.adj[7],dunn_result$res$P.adj[8],dunn_result$res$P.adj[9],dunn_result$res$P.adj[10])

# Usage time prior measurement
state.data$upm <- state.data$timestamp_end_diff-(state.data$timestamp_end_diff_mea-state.data$time_diff_mea)
tab <- 7
stat_result <- describeBy(state.data$upm,state.data$Group)
print(stat_result)
by(state.data$upm, state.data$Group, IQR)

for(i in 1:length(group)){
  res_frame[tab,as.character(group[i])] <- stat_result[[as.character(group_N[i])]]$median
}
kruskal_result <- kruskal.test(formula = upm ~ Group, data = state.data)
print(kruskal_result)
res_frame[c(tab,tab+1),"Kruskal"] <- c(as.numeric(kruskal_result$statistic), kruskal_result$p.value)
dunn_result <- dunnTest(upm ~ Group, data = state.data,method="bh") # or non
print(dunn_result)
res_frame[tab+2,c(1)] <- c(dunn_result$res$P.adj[1])
res_frame[tab+3,c(1,2)] <- c(dunn_result$res$P.adj[2],dunn_result$res$P.adj[3])
res_frame[tab+4,c(1,2,3)] <- c(dunn_result$res$P.adj[4],dunn_result$res$P.adj[5],dunn_result$res$P.adj[6])
res_frame[tab+5,c(1,2,3,4)] <- c(dunn_result$res$P.adj[7],dunn_result$res$P.adj[8],dunn_result$res$P.adj[9],dunn_result$res$P.adj[10])


# Usage time after measurement
state.data$uam <- (state.data$timestamp_end_diff_mea)-state.data$timestamp_end_diff
tab <- 13
stat_result <- describeBy(state.data$uam,state.data$Group)
print(stat_result)
by(state.data$uam, state.data$Group, IQR)

for(i in 1:length(group)){
  res_frame[tab,as.character(group[i])] <- stat_result[[as.character(group_N[i])]]$median
}
kruskal_result <- kruskal.test(formula = uam ~ Group, data = state.data)
print(kruskal_result)
res_frame[c(tab,tab+1),"Kruskal"] <- c(as.numeric(kruskal_result$statistic), kruskal_result$p.value)
dunn_result <- dunnTest(uam ~ Group, data = state.data,method="bh") # or non
print(dunn_result)
res_frame[tab+2,c(1)] <- c(dunn_result$res$P.adj[1])
res_frame[tab+3,c(1,2)] <- c(dunn_result$res$P.adj[2],dunn_result$res$P.adj[3])
res_frame[tab+4,c(1,2,3)] <- c(dunn_result$res$P.adj[4],dunn_result$res$P.adj[5],dunn_result$res$P.adj[6])
res_frame[tab+5,c(1,2,3,4)] <- c(dunn_result$res$P.adj[7],dunn_result$res$P.adj[8],dunn_result$res$P.adj[9],dunn_result$res$P.adj[10])

# Time span to prior usage
state.data$tpu <- (state.data$timestamp_end_diff-state.data$time_diff)-(state.data$timestamp_end_diff_mea_minus)
tab <- 19
stat_result <- describeBy(state.data$tpu,state.data$Group)
print(stat_result)
for(i in 1:length(group)){
  res_frame[tab,as.character(group[i])] <- stat_result[[as.character(group_N[i])]]$median
}
kruskal_result <- kruskal.test(formula = tpu ~ Group, data = state.data)
print(kruskal_result)
res_frame[c(tab,tab+1),"Kruskal"] <- c(as.numeric(kruskal_result$statistic), kruskal_result$p.value)
dunn_result <- dunnTest(tpu ~ Group, data = state.data,method="bh") # or non
print(dunn_result)
res_frame[tab+2,c(1)] <- c(dunn_result$res$P.adj[1])
res_frame[tab+3,c(1,2)] <- c(dunn_result$res$P.adj[2],dunn_result$res$P.adj[3])
res_frame[tab+4,c(1,2,3)] <- c(dunn_result$res$P.adj[4],dunn_result$res$P.adj[5],dunn_result$res$P.adj[6])
res_frame[tab+5,c(1,2,3,4)] <- c(dunn_result$res$P.adj[7],dunn_result$res$P.adj[8],dunn_result$res$P.adj[9],dunn_result$res$P.adj[10])


# Time span to next usage
state.data$tnu <- (state.data$timestamp_end_diff_mea_plus-state.data$time_diff_mea_plus)-(state.data$timestamp_end_diff_mea)
tab <- 25
stat_result <- describeBy(state.data$tnu,state.data$Group)
print(stat_result)
by(state.data$tnu, state.data$Group, IQR)

for(i in 1:length(group)){
  res_frame[tab,as.character(group[i])] <- stat_result[[as.character(group_N[i])]]$median
}
kruskal_result <- kruskal.test(formula = tnu ~ Group, data = state.data)
print(kruskal_result)
res_frame[c(tab,tab+1),"Kruskal"] <- c(as.numeric(kruskal_result$statistic), kruskal_result$p.value)
dunn_result <- dunnTest(tnu ~ Group, data = state.data,method="bh") # or non
print(dunn_result)
res_frame[tab+2,c(1)] <- c(dunn_result$res$P.adj[1])
res_frame[tab+3,c(1,2)] <- c(dunn_result$res$P.adj[2],dunn_result$res$P.adj[3])
res_frame[tab+4,c(1,2,3)] <- c(dunn_result$res$P.adj[4],dunn_result$res$P.adj[5],dunn_result$res$P.adj[6])
res_frame[tab+5,c(1,2,3,4)] <- c(dunn_result$res$P.adj[7],dunn_result$res$P.adj[8],dunn_result$res$P.adj[9],dunn_result$res$P.adj[10])


for(i in 1:nrow(res_frame)){
  for(j in 1:ncol(res_frame)){
    if(is.na(res_frame[i,j])){
      next
    }
    if(res_frame[i,j]>=1){
      res_frame[i,j] <- round(res_frame[i,j],0)
    }else if(res_frame[i,j]>=0.05){
      res_frame[i,j] <- round(res_frame[i,j],2)
    }else if(res_frame[i,j]>=0.01){
      res_frame[i,j] <- round(res_frame[i,j],3)
    }
  }
}

write(kable(res_frame, format = "html"), file = "8.1_table.html")







