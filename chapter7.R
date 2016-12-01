library(prettyR)
library(psych)
library(agricolae)
library(ggplot2)
library('reshape2')
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/tools.R")


# dt_min <- 1*60*60
# dt_max <- 1*60*60
# source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/onoff_calc.R")
# source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/arousal_calc.R")

# variable definition
blu <- "#1F497D"
graphics.off()
color_arousal <- c("green","orange","blue","purple","brown")

#### Chapter Correlation boredom, smartphone usage
print("Chapter Evaluation Study method")
sub_state <- list()
group <- factor(c("Bored","Little to do","Balanced","Slightly under pressure","Stressed"), levels = c("Bored","Little to do","Balanced","Slightly under pressure","Stressed"))
#group <- c("bored","little to do","balanced","slightly under pressure","stressed")
for (ar in 1:5){
  sub_state[[ar]] <- subset(db, variable == "esm_boredom_stress" & arousal == ar-1, select=c("arousal","usage_time","usage_freq"))
}
state_info <- data.frame("N"=c(nrow(sub_state[[1]]),nrow(sub_state[[2]]),nrow(sub_state[[3]]),nrow(sub_state[[4]]),nrow(sub_state[[5]])))
for (ar in 1:5){
sub_state[[ar]][,"Group"] <- as.factor(paste0(group[ar]," (N=",state_info$N[ar],")"))
}
sub_arousal <- subset(db, variable == "esm_boredom_stress", select = c("arousal","usage_time","usage_freq"))


# table mid average influence smartphone usage
for (ar in 1:length(sub_state)){
   print(describeBy(sub_state[[ar]]$usage_time/60))
}
print(describeBy(sub_arousal$usage_time/60))

for (ar in 1:length(sub_state)){
  print(describeBy(sub_state[[ar]]$usage_freq))
}
print(describeBy(sub_arousal$usage_freq))


# run Anova to compare group differences state - averge usage time per day (Variances are homogen as Bartlett Test  K-squared = 4.4026, p=0.3543>0.05 [filinger >0.05], df=4)
data_anova <- rbind(sub_state[[1]],sub_state[[2]],sub_state[[3]],sub_state[[4]],sub_state[[5]])
data_anova$usage_time <- data_anova$usage_time/60/dt
bartlett.test(usage_time ~ Group, data = data_anova)
anova <- aov(formula = usage_time ~ Group, data = data_anova)
sum_anova <- summary(anova)
print(sum_anova)
extractAIC(anova)

TukeyHSD(anova)
describeBy(state.data$usage_time,state.data$Group)

# boxplot usage time - states
fun_mean <- function(x){
  return(data.frame(y=round(mean(x),digits=0),label=round(mean(x,na.rm=T)),digits=0))}
state.data <- rbind(sub_state[[1]], sub_state[[2]], sub_state[[3]], sub_state[[4]],sub_state[[5]])
#state.data$Group <- factor(state.data$Group,levels=sample(levels(state.data$Group)))

ggplot(state.data, aes(x=arousal, y=usage_time/dt/60, fill=Group,order=Group)) +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  #scale_x_discrete() + 
  xlab(paste0("Self-assessed arousal state")) +
  ylab("Daytime smartphone usage in min/h") +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) +
  ylim(0,120) + scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  theme(#axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) +
  ggsave(file="box_arousal_usage_time.emf")

# boxplot usage freq - states
ggplot(state.data, aes(x=arousal, y=usage_freq/dt, fill=Group,order=Group)) +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  #scale_x_discrete() + 
  xlab(paste0("Self-assessed arousal state")) +
  ylab("Daytime smartphone usage in frequency/h") +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) +
  ylim(0,120) + scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  theme(#axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) +
  ggsave(file="box_arousal_usage_freq.emf")



# scatterplot average daily usage time - usage freq
cor <- cor.test(sub_arousal$usage_time,sub_arousal$usage_freq)
sub_arousal <- subset(db, variable == "esm_boredom_stress")
sub_arousal <- sub_arousal[,c("arousal","usage_time","usage_freq")]
ggplot(data=sub_arousal, aes(x=usage_time/dt/60, y=usage_freq/dt))+
  geom_point(color=blu) +
  xlab(paste0("Daytime smartphone usage in min/h"," (N=",arousal_N,")")) +
  ylab(paste0("Daytime smartphone usage in frequeny/h"," (N=",arousal_N,")")) +
  #geom_point(shape=1)+
  geom_smooth(method=loess,aes(colour="Non parametric LOESS regression curve")) +
  geom_smooth(method=lm, aes(colour=paste0("Linear regression curve r(",cor["parameter"],")=",round(as.numeric(cor["estimate"]),2),", p<.001"))) +
  scale_colour_manual(name="Legend", values=c("black", "red"), position="top")+
  theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99)) +
  ggsave(file="scatter_usage_time_usage_freq.emf") 






# Plot F value and sig over time window
plot_list <-list(simul_anova_time_dt,simul_anova_freq_dt)
plot_name <- list(deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt)))
for (i in 1:length(plot_list)){
  plot_data_anova <-plot_list[[i]]
  ylim_p <- ceiling(max(plot_data_anova$F_))
  xlim_p <- max(plot_data_anova$dt_min+plot_data_anova$dt_max)
  print(ggplot() + ylim(0, ylim_p) + xlim(0,xlim_p) + 
    geom_line(data=plot_data_anova, aes(dt_min+dt_max, F_, colour = "Group differences: ANOVA F(df=4)",linetype = "Group differences: ANOVA F(df=4)" ), size=1) + 
    geom_line(data=plot_data_anova, aes(dt_min+dt_max, sig*ylim_p, colour = "Group differences: Sig. p", linetype = "Group differences: Sig. p"),size=1) + 
    geom_line(data=plot_data_anova, aes(dt_min+dt_max, rep(0.05*ylim_p,nrow(plot_data_anova)),colour ="Sig. p=.05", linetype = "Sig. p=.05")) + 
    xlab(paste0("Window length centered at arousal measurement points in min")) +
    scale_colour_manual("Legend", labels = c("Group differences: ANOVA F(df=4)", "Group differences: Sig. p", "Sig. p=.05"), values = c(blu, blu, "black")) +
    scale_linetype_manual("Legend",values=c("solid", "dotted","dotted"),labels=c("Group differences: ANOVA F(df=4)", "Group differences: Sig. p", "Sig. p=.05")) +
    theme(legend.justification=c(0.3,0.99), legend.position=c(0.3,0.99)) +
    scale_x_continuous(breaks = round(seq(0, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 20),1)) +
    scale_y_continuous("F-Value", sec.axis = sec_axis(~ . / ylim_p, name = "p-value")
    ))
    ggsave(file=paste0(as.character(plot_name[[i]]),"_Anova.emf"))
}

# Plot group mean and duncan sig over time window (total time)
plot_list <-list(simul_anova_time_dt_min,simul_anova_time_dt_max,simul_anova_time_dt, simul_anova_freq_dt_min, simul_anova_freq_dt_max, simul_anova_freq_dt)
plot_name <- list(deparse(substitute(simul_anova_time_dt_min)),deparse(substitute(simul_anova_time_dt_max)),deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt_min)),deparse(substitute(simul_anova_freq_dt_max)),deparse(substitute(simul_anova_freq_dt)))
legend_name <- c("Mean: Bored","Mean: Little to do","Mean: Balanced","Mean: Slightly under pressure","Mean: Stressed","Sig. differences: Bored - balanced","Sig. differences: Little to do - balanced","Sig. differences: Slightly under pressure - balanced","Sig. differences: Stressed - balanced","Sig. p=.05")
breaks_point <- c(1:10)
cols <- c("green","orange", "red","blue", "purple","green","orange","blue", "purple", "black")
line_type <- c("solid", "solid","solid","solid","solid","dashed", "dashed","dashed","dashed","dashed")
line_size <- c(1,1,1,1,1,1,1,1,1,0.5)
#linetype_vector <- legend_vector
for (i in 1:length(plot_list)){
  plot_data_anova <-plot_list[[i]]
  if (grepl("freq",plot_name[[i]])){
    legend_entry <- "Mean smartphone usage in min/h"
  }else{
    legend_entry <- "Mean smartphone usage in frequency/h"
  }
  xlim_p <- max(plot_data_anova$dt_min+plot_data_anova$dt_max)
  xlim_m <- 5
  plot_data_anova_ylim <- subset(plot_data_anova, dt_min+dt_max >=xlim_m)
  ylim_p <- ceiling(max(c(plot_data_anova_ylim$M1,plot_data_anova_ylim$M2,plot_data_anova_ylim$M3,plot_data_anova_ylim$M4,plot_data_anova_ylim$M5)))
  plot_data_anova$sig1 <- plot_data_anova$sig1*ylim_p
  plot_data_anova$sig2 <- plot_data_anova$sig2*ylim_p
  plot_data_anova$sig3 <- plot_data_anova$sig3*ylim_p
  plot_data_anova$sig4 <- plot_data_anova$sig4*ylim_p
  plot_data_anova$sig5 <- plot_data_anova$sig5*ylim_p
  plot_data_anova[,"p"] <- rep(0.05*(ylim_p),nrow(plot_data_anova))
  melted <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M3","M4","M5","sig1","sig2","sig4","sig5","p"))
  print(ggplot(data=melted) +
     geom_line(aes(dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group)) +
     xlab(paste0("Window length centered at arousal measurement points in min")) +
     scale_colour_manual("Group",label= legend_name, values= cols) +
     scale_linetype_manual("Group",label= legend_name, values=line_type) +
     scale_size_manual ("Group",label= legend_name, values=line_size) +
     guides(colour=guide_legend("Group",nrow=5,ncol=2)) +
     theme(legend.position="top", legend.direction = "vertical") +
     scale_x_continuous(limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 20),1)) +
     scale_y_continuous(legend_entry, limits=c(0,ylim_p), sec.axis = sec_axis(~ . / (ylim_p), name = "p-value Post-Hoc(Tukey)")
     )
    )
  ggsave(file=paste0(as.character(plot_name[[i]]),"_Mean.emf")) 
}

  
# Plot group difference to balanced and duncan sig over time window (total time)
plot_list <-list(simul_anova_time_dt_min,simul_anova_time_dt_max,simul_anova_time_dt, simul_anova_freq_dt_min, simul_anova_freq_dt_max, simul_anova_freq_dt)
plot_name <- list(deparse(substitute(simul_anova_time_dt_min)),deparse(substitute(simul_anova_time_dt_max)),deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt_min)),deparse(substitute(simul_anova_freq_dt_max)),deparse(substitute(simul_anova_freq_dt)))
legend_name <- c("Differences: Bored - balanced","Differences: Little to do - balanced","Differences: Slightly under pressure - balanced","Differences: Stressed - balanced","Sig. differences: Bored - balanced","Sig. differences: Little to do - balanced","Sig. differences: Slightly under pressure - balanced","Sig. differences: Stressed - balanced","Sig. differences: Bored - stressed","Sig. p=.05")
breaks_point <- c(1:10)
cols <- c("green","orange", "blue", "purple","green","orange","blue", "purple", "red","black")
line_type <- c("solid", "solid","solid","solid","solid", "solid","solid","solid","solid","dotted")
line_size <- c(2,2,2,2,0.5,0.5,0.5,0.5,0.5,0.5)
line_shape <- c(1,1,1,1,5,5,5,5,5,1)
#linetype_vector <- legend_vector
p_sig_scale <- 1 # if 5% sig -> 20
for (i in 1:length(plot_list)){
  plot_data_anova <-plot_list[[i]]
  plot_data_anova$M1 <- (plot_data_anova$M1-plot_data_anova$M3)/plot_data_anova$M3*100
  plot_data_anova$M2 <- (plot_data_anova$M2-plot_data_anova$M3)/plot_data_anova$M3*100
  plot_data_anova$M3 <- plot_data_anova$M3
  plot_data_anova$M4 <- (plot_data_anova$M4-plot_data_anova$M3)/plot_data_anova$M3*100
  plot_data_anova$M5 <- (plot_data_anova$M5-plot_data_anova$M3)/plot_data_anova$M3*100
  if (grepl("freq",plot_name[[i]])){
    legend_entry <- "Usage frequency differences in %"
  }else{
    legend_entry <- "Usage time differences in %"
  }
  xlim_p <- max(plot_data_anova$dt_min+plot_data_anova$dt_max)
  xlim_m <- 5
  plot_data_anova_ylim <- subset(plot_data_anova, dt_min+dt_max >=xlim_m)
  ylim_m <- floor(min(c(plot_data_anova_ylim$M1,plot_data_anova_ylim$M2,plot_data_anova_ylim$M4,plot_data_anova_ylim$M5)))
  ylim_p <- ceiling(max(c(plot_data_anova_ylim$M1,plot_data_anova_ylim$M2,plot_data_anova_ylim$M4,plot_data_anova_ylim$M5)))
  plot_data_anova$sig1 <- plot_data_anova$p1*ylim_p*p_sig_scale
  plot_data_anova$sig2 <- plot_data_anova$p2*ylim_p*p_sig_scale
  plot_data_anova$sig3 <- plot_data_anova$p3*ylim_p*p_sig_scale
  plot_data_anova$sig4 <- plot_data_anova$p4*ylim_p*p_sig_scale
  plot_data_anova$sig5 <- plot_data_anova$p5*ylim_p*p_sig_scale
  plot_data_anova[,"p"] <- rep(0.05*(ylim_p),nrow(plot_data_anova))*p_sig_scale
  melted <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M4","M5","sig1","sig2","sig3","sig4","sig5","p"))
  print(ggplot(data=melted) +
          geom_line(aes(dt_min+dt_max, y=value, group=Group,colour=Group, size=Group, linetype=Group,shape=Group)) +
          xlab(paste0("Window length centered at arousal measurement points in min")) +
          scale_colour_manual("Group",label= legend_name, values= cols) +
          scale_linetype_manual("Group",label= legend_name, values=line_type) +
          scale_shape_manual("Group",label= legend_name, values=line_shape) +
          scale_size_manual ("Group",label= legend_name, values=line_size) +
          guides(colour=guide_legend("Group",nrow=5,ncol=2)) +
          theme(legend.position="top", legend.direction = "vertical") +
          scale_x_continuous(limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 20),1)) +
          scale_y_continuous(legend_entry, limits=c(ylim_m,ylim_p), sec.axis = sec_axis(~ . / (ylim_p*p_sig_scale), name = "p-value Post-Hoc(Tukey)")
          )
  )
  ggsave(file=paste0(as.character(plot_name[[i]]),"_Differences.emf")) 
}

# Plot group mean and duncan sig over time window (limited time)
plot_list <-list(simul_anova_time_dt,simul_anova_freq_dt)
plot_name <- list(deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt)))
legend_name <- c("Mean: Bored","Mean: Little to do","Mean: Balanced","Mean: Slightly under pressure","Mean: Stressed","Sig. differences: Bored - balanced","Sig. differences: Little to do - balanced","Sig. differences: Slightly under pressure - balanced","Sig. differences: Stressed - balanced","Sig. p=.05")
breaks_point <- c(1:10)
cols <- c("green","orange", "red","blue", "purple","green","orange","blue", "purple", "black")
line_type <- c("solid", "solid","solid","solid","solid","dashed", "dashed","dashed","dashed","dashed")
line_type <- c("solid", "solid","solid","solid","solid","dashed", "dashed","dashed","dashed","dashed")
line_size <- c(1,1,1,1,1,1,1,1,1,0.5)
#linetype_vector <- legend_vector
for (i in 1:length(plot_list)){
  plot_data_anova <-plot_list[[i]]
  if (grepl("freq",plot_name[[i]])){
    legend_entry <- "Mean smartphone usage in min/h"
  }else{
    legend_entry <- "Mean smartphone usage in frequency/h"
  }
  xlim_p <- 40
  xlim_m <- 5
  plot_data_anova_ylim <- subset(plot_data_anova, dt_min+dt_max >=xlim_m)
  ylim_p <- ceiling(max(c(plot_data_anova_ylim$M1,plot_data_anova_ylim$M2,plot_data_anova_ylim$M3,plot_data_anova_ylim$M4,plot_data_anova_ylim$M5)))
  plot_data_anova$sig1 <- plot_data_anova$sig1*ylim_p
  plot_data_anova$sig2 <- plot_data_anova$sig2*ylim_p
  plot_data_anova$sig3 <- plot_data_anova$sig3*ylim_p
  plot_data_anova$sig4 <- plot_data_anova$sig4*ylim_p
  plot_data_anova$sig5 <- plot_data_anova$sig5*ylim_p
  plot_data_anova[,"p"] <- rep(0.05*(ylim_p),nrow(plot_data_anova))
  melted <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M3","M4","M5","sig1","sig2","sig4","sig5","p"))
  print(ggplot(data=melted) +
          geom_line(aes(dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group, size=Group)) +
          xlab(paste0("Window length centered at arousal measurement points in min")) +
          scale_colour_manual("Group",label= legend_name, values= cols) +
          scale_linetype_manual("Group",label= legend_name, values=line_type) +
          scale_size_manual ("Group",label= legend_name, values=line_size) +
          guides(colour=guide_legend("Group",nrow=5,ncol=2)) +
          theme(legend.position="top", legend.direction = "vertical") +
          scale_x_continuous(limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 20),1)) +
          scale_y_continuous(legend_entry, limits=c(0,ylim_p), sec.axis = sec_axis(~ . / (ylim_p), name = "p-value (Post-Hoc: Tukey)")
          )
  )
  ggsave(file=paste0(as.character(plot_name[[i]]),"_Mean_limited_window.emf")) 
}



# scatterplot average arousal z-value - usage time
sub_arousal <- subset(db, variable == "esm_boredom_stress")
sub_arousal <- sub_arousal[,c("arousal_z","usage_time","usage_freq")]
cor <- cor.test(sub_arousal$usage_time,sub_arousal$usage_freq)
ggplot(data=sub_arousal, aes(x=arousal_z, y=usage_time/dt/60))+
  geom_point(color=blu) +
  xlab(paste0("Self-assed arousal state"," (N=",arousal_N,")")) +
  ylab(paste0("Daytime smartphone usage in min/h"," (N=",arousal_N,")")) +
  #geom_point(shape=1)+
  geom_smooth(method=loess,aes(colour="Non parametric LOESS regression curve")) +
  geom_smooth(method=lm, aes(colour=paste0("Linear regression curve r(",cor["parameter"],")=",round(as.numeric(cor["estimate"]),2),", p<.001"))) +
  scale_colour_manual(name="Legend", values=c("black", "red"), position="top")+
  theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99)) +
  ggsave(file="scatter_z_all_usage_time.emf") 

# scatterplot average arousal z-value - usage time
sub_arousal <- subset(db, variable == "esm_boredom_stress")
sub_arousal <- sub_arousal[,c("arousal_z","usage_time","usage_freq")]
cor <- cor.test(sub_arousal$usage_time,sub_arousal$usage_freq)
ggplot(data=sub_arousal, aes(x=arousal_z, y=usage_time/dt/60))+
  geom_point(color=blu) +
  xlab(paste0("Self-assed arousal state"," (N=",arousal_N,")")) +
  ylab(paste0("Daytime smartphone usage in min/h"," (N=",arousal_N,")")) +
  #geom_point(shape=1)+
  geom_smooth(method=loess,aes(colour="Non parametric LOESS regression curve")) +
  geom_smooth(method=lm, aes(colour=paste0("Linear regression curve r(",cor["parameter"],")=",round(as.numeric(cor["estimate"]),2),", p<.001"))) +
  scale_colour_manual(name="Legend", values=c("black", "red"), position="top")+
  theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99)) +
  ggsave(file="scatter_z_all_usage_freq.emf") 


# scatterplot average arousal z-value - usage freq
sub_arousal <- subset(db, variable == "esm_boredom_stress")
sub_arousal <- sub_arousal[,c("arousal_z","usage_time","usage_freq")]
cor <- cor.test(sub_arousal$usage_time,sub_arousal$usage_freq)
ggplot(data=sub_arousal, aes(x=arousal_z, y=usage_freq/dt))+
  geom_point(color=blu) +
  xlab(paste0("Self-assed arousal state"," (N=",arousal_N,")")) +
  ylab(paste0("Daytime smartphone usage in freq/h"," (N=",arousal_N,")")) +
  #geom_point(shape=1)+
  geom_smooth(method=loess,aes(colour="Non parametric LOESS regression curve")) +
  geom_smooth(method=lm, aes(colour=paste0("Linear regression curve r(",cor["parameter"],")=",round(as.numeric(cor["estimate"]),2),", p<.001"))) +
  scale_colour_manual(name="Legend", values=c("black", "red"), position="top")+
  theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99)) +
  ggsave(file="scatter_z_all_usage_freq.emf") 


# boxplot average arousal z-value (or normale value) grouped - usage time : makes no sense as grouping brings back de-interpolating
# fun_mean <- function(x){
#   return(data.frame(y=round(mean(x),digits=0),label=round(mean(x,na.rm=T)),digits=0))}
# data <- subset(db, variable == "screen" & !is.nan(time_diff) & !is.nan(arousalp)& !is.nan(arousalp_z))
# data <- data[,c("arousalp","arousalp_z","time_diff")]
# data[,"Group"] <- as.factor(round(data$arousalp))
# #state.data$Group <- factor(state.data$Group,levels=sample(levels(state.data$Group)))
# data_plot <- rbind(data)
# ggplot(data_plot, aes(x=arousalp, y=time_diff/60, group=Group, fill=Group,order=Group)) +
#   geom_boxplot() +
#   #scale_x_discrete() + 
#   xlab(paste0("Self-assessed arousal state")) +
#   ylab("Daytime smartphone usage in min/h") +
#   stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
#   stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) +
#   #ylim(0,120) + scale_y_continuous(breaks = seq(0, 120, by = 20)) +
#   theme(#axis.title.x=element_blank(),
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank()) +
#   ggsave(file="box_arousal_usage_time_linear_interpol.emf")

# scatterplot average arousal z-value (or normale value) - usage time : no interesting results
# sub_arousal <- subset(db, variable == "screen" & arousalp)
# sub_arousal <- sub_arousal[,c("arousalp","time_diff","arousalp_z")]
# cor <- cor.test(sub_arousal$arousalp_z,sub_arousal$time_diff)
# ggplot(data=sub_arousal, aes(x=arousalp_z, y=time_diff))+
#   geom_point(color=blu) +
#   xlab(paste0("Self-assed arousal state"," (N=",arousal_N,")")) +
#   ylab(paste0("Daytime smartphone usage in freq/h"," (N=",arousal_N,")")) +
#   #geom_point(shape=1)+
#   geom_smooth(method=loess,aes(colour="Non parametric LOESS regression curve")) +
#   #geom_smooth(method=lm, aes(colour=paste0("Linear regression curve r(",cor["parameter"],")=",round(as.numeric(cor["estimate"]),2),", p<.001"))) +
#   scale_colour_manual(name="Legend", values=c("black"), position="top")+
#   theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99))
#   #ggsave(file="scatter_arousal_usage_time_linear_interpol.emf")
# 

# check if correlation exist between z arousal and usage time / freq (no interesting effect)
sub_bored <- subset(db, arousal_z <0)
sub_stressed <- subset(db, arousal_z >0)
cor <- cor.test(sub_stressed$arousal_z,sub_stressed$usage_time/60/dt)
#ggplot(data=sub_bored, aes(arousal_z,usage_time/60/dt))+
ggplot(data=sub_stressed, aes(arousal_z,usage_freq/dt))+
  geom_point(color=blu) +
  xlab(paste0("Z-value self-assed arousal state"," (N=",nrow(sub_stressed),")")) +
  ylab(paste0("Daytime smartphone usage in min/h"," (N=",nrow(sub_stressed),")")) +
  #geom_point(shape=1)+
  geom_smooth(method=loess,aes(colour="Non parametric LOESS regression curve")) +
  stat_smooth(method="lm",fill=NA,colour="black",linetype=2,geom="ribbon") +
  geom_smooth(method=lm, aes(fill = "95%",colour=paste0("Linear regression curve r(",cor["parameter"],")=",round(as.numeric(cor["estimate"]),2),", p<.001"))) +
  scale_colour_manual(name="Legend", values=c("black", "red"), position="top")+
  scale_fill_grey(name = 'Confidence level') +
  theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99))
  
