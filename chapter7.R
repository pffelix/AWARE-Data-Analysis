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
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/tools.R")
setwd("C:/Users/Felix/Dropbox/Exchange/Universitaet/WS 16_17 KW/BA/Ausarbeitung/7. Thesis/Plots")

# dt_min <- 1*60*60
# dt_max <- 1*60*60
# source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/onoff_calc.R")
# source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/arousal_calc.R")

# variable definition
blu <- "#1F497D"
graphics.off()
color_arousal <- c(blu,'#43a2ca','#7bccc4','#bae4bc','#f0f9e8')
color_2 <- c(blu,"steelblue2","black",'orange',"red")
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
sub_state[[ar]][,"Group"] <- as.factor(paste0(group[ar],"\n (N=",state_info$N[ar],")"))
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



# boxplot usage time - states, 5 minute prior window
fun_mean <- function(x){
  return(data.frame(y=round(median(x),digits=0),label=paste0("Mdn=",round(median(x,na.rm=T))),digits=0))}
state.data <- rbind(sub_state[[1]], sub_state[[2]], sub_state[[3]], sub_state[[4]],sub_state[[5]])
#state.data$arousal[state.data$arousal==0] <- group[1]
#state.data$arousal[state.data$arousal==1] <- group[2]
#state.data$arousal[state.data$arousal==2] <- group[3]
#state.data$arousal[state.data$arousal==3] <- group[4]
#state.data$arousal[state.data$arousal==4] <- group[5]

#state.data$Group <- factor(state.data$Group,levels=sample(levels(state.data$Group)))

p1 <- ggplot(state.data, aes(x=Group, y=usage_time,order=Group,fill=Group)) +
    stat_boxplot(geom ='errorbar',size=1) + 
    geom_boxplot() +
    #scale_x_discrete() + 
    xlab(paste0("Self-assessed arousal state")) +
    ylab("Smartphone usage time in seconds") +
    guides(fill=FALSE) +
    theme_grey( ) +
    #stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
    stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) +
    scale_fill_manual(values = color_2) +
    theme(panel.grid.major = element_line(colour = "white")) +
    theme(panel.grid.minor = element_line(colour = "white"))+
    scale_y_continuous(breaks = seq(0, 300,25))
    #scale_y_continuous(breaks=seq(0,600,25))+
    #theme(#axis.title.x=element_blank(),
      #axis.text.x=element_blank(),
      #axis.ticks.x=element_blank()) +
    #ggsave(file="boxplot_arousal_usage_time_5minp.emf")
print(p1)

# boxplot usage freq - states, 5 minute prior window

p2 <- ggplot(state.data, aes(x=Group, y=usage_freq,order=Group,fill=Group)) +
      stat_boxplot(geom ='errorbar',size=1) + 
      geom_boxplot() +
      #scale_x_discrete() + 
      xlab(paste0("Self-assessed arousal state")) +
      ylab("Smartphone usage frequency") +
      guides(fill=FALSE) +
      theme_grey( ) +
      #stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
      stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) +
      scale_fill_manual(values = color_2) +
      theme(panel.grid.major = element_line(colour = "white")) +
      theme(panel.grid.minor = element_line(colour = "white"))+
      scale_y_continuous(breaks = seq(0, 10,1))
      #scale_y_continuous(breaks=seq(0,600,25))+
      #theme(#axis.title.x=element_blank(),
      #axis.text.x=element_blank(),
      #axis.ticks.x=element_blank()) +
      #ggsave(file="boxplot_arousal_usage_freq_5minp.emf")
print(p2)


boxplot_grid <- plot_grid(p1,p2, ncol=2, labels = c(),hjust=-21)
save_plot(file="boxplot_arousal_usage_5minp.emf", plot=boxplot_grid, base_width=12, base_height=4)

  
# run Anova to compare group differences state - averge usage time (Variances are homogen as Bartlett Test  K-squared = 4.4026, p=0.3543>0.05 [filinger >0.05], df=4)
data_anova <- rbind(sub_state[[1]],sub_state[[2]],sub_state[[3]],sub_state[[4]],sub_state[[5]])
data_anova$usage_time <- data_anova$usage_time
bartlett.test(usage_time ~ Group, data = data_anova)
leveneTest(usage_freq ~ Group, data = data_anova) # inhomogeneous variacnes if p<.05
leveneTest(usage_time ~ Group, data = data_anova) # inhomogeneous variacnes if p<.05
anova <- aov(formula = usage_time ~ Group, data = data_anova)
sum_anova <- summary(anova)
extractAIC(anova)
TukeyHSD(anova)
describeBy(state.data$usage_time,state.data$Group)
print(sum_anova)

# run Kruskal-Wallis test to compare group differences state - averge usage time
describeBy(state.data$usage_time,state.data$Group)
data_kruskal <- rbind(sub_state[[1]],sub_state[[2]],sub_state[[3]],sub_state[[4]],sub_state[[5]])
data_kruskal$usage_time <- data_kruskal$usage_time
kruskal_result <- kruskal.test(formula = usage_time ~ Group, data = data_kruskal)
print(kruskal_result)
#kruskalmc(usage_time ~ Group, data = data_kruskal) # post hoc differences
jonckheere.test(data_kruskal$usage_time,data_kruskal$arousal) # post hoc trend
dunnTest(usage_time ~ Group, data = data_kruskal,method="bh") # or non
#pairwise.wilcox.test(data_kruskal$usage_time,data_kruskal$arousal, p.adjust.method="none")

# run Kruskal-Wallis test to compare group differences state - averge usage freq
describeBy(state.data$usage_freq,state.data$Group)
data_kruskal <- rbind(sub_state[[1]],sub_state[[2]],sub_state[[3]],sub_state[[4]],sub_state[[5]])
data_kruskal$usage_freq <- data_kruskal$usage_freq
kruskal_result <- kruskal.test(formula = usage_freq ~ Group, data = data_kruskal)
print(kruskal_result)
#kruskalmc(usage_time ~ Group, data = data_kruskal) # post hoc differences
jonckheere.test(data_kruskal$usage_freq,data_kruskal$arousal) # post hoc trend
dunnTest(usage_freq ~ Group, data = data_kruskal,method="bh") # or non
#pairwise.wilcox.test(data_kruskal$usage_freq,data_kruskal$arousal, p.adjust.method="none")

# scatterplot average daily usage time - usage freq
cor <- cor.test(sub_arousal$usage_time,sub_arousal$usage_freq)
sub_arousal <- subset(db, variable == "esm_boredom_stress")
#sub_arousal <- sub_arousal[,c("arousal","usage_time","usage_freq")]
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
  
