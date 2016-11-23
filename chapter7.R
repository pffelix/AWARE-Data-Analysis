library(prettyR)
theme_set(theme_grey(base_size = 16)) 
library(psych)
library(agricolae)


dt_min <- 1*60*60
dt_max <- 1*60*60
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/onoff_calc.R")
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/arousal_calc.R")

# variable definition
blu <- "#1F497D"
graphics.off()

#### Chapter Correlation boredom, smartphone usage
print("Chapter Evaluation Study method")
sub_state <- list()
group <- factor(c("bored","little to do","balanced","slightly under pressure","stressed"), levels = c("bored","little to do","balanced","slightly under pressure","stressed"))
#group <- c("bored","little to do","balanced","slightly under pressure","stressed")
for (ar in 1:5){
  sub_state[[ar]] <- subset(db, variable == "esm_boredom_stress" & arousal == ar-1)
  sub_state[[ar]] <- sub_state[[ar]][,c("arousal","usage_time","usage_freq")]
}
state_info <- data.frame("N"=c(nrow(sub_state[[1]]),nrow(sub_state[[2]]),nrow(sub_state[[3]]),nrow(sub_state[[4]]),nrow(sub_state[[5]])))
for (ar in 1:5){
sub_state[[ar]][,"Group"] <- as.factor(paste0(group[ar]," (N=",state_info$N[ar],")"))
}
sub_arousal <- subset(db, variable == "esm_boredom_stress")
sub_arousal <- sub_arousal[,c("arousal","usage_time","usage_freq")]


# table mid average influence smartphone usage
for (ar in 1:length(sub_state)){
   print(describeBy(sub_state[[ar]]$usage_time/60))
}
print(describeBy(sub_arousal$usage_time/60))

for (ar in 1:length(sub_state)){
  print(describeBy(sub_state[[ar]]$usage_freq))
}
print(describeBy(sub_arousal$usage_freq))

# boxplot usage time - states
fun_mean <- function(x){
  return(data.frame(y=round(mean(x),digits=0),label=round(mean(x,na.rm=T)),digits=0))}
state.data <- rbind(sub_state[[1]], sub_state[[2]], sub_state[[3]], sub_state[[4]],sub_state[[5]])
state.data$usage_time <- state.data$usage_time/60
#state.data$Group <- factor(state.data$Group,levels=sample(levels(state.data$Group)))

ggplot(state.data, aes(x=arousal, y=usage_time/dt, fill=Group,order=Group)) +
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
  ggsave(file="boxplot_arousal_usage_time_h.emf")

# boxplot usage freq - states
ggplot(state.data, aes(x=arousal, y=usage_freq/dt, fill=Group,order=Group)) +
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
  ggsave(file="boxplot_arousal_usage_freq_h.emf")



# scatterplot average daily usage time - usage freq
cor <- cor.test(sub_arousal$usage_time,sub_arousal$usage_freq)
sub_arousal <- subset(db, variable == "esm_boredom_stress")
sub_arousal <- sub_arousal[,c("arousal","usage_time","usage_freq")]
ggplot(data=sub_arousal, aes(x=usage_time/dt, y=usage_freq/dt))+
  geom_point(color=blu) +
  xlab(paste0("Daytime smartphone usage in min/h"," (N=",arousal_N,")")) +
  ylab(paste0("Daytime smartphone usage in frequeny/h"," (N=",arousal_N,")")) +
  #geom_point(shape=1)+
  geom_smooth(method=loess,aes(colour="Non parametric LOESS regression")) +
  geom_smooth(method=lm, aes(colour=paste0("Linear regression curve r(",cor["parameter"],")=",round(as.numeric(cor["estimate"]),2),", p<.001"))) +
  scale_colour_manual(name="Legend", values=c("black", "red"), position="top")+
  theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99)) +
  ggsave(file="scatterplot_usage_time_usage_freq_h.emf") 


# run Anova to compare group differences state - averge usage time per day
state.data <- rbind(sub_state[[1]],sub_state[[2]],sub_state[[3]], sub_state[[4]],sub_state[[5]])
state.data$usage_time <- state.data$usage_time/60/dt
anova <- aov(formula = usage_time ~ Group, data = state.data)
sum_anova <- summary(anova)
TukeyHSD(anova)
describeBy(state.data$usage_time,state.data$Group)

#duncan.test(anova)





