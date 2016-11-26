library(prettyR)
theme_set(theme_grey(base_size = 16)) 
library(psych)

# variable definition
blu <- "#1F497D"
lab <- c("bored","litte to do","balanced","slightly under pressure","stressed")
labe <- c("bored","","balanced","","stressed")
lab_at <- c(-2,-1,0,1,2)
color_arousal <- c("green","orange",blu,"purple","brown")

#### Chapter Evaluation Study method
print("Chapter Evaluation Study method")

# Descriptive statistics Feedback
item <- c("week_stress","week_boredom","feedback_overall","feedback_app","feedback_questions","feedback_esm_identify","feedback_esm_frequency","feedback_esm_reliability","feedback_esm_intuitive","feedback_monitoring","feedback_reactivity","feedback_extra","feedback_continue")
for(pos in 1:length(item)) {
  sub <- subset(db, variable == item[pos])
  print(item[pos])
  print(describe(as.numeric(as.character(sub$value))))# mat=TRUE
  print(freq(as.character(sub$value)))
}



# Validity scale - boredom stress week rewareded

sub <- subset(db, variable == "week_stress") 
id_vector_bored <- sub$id
id_vector_bored <- id_vector_bored[!is.na(id_vector_bored)]
mood_week <- sub[sub$id %in% id_vector_bored, ]
mood_week$value <-as.numeric(as.character(mood_week$value))
time_bored_week <- mood_week$timestamp_end_diff

sub <- subset(db, variable == "esm_boredom_stress") 
sub <- sub[sub$id %in% id_vector_bored, ]
mood_esm <- data.frame()
for(dev in id_vector_bored) {
  week_dev <- subset(mood_week, id==dev) 
  week_dev_t <- week_dev$timestamp_end_diff
  #4print(week_dev_t)
  delta <-7*24*60*60
  sub_temp <- subset(sub, id == dev & timestamp_end_diff >= week_dev_t-delta & timestamp_end_diff <= week_dev_t, ) 
  mood_esm <- rbind(mood_esm,sub_temp)
}
arousal_mean <- data.frame(mood_esm$id,as.numeric(as.character(mood_esm$value)))
colnames(arousal_mean)<- c("id","value")
arousal_mean <- aggregate(arousal_mean, list(arousal_mean$id), mean)

cor.test(mood_week$value+2,arousal_mean$value)



#### Chapter statistics collected data
print("Chapter statistics collected data")
sub <- subset(db, variable == "esm_boredom_stress")

### Total

## Number of arousal answers

# Descriptive
sub <- subset(db, variable == "esm_boredom_stress")
sub$id <- 1
table <- describeBy(as.numeric(as.character(sub$value)))
print(table)
sub <- subset(db, variable == "esm_boredom_stress")
freq(as.character(sub$value))

# Histogram arousal state
sub <- subset(db, variable == "esm_boredom_stress")
ggplot(sub, aes(arousal-2)) +
  n        <- nrow(sub)
  mean_v     <- mean(sub$arousal-2)
  sd_v       <- sd(sub$arousal-2)
  binwidth <- 1
  ggplot(sub, aes(arousal-2)) +
  geom_histogram( aes(),colour="black", fill=blu,binwidth = 1,boundary = -0.5) +
  scale_x_continuous(breaks=lab_at,labels=lab)+
  stat_function(fun = function(x, mean_v, sd_v, n, bw){dnorm(x = x, mean = mean_v, sd = sd_v) * n * bw},args = c(mean = mean_v, sd = sd_v, n = n, bw = binwidth))+
  labs(x=paste0("Self-assessed arousal state (N=",arousal_N,", skew=",round(table$skew,2),", kurtosis=",round(table$kurtosis,2),")"), y="Frequency") 
  ggsave(file="hist_ar_time.emf")
  
# Q-Q Plot arousal state
sub <- subset(db, variable == "esm_boredom_stress")
linearmodel.lm = lm(usage_time ~ arousal, data=sub) 
linearmodel.lm.res=residuals(linearmodel.lm)
shapiro.test(linearmodel.lm.res)
linearmodel.lm.stdres = rstandard(linearmodel.lm)
qqnorm(linearmodel.lm.stdres, ylab="Expected normal value",  xlab="Observed value", main="Normal Q-Q Plot Usage time") 
qqline(linearmodel.lm.stdres)

# Histogram balanced(filter) - usage time
sub <- subset(db, variable == "esm_boredom_stress" & arousal==2)
ggplot(sub, aes(usage_time/dt/60)) +
  geom_histogram(col="black", fill=blu,binwidth = 1, boundary = -0.5) 
  #labs(x="Arousal level", y="Frequency") 
  #scale_x_continuous(breaks=lab_at,labels=lab)+
  #labs(x=paste0("Self-assessed arousal state (N=",arousal_N,", skew=",round(table$skew,2),", kurtosis=",round(table$kurtosis,2),")"), y="Frequency") 
#ggsave(file="hist_ar_freq.emf")


  
# cor: time increase - time_diff arousal answer
cor.test(sub$timestamp_end_diff,sub$time_diff, alternative="greater")


# cor: time increase - time_interval arousal answers
arousal_diff <- c()
for(dev in 1:dev_N) {
  sub_dev <- subset(sub, id==dev) 
  arousal_diff_dev <- diff(sub_dev$timestamp_end_diff)
  arousal_diff <- as.numeric(c(arousal_diff,NA,arousal_diff_dev))
}
cor.test(sub$timestamp_end_diff,arousal_diff, alternative="greater")

    
## Between subjects

# Average arousal self-assesment per participant
describe(arousal$N)
freq(arousal$N)

# Average arousal self-assesment per participant per day
study_duration <- c()
for(dev in 1:dev_N) {
  sub_dev <- subset(db, id==dev) 
  last_timestamp_end <- tail(sub_dev, n=1)$timestamp_end_diff
  study_duration <- as.numeric(c(study_duration,last_timestamp_end))
}
study_duration_days <- study_duration/24/60/60
answers_per_day <- arousal$N/study_duration_days
describe(answers_per_day)


### screen on/off events
sub <- subset(db, variable == "smartphone_usage_time") 

## Smartphone usage per day
describeBy(24*60/onoff$freq)

## self-assed smartphone usage per day
describeBy(as.numeric(as.character(sub$value)))

## Correlation self assesed - actual smartphone usage time
id_vector <- sub$id
on_off_id_vector <- onoff[onoff$id %in% id_vector, ]
cor.test(24*60/on_off_id_vector$freq,as.numeric(as.character(sub$value)))

# T-test self-assed , actual usage
# if t.test(a,b) is independent (2 group) -> paired=FALSE (standard) + do var.test(a,b) if p<0.05 -> var.equal = FALSE (standard)
# if t.test(a,meanb) is dependent (1 group) -> paired=TRUE
t.test(as.numeric(as.character(sub$value)),24*60/on_off_id_vector$freq, paired=TRUE)
## Within subjects
#describeBy(as.numeric(as.character(sub$value)), sub$id)

group <- c(paste0("total"," (N=",sum(onoff$N),")"))
group <- factor(group, levels = group)

# boxplot usage time
fun_mean <- function(x){
  return(data.frame(y=round(mean(x),digits=0),label=round(mean(x,na.rm=T)),digits=0))}
ggplot(onoff) + aes(x=1, y=usage_time_day, fill=group) +
  geom_boxplot(fill=blu) +
  xlab(paste0("Measured at"," N=",sum(onoff$N), " time points")) +
  #theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank()) +
  ylab("Average smartphone usage in min per day") +
  scale_x_discrete() +
  #xlab(paste0("Self-assessed arousal state")) +
  #ylab("Daily smartphone usage time in min") +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
  #ylim(0,120) + scale_y_continuous(breaks = seq(0, 120, by = 20))
  ggsave(file="boxplot_usage_time.emf")

# boxplot usage freq
fun_mean <- function(x){
  return(data.frame(y=round(mean(x),digits=1),label=round(mean(x,na.rm=T)),digits=1))}
ggplot(onoff) + aes(x=1, y=usage_freq_day, fill=group) +
  geom_boxplot(fill=blu) +
  xlab(paste0("Measured at"," N=",sum(onoff$N), " time points")) +
  ylab("Average smartphone usage frequency per day") +
  scale_x_discrete() +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
  ggsave(file="boxplot_usage_freq.emf")

  
