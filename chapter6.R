library(prettyR)
theme_set(theme_grey(base_size = 16)) 
library(psych)
library(cowplot)
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/tools.R")
library("car")
library(data.table)
library(coin)
setwd("C:/Users/Felix/Dropbox/Exchange/Universitaet/WS 16_17 KW/BA/Ausarbeitung/7. Thesis/Plots")

# variable definition
blu <- "#1F497D"
lab <- c("bored","litte to do","balanced","slightly under pressure","stressed")
labe <- c("bored","","balanced","","stressed")
lab_at <- c(-2,-1,0,1,2)
color_arousal <- c("green","orange",blu,"purple","brown")

#### Chapter Evaluation Study method
print("Chapter Evaluation Study method")


# Validity scale - boredom (if changed: stress) week rewareded

sub <- subset(db, variable == "week_boredom") 
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
  sub_temp <- subset(sub, id == dev & timestamp_end_diff >= 0 & timestamp_end_diff <= week_dev_t, ) #week_dev_t-delta instead of 0
  mood_esm <- rbind(mood_esm,sub_temp)
}
arousal_mean <- data.frame(mood_esm$id,as.numeric(as.character(mood_esm$value)))
colnames(arousal_mean)<- c("id","value")
arousal_mean <- aggregate(arousal_mean, list(arousal_mean$id), median)
id_vector_arousal <- unique(arousal_mean$id)
id_vector_arousal <- id_vector_arousal[!is.na(id_vector_arousal)]
mood_week <-mood_week[mood_week$id %in% id_vector_arousal, ]


cor.test(mood_week$value+2,arousal_mean$value,method = c("spearman"))
cor.test(mood_week$value+2,arousal_mean$value)


# Validity scale - boredom stress week forwarded

sub <- subset(db, variable == "week_workload_time") 
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
  sub_temp <- subset(sub, id == dev & timestamp_end_diff >= week_dev_t & timestamp_end_diff <= week_dev_t+delta, ) 
  mood_esm <- rbind(mood_esm,sub_temp)
}
arousal_mean <- data.frame(mood_esm$id,as.numeric(as.character(mood_esm$value)))
colnames(arousal_mean)<- c("id","value")
arousal_mean <- aggregate(arousal_mean, list(arousal_mean$id), median)
id_vector_arousal <- unique(arousal_mean$id)
id_vector_arousal <- id_vector_arousal[!is.na(id_vector_arousal)]
mood_week <-mood_week[mood_week$id %in% id_vector_arousal, ]

cor.test(mood_week$value+2,arousal_mean$value,method = c("spearman"))
cor.test(mood_week$value+2,arousal_mean$value)


# Correlation week_workload_valence - arousal state forwarded

sub <- subset(db, variable == "week_workload_valence") 
id_vector_bored <- sub$id
id_vector_bored <- id_vector_bored[!is.na(id_vector_bored)]
mood_week <- sub[sub$id %in% id_vector_bored, ]
mood_week$value <-as.numeric(as.character(mood_week$value))
time_bored_week <- mood_week$timestamp_end_diff

sub <- subset(db, variable == "esm_boredom_stress" & arousal== c(2,3,4))
sub <- sub[sub$id %in% id_vector_bored, ]
mood_esm <- data.frame()
for(dev in id_vector_bored) {
  week_dev <- subset(mood_week, id==dev) 
  week_dev_t <- week_dev$timestamp_end_diff
  #4print(week_dev_t)
  delta <-7*24*60*60
  sub_temp <- subset(sub, id == dev & timestamp_end_diff >= week_dev_t & timestamp_end_diff <= week_dev_t+delta, ) 
  mood_esm <- rbind(mood_esm,sub_temp)
}
arousal_mean <- data.frame(mood_esm$id,as.numeric(as.character(mood_esm$value)))
colnames(arousal_mean)<- c("id","value")
arousal_mean <- aggregate(arousal_mean, list(arousal_mean$id), median)
id_vector_arousal <- unique(arousal_mean$id)
id_vector_arousal <- id_vector_arousal[!is.na(id_vector_arousal)]
mood_week <-mood_week[mood_week$id %in% id_vector_arousal, ]

cor.test(mood_week$value+2,arousal_mean$value,method = c("spearman"))
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
addline_format <- function(x,...){
  gsub('\\_','\n',x)
}
sub <- subset(db, variable == "esm_boredom_stress")
d_ar_N <- describeBy(sub$arousal,sub$arousal)
lab_N <- c(paste0("bored_N=",d_ar_N[[1]]$n),paste0("litte to do_N=",d_ar_N[[2]]$n),paste0("balanced_N=",d_ar_N[[3]]$n),paste0("slightly under pressure_N=",d_ar_N[[4]]$n),paste0("stressed_N=",d_ar_N[[5]]$n))
xlab(expression(atop("A long string of text for the purpose", paste("of illustrating my point" [reported]))))
sub <- subset(db, variable == "esm_boredom_stress")
n <- nrow(sub)
mean_v <- mean(sub$arousal-2)
sd_v <- sd(sub$arousal-2)
binwidth <- 1
ggplot(sub, aes(arousal-2)) +
  geom_histogram( aes(),colour="black", fill=blu,binwidth = 1,boundary = -0.5) +
  scale_x_continuous(breaks=lab_at,labels=addline_format(lab_N))+
  stat_function(fun = function(x, mean_v, sd_v, n, bw){dnorm(x = x, mean = mean_v, sd = sd_v) * n * bw},args = c(mean = mean_v, sd = sd_v, n = n, bw = binwidth))+
  labs(x=paste0("Self-assessed arousal state (N=",arousal_N,", skew=",round(table$skew,2),", kurtosis=",round(table$kurtosis,2),")"), y="Frequency") 
ggsave(file="hist_ar_time.emf")
  
# Test normality Kolmogorov - Smirnov arousal answer (only continuous)
  
x=seq(0,4,length=nrow(sub))
y=dnorm(x,mean=2,sd=1)
temp <- sub$arousal
ks.test(y,temp)
  
# Q-Q Plot arousal state
sub <- subset(db, variable == "esm_boredom_stress")
linearmodel.lm = lm(usage_time ~ arousal, data=sub) 
linearmodel.lm.res=residuals(linearmodel.lm)
shapiro.test(linearmodel.lm.res)
linearmodel.lm.stdres = rstandard(linearmodel.lm)
qqnorm(linearmodel.lm.stdres, ylab="Expected normal value",  xlab="Observed value", main="Normal Q-Q Plot Usage time") 
qqline(linearmodel.lm.stdres)
qqPlot(linearmodel.lm.stdres)



# Histogram balanced(filter) - usage time
sub <- subset(db, variable == "esm_boredom_stress" & arousal==2)
ggplot(sub, aes(usage_time/dt/60)) +
  geom_histogram(col="black", fill=blu,binwidth = 1, boundary = -0.5) 
  #labs(x="Arousal level", y="Frequency") 
  #scale_x_continuous(breaks=lab_at,labels=lab)+
  #labs(x=paste0("Self-assessed arousal state (N=",arousal_N,", skew=",round(table$skew,2),", kurtosis=",round(table$kurtosis,2),")"), y="Frequency") 
#ggsave(file="hist_ar_freq.emf")


  

    
# Average arousal self-assesment per participant
describe(arousal$N)
freq(arousal$N)


# Average arousal self-assesment per participant per day (calculate from feedbac measurment point)
study_duration <- c()
for(dev in 1:dev_N) {
  sub_dev <- subset(db, id==dev) 
  last_timestamp_end <- tail(sub_dev, n=1)$timestamp_end_diff
  study_duration <- as.numeric(c(study_duration,last_timestamp_end))
}
study_duration_days <- study_duration/24/60/60
answers_per_day <- arousal$N/study_duration_days
describe(answers_per_day)

# Average arousal self-assesment per participant per day (calculate from last esm measurment point)
answers_per_day <- arousal$N/arousal$duration*24*60*60
describe(answers_per_day)


# cor: time increase - time_diff arousal answer
cor.test(sub$timestamp_end_diff,sub$time_diff, alternative="greater")
cor.test(sub$timestamp_end_diff,sub$time_diff, alternative="greater", method="spearman")


# cor: time increase - time_interval arousal answers
arousal_diff <- c()
for(dev in 1:dev_N) {
  sub_dev <- subset(sub, id==dev) 
  arousal_diff_dev <- diff(sub_dev$timestamp_end_diff)
  arousal_diff <- as.numeric(c(arousal_diff,NA,arousal_diff_dev))
}
cor.test(sub$timestamp_end_diff,arousal_diff, alternative="greater")
cor.test(sub$timestamp_end_diff,arousal_diff, alternative="greater", method="spearman")


### Measured screen on/off events

## Measured Smartphone usage per day
#total
describeBy(onoff$usage_time_day/60)
IQR(onoff$usage_time_day/60)
describeBy(onoff$usage_freq_day)
IQR(onoff$usage_freq_day)
# male
sub <- setDT(db)[, gender := value[variable == "demographic_gender"], by = id]
sub_Male <- subset(sub, gender == "Male") 
onoff_Male <- data.frame()
ids <- unique(sub_Male$id)
i <- 0
for (dev in ids) {
  i <- i+1
  search_frame <- subset(sub_Male, variable == "screen" & id == dev & value == "on") 
  row_names <- row.names(search_frame)
  onoff_Male[i, "id"] <- dev
  onoff_Male[i, "N"] <- nrow(search_frame)
  onoff_Male[i, "tot"] <- sum(search_frame$time_diff)
  onoff_Male[i, "freq"] <- search_frame$timestamp_end_diff[onoff_Male[i, "N"]]/onoff_Male[i, "tot"]
  onoff_Male[i,"usage_time_day"] <- onoff_Male[i, "tot"]/(search_frame$timestamp_end_diff[onoff_Male[i, "N"]])*24*60*60
  onoff_Male[i,"usage_freq_day"] <- onoff_Male[i, "N"]/(search_frame$timestamp_end_diff[onoff_Male[i, "N"]])*24*60*60
}
describeBy(24*60/onoff_Male$freq)
# female
sub_Female <- subset(sub, gender == "Female") 
onoff_Female <- data.frame()
ids <- unique(sub_Female$id)
i <- 0
for (dev in ids) {
  i <- i+1
  search_frame <- subset(sub_Female, variable == "screen" & id == dev & value == "on") 
  row_names <- row.names(search_frame)
  onoff_Female[i, "id"] <- dev
  onoff_Female[i, "N"] <- nrow(search_frame)
  onoff_Female[i, "tot"] <- sum(search_frame$time_diff)
  onoff_Female[i, "freq"] <- search_frame$timestamp_end_diff[onoff_Female[i, "N"]]/onoff_Female[i, "tot"]
  onoff_Female[i,"usage_time_day"] <- onoff_Female[i, "tot"]/(search_frame$timestamp_end_diff[onoff_Female[i, "N"]])*24*60*60
  onoff_Female[i,"usage_freq_day"] <- onoff_Female[i, "N"]/(search_frame$timestamp_end_diff[onoff_Female[i, "N"]])*24*60*60
}
describeBy(24*60/onoff_Female$freq)
# Gender differences usage time
GroupA <- onoff_Female$usage_time_day/60
GroupB <- onoff_Male$usage_time_day/60
mwu_test(GroupA,GroupB) # r small size: 0.1	medium size:0.3	large size:0.5

# Gender differences usage freq
GroupA <- onoff_Female$usage_freq_day
GroupB <- onoff_Male$usage_freq_day
mwu_test(GroupA,GroupB) # r small size: 0.1	medium size:0.3	large size:0.5

## self-assed smartphone usage per day
sub <- subset(db, variable == "smartphone_usage_time") 
describeBy(as.numeric(as.character(sub$value)))
IQR(as.numeric(as.character(sub$value)))

## Correlation self assesed - actual smartphone usage time
id_vector <- sub$id
on_off_id_vector <- onoff[onoff$id %in% id_vector, ]
cor.test(24*60/on_off_id_vector$freq,as.numeric(as.character(sub$value)))
cor.test(24*60/on_off_id_vector$freq,as.numeric(as.character(sub$value)), method="spearman")

# T-test self-assed , actual usage
# if t.test(a,b) is independent (2 group) -> paired=FALSE (standard) + do var.test(a,b) if p<0.05 -> var.equal = FALSE (standard)
# if t.test(a,meanb) is dependent (1 group) -> paired=TRUE
t.test(as.numeric(as.character(sub$value)),24*60/on_off_id_vector$freq, paired=TRUE)
## Within subjects
#describeBy(as.numeric(as.character(sub$value)), sub$id)


# boxplot usage time
#group <- c(paste0("total"," (N=",sum(onoff$N),")"))
#group <- factor(group, levels = group)
onoff$Group <- paste0("All participants (N=", nrow(onoff),")")
onoff_Female$Group <- paste0("Female participants (N=", nrow(onoff_Female),")")
onoff_Male$Group <- paste0("Male participants (N=", nrow(onoff_Male),")")
onoff_melted <- melt(rbind(onoff,onoff_Female,onoff_Male),"Group",c("usage_time_day","usage_freq_day","N"))
fun_mean <- function(x){
  return(data.frame(y=round(median(x),digits=1),label=paste0("Mdn=",round(median(x,na.rm=T))),digits=1))}
p1 <- ggplot(subset(onoff_melted,variable=="usage_time_day"),aes(x=Group, y=value/60, group=Group,fill=Group)) +
      stat_boxplot(geom ='errorbar',size=1) + 
      geom_boxplot() +
      guides(fill=FALSE) +
      #lab(paste0("") +
      #scale_x_continuous(expand=c(0,0)) +
      ylab("Average smartphone usage time in minutes per day") +
      scale_x_discrete() +
      scale_fill_manual(values = c(blu,blu,blu)) +
      #xlab(paste0("Self-assessed arousal state")) +
      #ylab("Daily smartphone usage time in min") +
      stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
      stat_summary(fun.data =  fun_mean, geom="text", vjust=1.7,colour = "white") +
      theme(panel.grid.major = element_line(colour = "white")) +
      theme(panel.grid.minor = element_line(colour = "white"))+
      scale_y_continuous(breaks=seq(0,600,25))+
      theme_grey( ) +
      theme(axis.title.x=element_blank()) 
      #ylim(0,120) + scale_y_continuous(breaks = seq(0, 120, by = 20))
      #ggsave(file="boxplot_usage_time.emf")
print(p1)


# boxplot usage freq
p2 <- ggplot(subset(onoff_melted,variable=="usage_freq_day"),aes(x=Group, y=value, group=Group,fill=Group)) +
  stat_boxplot(geom ='errorbar',size=1) + 
  geom_boxplot() +
  guides(fill=FALSE) +
  #lab(paste0("") +
  #scale_x_continuous(expand=c(0,0)) +
  ylab("Average smartphone usage time in minutes per day") +
  scale_x_discrete() +
  scale_fill_manual(values = c(blu,blu,blu)) +
  #xlab(paste0("Self-assessed arousal state")) +
  ylab("Average smartphone usage frequency per day") +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data =  fun_mean, geom="text", vjust=1.5,colour = "white") +
  theme(panel.grid.major = element_line(colour = "white")) +
  theme(panel.grid.minor = element_line(colour = "white"))+
  scale_y_continuous(breaks=seq(0,600,25))+
  theme_grey( ) +
  theme(axis.title.x=element_blank()) 
#ylim(0,120) + scale_y_continuous(breaks = seq(0, 120, by = 20))
#ggsave(file="boxplot_usage_time.emf")
print(p2)


boxplot_grid <- plot_grid(p1,p2, ncol=2, labels = c(),hjust=-21)
save_plot(file="boxplot_usage_time_freq_comb.emf", plot=boxplot_grid, base_width=11, base_height=4)


# Q-Q Plot usage state
sub <- subset(db, variable == "esm_boredom_stress")
qqnorm(sub$usage_time)
qqline(sub$usage_time)
qqPlot(sub$usage_time)
