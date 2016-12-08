library(plyr)
library(cowplot)
library(ggplot2)
library(reshape2)
blu <- "#1F497D"

setwd("C:/Users/Felix/Dropbox/Exchange/Universitaet/WS 16_17 KW/BA/Ausarbeitung/7. Thesis/Plots")

db_temp <- db
# smartphone usage over time
sub <- subset(db_temp, variable == "esm_boredom_stress" & arousal==2 & timestamp_end_diff <60*60*24*7)
mean(sub$usage_time/dt)/60

# visualize arousal and on/off behaviour over time

usage_time <-  c()
usage_freq <-  c()

sub_screen <- subset(db_temp, variable == "screen" & value == "on")
sub_arousal <- subset(db_temp, variable == "esm_boredom_stress")
name_row_arousal <- rownames(sub_arousal)
name_row_screen <- rownames(sub_screen)
dt_time_vector <- 60*60*24
number_days <- 13
time_vector <- seq(0,number_days*24*60*60,dt_time_vector)
time_vector_N <- length(time_vector)
over_time <- data.frame("id"=rep(empty(0),dev_N*time_vector_N),"time"=rep(empty(0),dev_N*time_vector_N),"usage_time"=rep(empty(0),dev_N*time_vector_N),"usage_freq"=rep(empty(0),dev_N*time_vector_N),"arousal_freq_0"=rep(empty(0),dev_N*time_vector_N),"arousal_freq_1"=rep(empty(0),dev_N*time_vector_N),"arousal_freq_2"=rep(empty(0),dev_N*time_vector_N),"arousal_freq_3"=rep(empty(0),dev_N*time_vector_N),"arousal_freq_4"=rep(empty(0),dev_N*time_vector_N))
over_time[,"id"] <- rep(1:dev_N,each=time_vector_N)
over_time[,"time"] <- rep(1:time_vector_N,dev_N)

for (dev in 1:dev_N){
  sub_screen_dev <- subset(sub_screen, id == dev)
  dev_max_time <- sub_screen_dev$timestamp_end_diff[nrow(sub_screen_dev)]
  for (i in 1:time_vector_N){

    time_sub <- time_vector[i]
    time_min <- time_sub
    time_max <- time_sub + dt_time_vector
    sub_screen_time <- subset(sub_screen, id == dev & timestamp_end_diff >= time_min & timestamp_end_diff <= time_diff+time_max, )

    if (nrow(sub_screen_time) <1){
      if(time_sub > dev_max_time){
        usage_time <- c(usage_time, NA)
        usage_freq <- c(usage_freq, NA)
      }else{
        usage_time <- c(usage_time, 0)
        usage_freq <- c(usage_freq, 0)
      }
      next
    }

    if (dev==53){
      if(i==9){
        print("x")

      }
    }

    # correct overlapping intervalls
    for (j in 1:nrow(sub_screen_time)){
      # wrong AWARE database entry, arousal entry wherby smartphone was off -> delete (delete_arousal_wrong_off)
      timestamp_diff <- sub_screen_time$timestamp_end_diff[j]-sub_screen_time$time_diff[j]
      if(sub_screen_time$timestamp_end_diff[j] > time_max){
        #print(sub_screen_time$timestamp_end_diff[j]-time_max)
        sub_screen_time$time_diff[j] <- sub_screen_time$time_diff[j]-(sub_screen_time$timestamp_end_diff[j]-time_max)
      }
      if(timestamp_diff < time_min){
        #print(time_min-timestamp_diff)
        sub_screen_time$time_diff[j] <- sub_screen_time$time_diff[j]-(time_min-timestamp_diff)
      }
    }

    usage_freq <- c(usage_freq,nrow(sub_screen_time))
    # correct AWARE error: database entries with usage frequencys in milliseconds change set to 60/h max
    #if (sub_arousal_dev$X_id[i] == 6041 & usage_freq[length(usage_freq)]/dt >100){
    #usage_freq[length(usage_freq)] <- usage_freq[length(usage_freq)] - (351-17)
    #}
    if (usage_freq[length(usage_freq)] >350){
      usage_freq[length(usage_freq)] <- 350
    }

    usage_time <- c(usage_time, sum(sub_screen_time$time_diff))
    if (usage_time[length(usage_time)] >60*60*24){
      usage_time[length(usage_time)] <- 60*60*24
    }
    #print (position)
  }

}
over_time[,"usage_time"] <- usage_time
over_time[,"usage_freq"] <- usage_freq

usage_time_mean <- ddply(over_time, c("time"), function(x) mean(x$usage_time, na.rm=TRUE))
usage_time_median <- ddply(over_time, c("time"), function(x) median(x$usage_time, na.rm=TRUE))
usage_freq_mean <- ddply(over_time, c("time"), function(x) mean(x$usage_freq, na.rm=TRUE))
usage_freq_median <- ddply(over_time, c("time"), function(x) median(x$usage_freq, na.rm=TRUE))

arousal_freq_0 <- c()
arousal_freq_1 <- c()
arousal_freq_2 <- c()
arousal_freq_3 <- c()
arousal_freq_4 <- c()
for (dev in 1:dev_N){
  sub_arousal_dev <- subset(sub_arousal, id == dev)
  dev_max_time <- sub_screen_dev$timestamp_end_diff[nrow(sub_arousal_dev)]
  for (i in 1:time_vector_N){
    
    time_sub <- time_vector[i]
    time_min <- time_sub
    time_max <- time_sub + dt_time_vector
    sub_arousal_time <- subset(sub_arousal, id == dev & timestamp_end_diff >= time_min & timestamp_end_diff < time_max, )
    
    if (nrow(sub_arousal_time) <1){
      if(time_sub > dev_max_time){
        arousal_freq_0 <- c(arousal_freq_0, NA)
        arousal_freq_1 <- c(arousal_freq_1, NA)
        arousal_freq_2 <- c(arousal_freq_2, NA)
        arousal_freq_3 <- c(arousal_freq_3, NA)
        arousal_freq_4 <- c(arousal_freq_4, NA)
      }else{
        arousal_freq_0 <- c(arousal_freq_0, 0)
        arousal_freq_1 <- c(arousal_freq_1, 0)
        arousal_freq_2 <- c(arousal_freq_2, 0)
        arousal_freq_3 <- c(arousal_freq_3, 0)
        arousal_freq_4 <- c(arousal_freq_4, 0)
      }
      next
    }
    if(i==10 & dev==50){
      print("hallo")
    }
    sub_arousal_time_arousal <- sub_arousal_time$arousal
    count <- length(sub_arousal_time_arousal[sub_arousal_time_arousal== 0])
    arousal_freq_0 <- c(arousal_freq_0,count)
    count <- length(sub_arousal_time_arousal[sub_arousal_time_arousal== 1])
    arousal_freq_1 <- c(arousal_freq_1,count)
    count <- length(sub_arousal_time_arousal[sub_arousal_time_arousal== 2])
    arousal_freq_2 <- c(arousal_freq_2,count)
    count <- length(sub_arousal_time_arousal[sub_arousal_time_arousal== 3])
    arousal_freq_3 <- c(arousal_freq_3,count)
    count <- length(sub_arousal_time_arousal[sub_arousal_time_arousal== 4])
    arousal_freq_4 <- c(arousal_freq_4,count)
    
  }
}


over_time[,"arousal_freq_0"] <- arousal_freq_0
over_time[,"arousal_freq_1"] <- arousal_freq_1
over_time[,"arousal_freq_2"] <- arousal_freq_2
over_time[,"arousal_freq_3"] <- arousal_freq_3
over_time[,"arousal_freq_4"] <- arousal_freq_4


melted_usage_time <- melt(over_time, variable.name="Group", id=c("id","time"),measure.vars=c("usage_time"))
colnames(melted_usage_time)[1] <- "Participant"
p1 <-ggplot() +
      geom_line(data=melted_usage_time, aes(x=time, y=value/60, group=Participant, colour=Participant)) +
      geom_line(data = usage_time_median, aes(x=time, y=V1/60,linetype="solid"), colour = "red4", size=2) +
      geom_line(data = usage_time_mean, aes(x=time, y=V1/60,linetype="dotted"), colour = "red4", size=2) +
      #stat_summary(fun.y = "mean") +
      xlab(paste0("Days after signup for the study")) +
      ylab(paste0("Smartphone usage time per day in minutes"))+
      theme_grey() +
      scale_linetype_manual(name = 'Total sample', breaks=c("solid","dotted"),values =c('dotted'='dotted','solid'='solid'), labels = c('Median','Mean'))+
      scale_x_continuous(breaks = seq(0, 30, by = 1))+
      scale_y_continuous(breaks = seq(0, 1000, by = 50))+
      theme(legend.position="none")+
      #scale_colour_manual("Group",label= legend_name, values= cols) +
      #scale_linetype_manual("Group",label= legend_name, values=line_type) +
      #scale_size_manual ("Group",label= legend_name, values=line_size) +
      #guides(colour=guide_legend("Group",nrow=5,ncol=2)) +
      #theme(legend.position="top", legend.direction = "vertical") 
      #scale_x_continuous(limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 20),1)) +
      ggsave(file="week_usage_time_per_day.emf")
print(p1)


melted_usage_freq <- melt(over_time, variable.name="group", id=c("id","time"),measure.vars=c("usage_freq"))
colnames(melted_usage_freq)[1] <- "Participant"
p2 <-ggplot() +
  geom_line(data=melted_usage_freq, aes(x=time, y=value, group=Participant, colour=Participant)) +
  geom_line(data = usage_freq_median, aes(x=time, y=V1,linetype="solid"), colour = "red4", size=2) +
  geom_line(data = usage_freq_mean, aes(x=time, y=V1,linetype="dotted"), colour = "red4", size=2) +
  #stat_summary(fun.y = "mean") +
  xlab(paste0("Days after signup for the study")) +
  ylab(paste0("Smartphone usage frequency per day")) +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, 30, by = 1))+
  scale_y_continuous(breaks = seq(0, 600, by = 20))+
  scale_linetype_manual(name = 'Total sample', breaks=c("solid","dotted"),values =c('dotted'='dotted','solid'='solid'), labels = c('Median','Mean'))
  ggsave(file="week_usage_freq_per_day.emf")
#scale_colour_manual("Group",label= legend_name, values= cols) +
#scale_linetype_manual("Group",label= legend_name, values=line_type) +
#scale_size_manual ("Group",label= legend_name, values=line_size) +
#guides(colour=guide_legend("Group",nrow=5,ncol=2)) +
#theme(legend.position="top", legend.direction = "vertical") 
#scale_x_continuous(limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 20),1)) +
print(p2)

arousal_0_mean <- ddply(over_time, c("time"), function(x) mean(x$arousal_freq_0, na.rm=TRUE))
arousal_1_mean <- ddply(over_time, c("time"), function(x) mean(x$arousal_freq_1, na.rm=TRUE))
arousal_2_mean <- ddply(over_time, c("time"), function(x) mean(x$arousal_freq_2, na.rm=TRUE))
arousal_3_mean <- ddply(over_time, c("time"), function(x) mean(x$arousal_freq_3, na.rm=TRUE))
arousal_4_mean <- ddply(over_time, c("time"), function(x) mean(x$arousal_freq_4, na.rm=TRUE))

mean_data_frame <- data.frame("time"=arousal_0_mean[,1], "arousal_0_mean"=arousal_0_mean[,2], "arousal_1_mean"=arousal_1_mean[,2], "arousal_2_mean"=arousal_2_mean[,2], "arousal_3_mean"=arousal_3_mean[,2], "arousal_4_mean"=arousal_4_mean[,2])


melted_arousal_freq <- melt(mean_data_frame, variable.name="Group", id=c("time"),measure.vars=c("arousal_0_mean","arousal_1_mean","arousal_2_mean","arousal_3_mean","arousal_4_mean"))
total_arousal_mean <- ddply(melted_arousal_freq, c("time"), function(x) sum(x$value, na.rm=TRUE))

mean_data_frame <- data.frame("time"=arousal_0_mean[,1], "arousal_0_mean"=arousal_0_mean[,2], "arousal_1_mean"=arousal_1_mean[,2], "arousal_2_mean"=arousal_2_mean[,2], "arousal_3_mean"=arousal_3_mean[,2], "arousal_4_mean"=arousal_4_mean[,2],"total_mean"=total_arousal_mean[,2])
melted_arousal_freq <- melt(mean_data_frame, variable.name="Group", id=c("time"),measure.vars=c("arousal_0_mean","arousal_1_mean","arousal_2_mean","arousal_3_mean","arousal_4_mean","total_mean"))

color_2 <- c(blu,"steelblue2","black",'orange',"red")
cols <- c(color_2,"red4")
legend_name <- c("Bored","Little to do","Balanced","Slightly under pressure","Stressed","All")
line_size <- c(1,1,1,1,1,1.5)
p3 <-ggplot() +
  geom_line(data = melted_arousal_freq, aes(x=time, y=value, colour = Group, size=Group)) +
  #geom_line(data = usage_time_mean, aes(x=time, y=V1/60/60), colour = "red", size=2) +
  #stat_summary(fun.y = "mean") +
  xlab(paste0("Days after signup for the study")) +
  ylab(paste0("Mean answer frequency per day"))+
  theme_grey() +
  scale_colour_manual("Arousal state category",label= legend_name, values= cols) +
  scale_size_manual ("Arousal state category",label= legend_name, values=line_size) +
  scale_x_continuous(breaks = seq(0, 30, by = 1))+
  scale_y_continuous(breaks = seq(0, 30, by = 0.5))+
  ggsave(file="week_mean_arousal_freq_per_day.emf")
print(p3)
  #scale_colour_manual("Group",label= legend_name, values= cols) +
#scale_linetype_manual("Group",label= legend_name, values=line_type) +
#scale_size_manual ("Group",label= legend_name, values=line_size) +
#guides(colour=guide_legend("Group",nrow=5,ncol=2)) +
#theme(legend.position="top", legend.direction = "vertical") 
#scale_x_continuous(limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 20),1)) +


boxplot_grid <- plot_grid(p1,p2, ncol=2, labels = c(),hjust=-21,rel_widths=c(0.82,1))
save_plot(file="week_usage_time_freqper_day.emf",plot=boxplot_grid, base_width=12, base_height=4)






