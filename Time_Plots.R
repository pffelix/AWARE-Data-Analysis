library(ggplot2)
library(cowplot)

db_temp_init <- db
# delete devices data cleansing:
sub <- subset(db, variable == "esm_boredom_stress")
id_vector <- unique(sub$id)
plot_vector <- list()
max_dev <- length(id_vector)
value_last <- 0
count <-0
for (dev in 1:max_dev){
  sub <- subset(db, variable == "esm_boredom_stress" & id==dev & timestamp_end_diff <=60*60*24*14)
  values_included <- unique(sub$arousal)
  if(length(values_included)<3){
    print(paste0("var id=",dev))
    print(paste0("var device id=",info$device_id[dev]))
  }else if(nrow(sub)<10){
    print(paste0("row id=",dev))
    print(paste0("row device id=",info$device_id[dev]))
    print(paste0("n=",nrow(sub)))
  }else{
    for (i in 1:nrow(sub)){
      value_now <- sub$arousal[i]
      print <- FALSE
      if (value_now==value_last){
        count <- count +1
        if (count>10){
            printed <-TRUE
            #print(paste0("10er id=",dev))
            #print(paste0("10er device id=",info$device_id[dev]))
        }
      }else{
        count <- 0
      }
      value_last <- value_now
    }
  }

}
# Plot answers every participant over time
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/multiplot.R")
setwd("C:/Users/Felix/Dropbox/Exchange/Universitaet/WS 16_17 KW/BA/Ausarbeitung/7. Thesis/Plots")

sub <- subset(db, variable == "esm_boredom_stress")
id_vector <- unique(sub$id)
plot_vector <- list()
max_dev <- length(id_vector)
for (dev in 1:max_dev){
  p <- ggplot(data=sub[sub$id==dev,]) +
    geom_line(aes(x=timestamp_end_diff/60/60/24, y=arousal-2)) + 
    xlab(paste0("Days after signup")) +
    ylab(paste0("self-assesed arousal state"))+
    scale_y_continuous(breaks=c(-2,-1,0,1,2),labels=c("bored", "", "balanced","", "stressed"),limits=c(-2,2))+
    scale_x_continuous(breaks=c(0,3.5,7,10.5,14), labels=c("0","3.5","7","10.5","14"),limits=c(0,14))
  #print(p)
  plot_vector[[dev]] <- p
}
print(plot_vector[1])
p1 <- plot_grid(plotlist = plot_vector[c(1:15)], ncol=3, labels = id_vector[c(1:15)],hjust=-21)
p1.2 <- plot_grid(plotlist = plot_vector[c(1:15)], ncol=3, labels = id_vector[c(1:15)],hjust=-10)
p2 <- plot_grid(plotlist = plot_vector[c(16:30)], ncol=3, labels = id_vector[c(16:30)],hjust=-10)
p3 <- plot_grid(plotlist = plot_vector[c(31:45)], ncol=3, labels = id_vector[c(31:45)],hjust=-10)
p4 <- plot_grid(plotlist = plot_vector[c(46:60)], ncol=3, labels = c(46,47,48,49,50),hjust=-10)

save_plot(file="plot_device_01_09_arousal_time.emf", plot=p1, base_width=11, base_height=17)
save_plot(file="plot_device_10_15_arousal_time.emf", plot=p1.2, base_width=11, base_height=17)
save_plot(file="plot_device_16_30_arousal_time.emf", plot=p2, base_width=11, base_height=17)
save_plot(file="plot_device_31_45_arousal_time.emf", plot=p3, base_width=11, base_height=17)
save_plot(file="plot_device_46_60_arousal_time.emf", plot=p4, base_width=11, base_height=17)


