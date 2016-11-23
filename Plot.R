# initialize
library(ggplot2)
library(RColorBrewer)
setwd("C:/Users/Felix/Dropbox/Exchange/Universitaet/WS 16_17 KW/BA/Ausarbeitung/7. Thesis/plots") 

graphics.off()

sub <- data.frame()
rgb <- list()
lab <- c("bored","litte to do","balanced","slightly under pressure","stressed")
labe <- c("bored","","balanced","","stressed")
lab_at <- c(-2,-1,0,1,2)

for(dev in 1:dev_N){
  sub[dev] <- subset(db, variable == "esm_boredom_stress" & id == dev)
  rgb[[dev]] <- rgb(runif(1),runif(1),runif(1))
}

# # Plot alle arousal over time all dev
# for(dev in 1:dev_N){
#   x = sub$timestamp_end
#   y = sub$arousal-2
#   sub <- subset(db, variable == "esm_boredom_stress" & id == dev)
#   if(dev == 1){
#     plot(x,y, main="name", ylim=c(0,4), col = rgb[[dev]])
#   }
#   else{
#     lines(x, y, col = rgb[[dev]])
#   }
# }

# 
# 
# # Plot z density
# 
# 
# # Plot participants number of answers
# x = arousal$id
# y = arousal$N
# plot(x,y)
# 
# # Plot participants answers over x
# for(dev in 1:dev){
#   sub <- subset(db, variable == "esm_boredom_stress" & id == dev)
#   x = rep(arousal$id[dev],arousal$N[dev])
#   y = sub$arousal-2
#   if(dev == 1){
#     plot(x,y, main="scatter plot", xlim=c(0,80), ylim=c(-2.5,2.5), col = rgb[[dev]], yaxt="n")
#     axis(2, at= lab_at, labels=lab) 
#   }
#   else{
#     points(x, y, col = rgb[[dev]])
#   }
# }
# 
# # Plot mean arousal over mean usage
# for(dev in 1:dev){
#   x = arousal$mean[dev]-2
#   y = onoff$tot_mean[dev]
#   sub <- subset(db, variable == "personality_boredom" & id == dev)
#   if (nrow(sub)==0){
#     x_2=NaN
#   }else{
#     x_2 <- as.numeric(as.character(sub$value))
#   }
#   
#   if(dev == 1){
#     plot(x,y, main="scatter plot", xlim=c(-2,2), col = rgb[[dev]])
#     points(x_2,y,  col = rgb[[dev]], pch=22)
#   }
#   else{
#     points(x, y,  col = rgb[[dev]])
#     points(x_2,y,  col = rgb[[dev]], pch=22)
#     
#   }
# }


# Plot Increacse answer time after installation
sub <- subset(db, variable == "esm_boredom_stress")
# show devices  less than x answers
#delete <- arousal$id[arousal$N<8]
#sub <- sub[sub$id %in% delete,]

# show devices less then 5 days study time
delete <- arousal$id[arousal$duration<4*24*60*60]
sub <- sub[sub$id %in% delete,]

cor <-cor.test(sub$timestamp_end_diff,sub$time_diff)
ggplot(sub, aes(x=timestamp_end_diff/60/60/24, y=time_diff/60/60,group=id,color=sub$color)) +
  geom_line() +
  theme(legend.position="none") +
  geom_text(aes(label=paste0(timestamp_end, ": ",as.character(arousal-2))),hjust=0, vjust=0)
  #labs(x=paste0("N=",arousal_N, "r=",cor,")"), y="Frequency") 

