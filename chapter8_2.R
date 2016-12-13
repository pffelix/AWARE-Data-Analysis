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
group_pre <- c("Bored","Little to do","Balanced","Slightly under pressure","Stressed")

for (ar in 1:5){
  group_pre[ar] <- paste0(group_pre[ar], " (N=",state_info$N[ar],")")
}
group <- factor(group_pre, levels = group_pre)

# Plot H value and sig over time window
plot_list <-list(simul_anova_time_dt,simul_anova_freq_dt)
plot_name <- list(deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt)))
legen_x_pos <- c(0.69,0.15)
legend_name <- c("usage time","usage frequency")
legend_pos_name <- list(c("Differences in usage time: H(df=4)","Differences in usage time: Sig. p","Sig. level p=.05"),c("Differences in usage frequency: H(df=4)","Differences in usage frequency: Sig. p","Sig. level p=.05"))
for (i in 1:length(plot_list)){
  plot_data_anova <-plot_list[[i]]
  ylim_p <- ceiling(max(plot_data_anova$H_))
  xlim_p <- max(plot_data_anova$dt_min+plot_data_anova$dt_max)
  print(ggplot() + ylim(0, ylim_p) + xlim(0,xlim_p) + 
          geom_line(data=plot_data_anova, aes(-dt_min+dt_max, sig, colour = legend_pos_name[[i]][2],linetype = legend_pos_name[[i]][2] ,size=legend_pos_name[[i]][2])) + 
          geom_line(data=plot_data_anova, aes(-dt_min+dt_max, H_/ylim_p , colour = legend_pos_name[[i]][1], linetype = legend_pos_name[[i]][1],size=legend_pos_name[[i]][1])) + 
          geom_line(data=plot_data_anova, aes(-dt_min+dt_max, rep(0.05,nrow(plot_data_anova)),colour =legend_pos_name[[i]][3], linetype = legend_pos_name[[i]][3], size=legend_pos_name[[i]][3])) + 
          xlab(paste0("Observation interval length around experience sampling answer in min")) +
          scale_colour_manual("All arousal groups", labels = legend_pos_name[[i]], values = c(blu, blu, "black")) +
          scale_linetype_manual("All arousal groups",values=c("twodash","solid","dotted"),labels=legend_pos_name[[i]]) +
          #scale_shape_manual(legend_name[i],values=c(5,6,0), labels = legend_pos_name[[i]])+
          scale_size_manual("All arousal groups",values=c(0.5,1,0.5),labels=legend_pos_name[[i]]) +
          theme_grey() +
          theme(legend.justification=c(0.3,0.99), legend.position=c(legen_x_pos[i],0.99)) +
          scale_x_continuous(expand=c(0,0),limits=c(-60,60),breaks = round(seq(-(max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max))/2, (max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max))/2, by = 5),1)) +
          scale_y_continuous(expand=c(0,0),limits=c(0,1),breaks=seq(0,1,by=0.05),"p-value", sec.axis = sec_axis(~ . * ylim_p, name = "H-value",breaks=seq(0,20,by=2))
          ))
  ggsave(file=paste0("8.2_3",as.character(plot_name[[i]]),"_Kruskal-Wallis.emf"))
}

# Plot group mean over time window (total time)
plot_list <-list(simul_anova_time_dt, simul_anova_freq_dt)
plot_name <- list(deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt)))
legend_name_all <- c("Bored","Little to do","Balanced","Slightly under pressure","Stressed","Sig. differences: Bored - balanced","Sig. differences: Little to do - balanced","Sig. differences: Slightly under pressure - balanced","Sig. differences: Stressed - balanced","Sig. p=.05")
breaks_point_al <- c(1:10)
cols_al <- c("green","orange", "red","blue", "purple","green","orange","blue", "purple", "black")
line_type_al <- c("solid", "solid","solid","solid","solid","dashed", "dashed","dashed","dashed","dashed")
line_size_al <- c(1,1,1,1,1,1,1,1,1,0.5)
legend_name_M <- group
breaks_point_M <- c(1:5)
cols_M <- color_2
line_type_M <- c("solid", "solid","solid","solid","solid")
line_size_M <- c(1,1,2,1,1)
legend_name <- c("Smartphone usage time","Smartphone usage frequency")
#linetype_vector <- legend_vector
for (i in 1:length(plot_list)){
  plot_data_anova <-plot_list[[i]]
  if (grepl("freq",plot_name[[i]])){
    legend_entry <- "Normalized mean smartphone usage time in min/h"
  }else{
    legend_entry <- "Normalized mean smartphone usage frequency per hour"
  }
  xlim_p <- max(plot_data_anova$dt_min+plot_data_anova$dt_max)
  xlim_m <- -60
  plot_data_anova_ylim <- subset(plot_data_anova, dt_min+dt_max >=xlim_m)
  ylim_p <- ceiling(max(c(plot_data_anova_ylim$M1,plot_data_anova_ylim$M2,plot_data_anova_ylim$M3,plot_data_anova_ylim$M4,plot_data_anova_ylim$M5)))
  ylim_m <- 0
  plot_data_anova$sig1 <- plot_data_anova$sig1*ylim_p
  plot_data_anova$sig2 <- plot_data_anova$sig2*ylim_p
  plot_data_anova$sig3 <- plot_data_anova$sig3*ylim_p
  plot_data_anova$sig4 <- plot_data_anova$sig4*ylim_p
  plot_data_anova$sig5 <- plot_data_anova$sig5*ylim_p
  plot_data_anova[,"p"] <- rep(0.05*(ylim_p),nrow(plot_data_anova))
  melted <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M3","M4","M5","sig1","sig2","sig4","sig5","p"))
  melted_M <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M3","M4","M5"))
  print(ggplot() +
          geom_line(data=subset(melted_M,-dt_min+dt_max<=0),aes(-dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group, size=Group)) + #,,method="gam", formula=y~s(x,k=100),se=FALSE
          geom_line(data=subset(melted_M,-dt_min+dt_max>=0),aes(-dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group, size=Group)) +
          #geom_line(data=subset(melted_M,-dt_min+dt_max==0),aes(-dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group, size=Group)) +
          #stat_smooth(aes(-dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group),se=FALSE) +
          xlab(paste0("Observation interval length around experience sampling answer in min")) +
          scale_colour_manual("Self-assessed arousal state",label= legend_name_M, values= cols_M) +
          scale_linetype_manual("Self-assessed arousal state",label= legend_name_M, values=line_type_M) +
          scale_size_manual ("Self-assessed arousal state",label= legend_name_M, values=line_size_M) +
          guides(colour=guide_legend("Self-assessed arousal state",nrow=5,ncol=2)) +
          theme_grey() +
          theme(legend.position=c(0.8,0.2), legend.direction = "vertical") +
          scale_x_continuous(expand=c(0,0),limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 5),1)) +
          # scale_y_continuous(legend_entry, limits=c(0,ylim_p), sec.axis = sec_axis(~ . / (ylim_p), name = "p-value Post-Hoc(Tukey)"))
          scale_y_continuous(expand=c(0,0),legend_entry, limits=c(ylim_m,ylim_p), breaks=round(seq(ylim_m, ylim_p, by = 1)))
  )
  ggsave(file=paste0("8.2_3",as.character(plot_name[[i]]),"_Mean.emf")) 
}




# Plot sig group mean difference to balanced over time window
plot_list <-list(simul_anova_time_dt, simul_anova_freq_dt)
plot_name <- list(deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt)))
legend_name_all <- c("Differences: Bored - balanced","Differences: Little to do - balanced","Differences: Slightly under pressure - balanced","Differences: Stressed - balanced","Sig. differences: Bored - balanced","Sig. differences: Little to do - balanced","Sig. differences: Slightly under pressure - balanced","Sig. differences: Stressed - balanced","Sig. differences: Bored - stressed","Sig. p=.05")
breaks_point_all <- c(1:10)
cols_all <- c("green","orange", "blue", "purple","green","orange","blue", "purple", "red","black")
line_type_all <- c("solid", "solid","solid","solid","solid", "solid","solid","solid","solid","dotted")
line_size_all <- c(2,2,2,2,0.5,0.5,0.5,0.5,0.5,0.5)
line_shape_all <- c(1,1,1,1,5,5,5,5,5,1)
legend_name_M <- c("Differences: Bored - balanced","Differences: Little to do - balanced","Differences: bored - stressed","Differences: Slightly under pressure - balanced","Differences: Stressed - balanced","Significance Level p=.05")
breaks_point_M <- c(1:6)
cols_M <- c(color_2,"black")
line_type_M <- c("solid", "solid","solid","solid","solid","dotted")
line_size_M <- c(1,1,1,1,1,1)
line_shape_M <- c(1,1,1,1,1,1)
legend_name <- c("Smartphone usage time","Smartphone usage frequency")
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
    legend_entry <- "Significance p"
  }else{
    legend_entry <- "Significance p"
  }
  xlim_p <- max(plot_data_anova$dt_min+plot_data_anova$dt_max)
  xlim_m <- -60
  plot_data_anova_ylim <- subset(plot_data_anova, dt_min+dt_max >=xlim_m)
  ylim_m <- floor(min(c(plot_data_anova_ylim$sig1,plot_data_anova_ylim$sig2,plot_data_anova_ylim$sig3,plot_data_anova_ylim$sig4,plot_data_anova_ylim$sig5)))
  ylim_p <- ceiling(max(c(plot_data_anova_ylim$sig1,plot_data_anova_ylim$sig2,plot_data_anova_ylim$sig3,plot_data_anova_ylim$sig4,plot_data_anova_ylim$sig5)))
  plot_data_anova$sig1 <- plot_data_anova$p1*ylim_p*p_sig_scale
  plot_data_anova$sig2 <- plot_data_anova$p2*ylim_p*p_sig_scale
  plot_data_anova$sig3 <- plot_data_anova$p3*ylim_p*p_sig_scale
  plot_data_anova$sig4 <- plot_data_anova$p4*ylim_p*p_sig_scale
  plot_data_anova$sig5 <- plot_data_anova$p5*ylim_p*p_sig_scale
  plot_data_anova[,"p"] <- rep(0.05*(ylim_p),nrow(plot_data_anova))*p_sig_scale
  melted_all <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M4","M5","sig1","sig2","sig3","sig4","sig5","p"))
  melted_M <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("sig1","sig2","sig3","sig4","sig5","p"))
  
  print(ggplot(data=melted_M) +
          geom_line(aes(-dt_min+dt_max, y=value, group=Group,colour=Group, size=Group, linetype=Group,shape=Group)) +
          xlab(paste0("Observation interval length around experience sampling answer in min")) +
          scale_colour_manual(legend_name[i],label= legend_name_M, values= cols_M) +
          scale_linetype_manual(legend_name[i],label= legend_name_M, values=line_type_M) +
          scale_shape_manual(legend_name[i],label= legend_name_M, values=line_shape_M) +
          scale_size_manual (legend_name[i],label= legend_name_M, values=line_size_M) +
          guides(colour=guide_legend(legend_name[i],nrow=3,ncol=2,title.hjust =0.0)) +
          theme_grey()+
          #theme(legend.title = element_text(face='bold'))+
          theme(legend.position="top", legend.direction = "vertical") +
          scale_x_continuous(expand=c(0,0),limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 5),1)) +
          scale_y_continuous(expand=c(0,0),legend_entry, limits=c(ylim_m,ylim_p),breaks=seq(0,1,by=0.05))
  )
  ggsave(file=paste0("8.2_3",as.character(plot_name[[i]]),"_Differences_sig.emf")) 
}

# scatterplot average daily usage time - usage freq
cor <- cor.test(sub_arousal$usage_time,sub_arousal$usage_freq)
sub_arousal <- subset(db, variable == "esm_boredom_stress")
#sub_arousal <- sub_arousal[,c("arousal","usage_time","usage_freq")]
ggplot(data=sub_arousal, aes(x=usage_time/dt/60, y=usage_freq/dt))+
  geom_point(color=blu) +
  xlab(paste0("Smartphone usage in min/h")) +
  ylab(paste0("Smartphone usage in frequency/h")) +
  #geom_point(shape=1)+
  geom_smooth(method=lm, aes(colour=paste0("Linear regression r(",cor["parameter"],")=",round(as.numeric(cor["estimate"]),2),", p<.001")),se=FALSE,size=2) +
  geom_smooth(method=loess,aes(colour="Non parametric LOESS regression"),se=FALSE,size=2) +
  scale_colour_manual(name="Regression models", values=c("black", "red"), position="top")+
  scale_x_continuous(breaks=seq(0,100,5))+
  scale_y_continuous(breaks=seq(0,100,5))+
  theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.94)) +
  ggsave(file="scatter_usage_time_usage_freq.emf") 

# # Plot group mean difference to balanced over time window (total time)
# plot_list <-list(simul_anova_time_dt, simul_anova_freq_dt)
# plot_name <- list(deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt)))
# legend_name_all <- c("Differences: Bored - balanced","Differences: Little to do - balanced","Differences: Slightly under pressure - balanced","Differences: Stressed - balanced","Sig. differences: Bored - balanced","Sig. differences: Little to do - balanced","Sig. differences: Slightly under pressure - balanced","Sig. differences: Stressed - balanced","Sig. differences: Bored - stressed","Sig. p=.05")
# breaks_point_all <- c(1:10)
# cols_all <- c("green","orange", "blue", "purple","green","orange","blue", "purple", "red","black")
# line_type_all <- c("solid", "solid","solid","solid","solid", "solid","solid","solid","solid","dotted")
# line_size_all <- c(2,2,2,2,0.5,0.5,0.5,0.5,0.5,0.5)
# line_shape_all <- c(1,1,1,1,5,5,5,5,5,1)
# legend_name_M <- c("Differences: Bored - balanced","Differences: Little to do - balanced","Differences: Slightly under pressure - balanced","Differences: Stressed - balanced")
# breaks_point_M <- c(1:4)
# cols_M <- color_2[-3]
# line_type_M <- c("solid", "solid","solid","solid")
# line_size_M <- c(1,1,1,1)
# line_shape_M <- c(1,1,1,1)
# #linetype_vector <- legend_vector
# p_sig_scale <- 1 # if 5% sig -> 20
# for (i in 1:length(plot_list)){
#   plot_data_anova <-plot_list[[i]]
#   plot_data_anova$M1 <- (plot_data_anova$M1-plot_data_anova$M3)/plot_data_anova$M3*100
#   plot_data_anova$M2 <- (plot_data_anova$M2-plot_data_anova$M3)/plot_data_anova$M3*100
#   plot_data_anova$M3 <- plot_data_anova$M3
#   plot_data_anova$M4 <- (plot_data_anova$M4-plot_data_anova$M3)/plot_data_anova$M3*100
#   plot_data_anova$M5 <- (plot_data_anova$M5-plot_data_anova$M3)/plot_data_anova$M3*100
#   if (grepl("freq",plot_name[[i]])){
#     legend_entry <- "Usage frequency differences in %"
#   }else{
#     legend_entry <- "Usage time differences in %"
#   }
#   xlim_p <- max(plot_data_anova$dt_min+plot_data_anova$dt_max)
#   xlim_m <- -60
#   plot_data_anova_ylim <- subset(plot_data_anova, dt_min+dt_max >=xlim_m)
#   ylim_m <- floor(min(c(plot_data_anova_ylim$M1,plot_data_anova_ylim$M2,plot_data_anova_ylim$M4,plot_data_anova_ylim$M5)))
#   ylim_p <- ceiling(max(c(plot_data_anova_ylim$M1,plot_data_anova_ylim$M2,plot_data_anova_ylim$M4,plot_data_anova_ylim$M5)))
#   plot_data_anova$sig1 <- plot_data_anova$p1*ylim_p*p_sig_scale
#   plot_data_anova$sig2 <- plot_data_anova$p2*ylim_p*p_sig_scale
#   plot_data_anova$sig3 <- plot_data_anova$p3*ylim_p*p_sig_scale
#   plot_data_anova$sig4 <- plot_data_anova$p4*ylim_p*p_sig_scale
#   plot_data_anova$sig5 <- plot_data_anova$p5*ylim_p*p_sig_scale
#   plot_data_anova[,"p"] <- rep(0.05*(ylim_p),nrow(plot_data_anova))*p_sig_scale
#   melted_all <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M4","M5","sig1","sig2","sig3","sig4","sig5","p"))
#   melted_M <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M4","M5"))
#   
#   print(ggplot(data=melted_M) +
#           stat_smooth(aes(-dt_min+dt_max, y=value, group=Group,colour=Group, size=Group, linetype=Group,shape=Group)) +
#           xlab(paste0("Window length centered at arousal measurement points in min")) +
#           
#           scale_colour_manual("Group",label= legend_name_M, values= cols_M) +
#           scale_linetype_manual("Group",label= legend_name_M, values=line_type_M) +
#           scale_shape_manual("Group",label= legend_name_M, values=line_shape_M) +
#           scale_size_manual ("Group",label= legend_name_M, values=line_size_M) +
#           guides(colour=guide_legend("Group",nrow=5,ncol=2)) +
#           theme(legend.position="top", legend.direction = "vertical") +
#           scale_x_continuous(limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 20),1)) +
#           scale_y_continuous(legend_entry, limits=c(ylim_m,ylim_p))
#   )
#   ggsave(file=paste0(as.character(plot_name[[i]]),"_Differences.emf")) 
# }


# 
# # Plot group median over time window (total time)
# plot_list <-list(simul_anova_time_dt, simul_anova_freq_dt)
# plot_name <- list(deparse(substitute(simul_anova_time_dt)),deparse(substitute(simul_anova_freq_dt)))
# legend_name_all <- c("Bored","Little to do","Balanced","Slightly under pressure","Stressed","Sig. differences: Bored - balanced","Sig. differences: Little to do - balanced","Sig. differences: Slightly under pressure - balanced","Sig. differences: Stressed - balanced","Sig. p=.05")
# breaks_point_al <- c(1:10)
# cols_al <- c("green","orange", "red","blue", "purple","green","orange","blue", "purple", "black")
# line_type_al <- c("solid", "solid","solid","solid","solid","dashed", "dashed","dashed","dashed","dashed")
# line_size_al <- c(1,1,1,1,1,1,1,1,1,0.5)
# legend_name_M <- c("Bored","Little to do","Balanced","Slightly under pressure","Stressed")
# breaks_point_M <- c(1:5)
# cols_M <- color_2
# line_type_M <- c("solid", "solid","solid","solid","solid")
# line_size_M <- c(1,1,2,1,1)
# legend_name <- c("Smartphone usage time","Smartphone usage frequency")
# #linetype_vector <- legend_vector
# for (i in 1:length(plot_list)){
#   plot_data_anova <-plot_list[[i]]
#   if (grepl("freq",plot_name[[i]])){
#     legend_entry <- "Median smartphone usage time in min/h"
#   }else{
#     legend_entry <- "Median smartphone usage frequency per hour"
#   }
#   xlim_p <- max(plot_data_anova$dt_min+plot_data_anova$dt_max)
#   xlim_m <- -60
#   plot_data_anova_ylim <- subset(plot_data_anova, dt_min+dt_max >=xlim_m)
#   ylim_p <- ceiling(max(c(plot_data_anova_ylim$M1,plot_data_anova_ylim$M2,plot_data_anova_ylim$M3,plot_data_anova_ylim$M4,plot_data_anova_ylim$M5)))
#   ylim_m <- 0
#   plot_data_anova$sig1 <- plot_data_anova$sig1*ylim_p
#   plot_data_anova$sig2 <- plot_data_anova$sig2*ylim_p
#   plot_data_anova$sig3 <- plot_data_anova$sig3*ylim_p
#   plot_data_anova$sig4 <- plot_data_anova$sig4*ylim_p
#   plot_data_anova$sig5 <- plot_data_anova$sig5*ylim_p
#   plot_data_anova[,"p"] <- rep(0.05*(ylim_p),nrow(plot_data_anova))
#   melted <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("M1","M2","M3","M4","M5","sig1","sig2","sig4","sig5","p"))
#   melted_M <- melt(plot_data_anova, variable.name="Group", id=c("dt_min","dt_max"),measure.vars=c("Mdn1","Mdn2","Mdn3","Mdn4","Mdn5"))
#   print(ggplot() +
#           geom_line(data=subset(melted_M,-dt_min+dt_max<=0),aes(-dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group, size=Group)) + #,,method="gam", formula=y~s(x,k=100),se=FALSE
#           geom_line(data=subset(melted_M,-dt_min+dt_max>=0),aes(-dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group, size=Group)) +
#           #geom_line(data=subset(melted_M,-dt_min+dt_max==0),aes(-dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group, size=Group)) +
#           #stat_smooth(aes(-dt_min+dt_max, y=value, group=Group,colour=Group, linetype=Group),se=FALSE) +
#           xlab(paste0("Analysed window length around experience sampling answer in min")) +
#           scale_colour_manual("Self-assessed arousal state",label= legend_name_M, values= cols_M) +
#           scale_linetype_manual("Self-assessed arousal state",label= legend_name_M, values=line_type_M) +
#           scale_size_manual ("Self-assessed arousal state",label= legend_name_M, values=line_size_M) +
#           guides(colour=guide_legend("Self-assessed arousal state",nrow=5,ncol=2)) +
#           theme_grey() +
#           theme(legend.position=c(0.8,0.2), legend.direction = "vertical") +
#           scale_x_continuous(expand=c(0,0),limits=c(xlim_m,xlim_p), breaks = round(seq(xlim_m, max(plot_data_anova$dt_min)+max(plot_data_anova$dt_max), by = 5),1)) +
#           # scale_y_continuous(legend_entry, limits=c(0,ylim_p), sec.axis = sec_axis(~ . / (ylim_p), name = "p-value Post-Hoc(Tukey)"))
#           scale_y_continuous(expand=c(0,0),legend_entry, limits=c(ylim_m,ylim_p), breaks=round(seq(ylim_m, ylim_p, by = 1)))
#   )
#   ggsave(file=paste0("8.2_3",as.character(plot_name[[i]]),"_Median.emf")) 
# }
