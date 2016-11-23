anova_simul_time <- data.frame(F_=NA,sig=NA,dt_min=NA,dt_max=NA,M_1=NA,M_2=NA,M_3=NA,M_4=NA,M_5=NA,N_1=NA,N_2=NA,N_3=NA,N_4=NA,N_5=NA)
anova_simul_freq <- data.frame(F_=NA,sig=NA,dt_min=NA,dt_max=NA,M_1=NA,M_2=NA,M_3=NA,M_4=NA,M_5=NA,N_1=NA,N_2=NA,N_3=NA,N_4=NA,N_5=NA)
test_vector_dt_min <- seq(2,100,1)
test_vector_dt_max <- seq(2,100,1)

for (test_pos in 1:length(test_vector_dt_min)){
  print(test_pos)
  dt_min <- test_vector_dt_min[test_pos]*60
  dt_max <- test_vector_dt_max[test_pos]*60
  source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/onoff_calc.R")
  source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/arousal_calc.R")
  
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

  
  state.data <- rbind(sub_state[[1]],sub_state[[2]],sub_state[[3]], sub_state[[4]],sub_state[[5]])
  state.data$usage_time <- state.data$usage_time/60/dt
  anova <- aov(formula = usage_time ~ Group, data = state.data)
  sum_anova <- summary(anova)
  sum_describe <- describeBy(state.data$usage_time,state.data$Group)
  #print(sum_anova)
  #print(sum_describe)
  anova_simul[test_pos,"dt_min"] <-test_vector_dt_min[test_pos]
  anova_simul[test_pos,"dt_max"] <-test_vector_dt_max[test_pos]
  anova_simul[test_pos,"F_"] <-sum_anova[[1]][1,"F value"]
  anova_simul[test_pos,"sig"] <-sum_anova[[1]][1,"Pr(>F)"]
  anova_simul[test_pos,"M_1"] <-round(sum_describe[[1]]$mean,1)
  anova_simul[test_pos,"M_2"] <-round(sum_describe[[2]]$mean,1)
  anova_simul[test_pos,"M_3"] <-round(sum_describe[[3]]$mean,1)
  anova_simul[test_pos,"M_4"] <-round(sum_describe[[4]]$mean,1)
  anova_simul[test_pos,"M_5"] <-round(sum_describe[[5]]$mean,1)
  anova_simul[test_pos,"N_1"] <-round(sum_describe[[1]]$n,1)
  anova_simul[test_pos,"N_2"] <-round(sum_describe[[2]]$n,1)
  anova_simul[test_pos,"N_3"] <-round(sum_describe[[3]]$n,1)
  anova_simul[test_pos,"N_4"] <-round(sum_describe[[4]]$n,1)
  anova_simul[test_pos,"N_5"] <-round(sum_describe[[5]]$n,1)
  }