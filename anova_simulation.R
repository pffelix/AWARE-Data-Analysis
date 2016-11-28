library(prettyR)
theme_set(theme_grey(base_size = 16)) 
library(psych)
library(agricolae)
database_path = "C:/Users/Felix/Dropbox/Apps/Aware/Database"
date <- Sys.Date()




simul_annova <- function(vector_dt_min,vector_dt_max) {
  anova_simul_time <- data.frame(F_=NA,sig=NA,dt_min=NA,dt_max=NA,M1=NA,M2=NA,M3=NA,M4=NA,M5=NA,sig1=NA,sig2=NA,sig3=NA,sig4=NA,sig5=NA,t1=NA,t2=NA,t3=NA,t4=NA,t5=NA,p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,df1=NA,df2=NA,df3=NA,df4=NA,df5=NA,N1=NA,N2=NA,N3=NA,N4=NA,N5=NA,p_bartlett=NA,p_fligner=NA)
  anova_simul_freq <- data.frame(F_=NA,sig=NA,dt_min=NA,dt_max=NA,M1=NA,M2=NA,M3=NA,M4=NA,M5=NA,sig1=NA,sig2=NA,sig3=NA,sig4=NA,sig5=NA,t1=NA,t2=NA,t3=NA,t4=NA,t5=NA,p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,df1=NA,df2=NA,df3=NA,df4=NA,df5=NA,N1=NA,N2=NA,N3=NA,N4=NA,N5=NA,p_bartlett=NA,p_fligner=NA)
  for (test_pos in 1:length(vector_dt_min)){
    sub_state <- list()
    #print(test_pos)
    # search windows in seconds
    dt_min <<- vector_dt_min[test_pos]*60
    dt_max <<- vector_dt_max[test_pos]*60

    source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/nr_2_onoff_calc.R")
    source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/nr_3_arousal_calc.R")
    print(dt_min)
    print(dt_max)
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
    state.data$usage_freq <- state.data$usage_freq/dt
    
    p_bartlett_time <- bartlett.test(usage_time ~ Group, data = state.data)$p.value
    p_bartlett_freq <- bartlett.test(usage_freq ~ Group, data = state.data)$p.value
    p_fligner_time <- fligner.test(usage_time ~ Group, data = state.data)$p.value
    p_fligner_freq <- fligner.test(usage_freq ~ Group, data = state.data)$p.value
    
    anova_time <- aov(formula = usage_time ~ Group, data = state.data)
    anova_freq <- aov(formula = usage_freq ~ Group, data = state.data)

    sum_anova_time <- summary(anova_time)
    sum_anova_freq <- summary(anova_freq)
    sum_tukey_time <- TukeyHSD(anova_time)
    sum_tukey_freq <- TukeyHSD(anova_freq)
    
    # T-test self-assed , actual usage time / freq
    # if t.test(a,b) is independent (2 group) -> paired=FALSE (standard) + do var.test(a,b) if p<0.05 -> var.equal = FALSE (standard)
    # if t.test(a,meanb) is dependent (1 group) -> paired=TRUE
    sum_var_time <- list()
    sum_var_freq <- list()
    sum_t_time <- list()
    sum_t_freq <- list()
    
    for (ar in 1:5) {
      comp_1 <- ar
      comp_2 <- 3
      if (comp_1 == 3){
        comp_1 <- 1
        comp_2 <- 5
      }
      sum_var_time[[ar]] <-var.test(sub_state[[comp_1]]$usage_time,sub_state[[comp_2]]$usage_time)$p.value
      if (sum_var_time[ar] < 0.05){
        sum_t_time[[ar]] <- as.numeric(t.test(sub_state[[comp_1]]$usage_time,sub_state[[comp_2]]$usage_time, paired=FALSE, var.equal = FALSE)[c("statistic","parameter","p.value")])
      } else {
        sum_t_time[[ar]] <- as.numeric(t.test(sub_state[[comp_1]]$usage_time,sub_state[[comp_2]]$usage_time, paired=FALSE, var.equal = TRUE)[c("statistic","parameter","p.value")])
      }
      sum_var_freq[[ar]] <-var.test(sub_state[[comp_1]]$usage_freq,sub_state[[comp_2]]$usage_freq)$p.value
      if (sum_var_freq[ar] < 0.05){
        sum_t_freq[[ar]] <- as.numeric(t.test(sub_state[[comp_1]]$usage_freq,sub_state[[comp_2]]$usage_freq, paired=FALSE, var.equal = FALSE)[c("statistic","parameter","p.value")])
      } else {
        sum_t_freq[[ar]] <- as.numeric(t.test(sub_state[[comp_1]]$usage_freq,sub_state[[comp_2]]$usage_freq, paired=FALSE, var.equal = TRUE)[c("statistic","parameter","p.value")])
      }
    }

    sum_describe_time <- describeBy(state.data$usage_time,state.data$Group)
    sum_describe_freq <- describeBy(state.data$usage_freq,state.data$Group)
    #print(sum_anova)
    #print(sum_describe)
    print(test_pos)
    anova_simul_time[test_pos,"dt_min"] <-vector_dt_min[test_pos]
    anova_simul_time[test_pos,"dt_max"] <-vector_dt_max[test_pos]
    anova_simul_time[test_pos,"F_"] <-sum_anova_time[[1]][1,"F value"]
    anova_simul_time[test_pos,"sig"] <-sum_anova_time[[1]][1,"Pr(>F)"]
    anova_simul_time[test_pos,"M1"] <-sum_describe_time[[1]]$mean
    anova_simul_time[test_pos,"M2"] <-sum_describe_time[[2]]$mean
    anova_simul_time[test_pos,"M3"] <-sum_describe_time[[3]]$mean
    anova_simul_time[test_pos,"M4"] <-sum_describe_time[[4]]$mean
    anova_simul_time[test_pos,"M5"] <-sum_describe_time[[5]]$mean
    anova_simul_time[test_pos,"sig1"] <-sum_tukey_time[[1]][2,4]
    anova_simul_time[test_pos,"sig2"] <-sum_tukey_time[[1]][5,4]
    anova_simul_time[test_pos,"sig3"] <-sum_tukey_time[[1]][4,4]
    anova_simul_time[test_pos,"sig4"] <-sum_tukey_time[[1]][8,4]
    anova_simul_time[test_pos,"sig5"] <-sum_tukey_time[[1]][9,4]
    anova_simul_time[test_pos,"t1"] <-sum_t_time[[1]][1]
    anova_simul_time[test_pos,"t2"] <-sum_t_time[[2]][1]
    anova_simul_time[test_pos,"t3"] <-sum_t_time[[3]][1]
    anova_simul_time[test_pos,"t4"] <-sum_t_time[[4]][1]
    anova_simul_time[test_pos,"t5"] <-sum_t_time[[5]][1]
    anova_simul_time[test_pos,"p1"] <-sum_t_time[[1]][3]
    anova_simul_time[test_pos,"p2"] <-sum_t_time[[2]][3]
    anova_simul_time[test_pos,"p3"] <-sum_t_time[[3]][3]
    anova_simul_time[test_pos,"p4"] <-sum_t_time[[4]][3]
    anova_simul_time[test_pos,"p5"] <-sum_t_time[[5]][3]
    anova_simul_time[test_pos,"df1"] <-sum_t_time[[1]][2]
    anova_simul_time[test_pos,"df2"] <-sum_t_time[[2]][2]
    anova_simul_time[test_pos,"df3"] <-sum_t_time[[3]][2]
    anova_simul_time[test_pos,"df4"] <-sum_t_time[[4]][2]
    anova_simul_time[test_pos,"df5"] <-sum_t_time[[5]][2]
    anova_simul_time[test_pos,"N1"] <-sum_describe_time[[1]]$n
    anova_simul_time[test_pos,"N2"] <-sum_describe_time[[2]]$n
    anova_simul_time[test_pos,"N3"] <-sum_describe_time[[3]]$n
    anova_simul_time[test_pos,"N4"] <-sum_describe_time[[4]]$n
    anova_simul_time[test_pos,"N5"] <-sum_describe_time[[5]]$n
    anova_simul_time[test_pos,"p_bartlett"] <- p_bartlett_time
    anova_simul_time[test_pos,"p_fligner"] <- p_fligner_time
    
    
    anova_simul_freq[test_pos,"dt_min"] <-vector_dt_min[test_pos]
    anova_simul_freq[test_pos,"dt_max"] <-vector_dt_max[test_pos]
    anova_simul_freq[test_pos,"F_"] <-sum_anova_freq[[1]][1,"F value"]
    anova_simul_freq[test_pos,"sig"] <-sum_anova_freq[[1]][1,"Pr(>F)"]
    anova_simul_freq[test_pos,"M1"] <-sum_describe_freq[[1]]$mean
    anova_simul_freq[test_pos,"M2"] <-sum_describe_freq[[2]]$mean
    anova_simul_freq[test_pos,"M3"] <-sum_describe_freq[[3]]$mean
    anova_simul_freq[test_pos,"M4"] <-sum_describe_freq[[4]]$mean
    anova_simul_freq[test_pos,"M5"] <-sum_describe_freq[[5]]$mean
    anova_simul_freq[test_pos,"sig1"] <-sum_tukey_freq[[1]][2,4]
    anova_simul_freq[test_pos,"sig2"] <-sum_tukey_freq[[1]][5,4]
    anova_simul_freq[test_pos,"sig3"] <-sum_tukey_freq[[1]][4,4]
    anova_simul_freq[test_pos,"sig4"] <-sum_tukey_freq[[1]][8,4]
    anova_simul_freq[test_pos,"sig5"] <-sum_tukey_freq[[1]][9,4]
    anova_simul_freq[test_pos,"t1"] <-sum_t_freq[[1]][1]
    anova_simul_freq[test_pos,"t2"] <-sum_t_freq[[2]][1]
    anova_simul_freq[test_pos,"t3"] <-sum_t_freq[[3]][1]
    anova_simul_freq[test_pos,"t4"] <-sum_t_freq[[4]][1]
    anova_simul_freq[test_pos,"t5"] <-sum_t_freq[[5]][1]
    anova_simul_freq[test_pos,"p1"] <-sum_t_freq[[1]][3]
    anova_simul_freq[test_pos,"p2"] <-sum_t_freq[[2]][3]
    anova_simul_freq[test_pos,"p3"] <-sum_t_freq[[3]][3]
    anova_simul_freq[test_pos,"p4"] <-sum_t_freq[[4]][3]
    anova_simul_freq[test_pos,"p5"] <-sum_t_freq[[5]][3]
    anova_simul_freq[test_pos,"df1"] <-sum_t_freq[[1]][2]
    anova_simul_freq[test_pos,"df2"] <-sum_t_freq[[2]][2]
    anova_simul_freq[test_pos,"df3"] <-sum_t_freq[[3]][2]
    anova_simul_freq[test_pos,"df4"] <-sum_t_freq[[4]][2]
    anova_simul_freq[test_pos,"df5"] <-sum_t_freq[[5]][2]
    anova_simul_freq[test_pos,"N1"] <-sum_describe_freq[[1]]$n
    anova_simul_freq[test_pos,"N2"] <-sum_describe_freq[[2]]$n
    anova_simul_freq[test_pos,"N3"] <-sum_describe_freq[[3]]$n
    anova_simul_freq[test_pos,"N4"] <-sum_describe_freq[[4]]$n
    anova_simul_freq[test_pos,"N5"] <-sum_describe_freq[[5]]$n
    anova_simul_freq[test_pos,"p_bartlett"] <- p_bartlett_freq
    anova_simul_freq[test_pos,"p_fligner"] <- p_fligner_freq

    
    #print(anova_simul_freq[1,"N5"])
  }
  return(list(anova_simul_time, anova_simul_freq))
}



#vector in minutes
test_vector_dt_min <- c(seq(0.5,59.75,0.25),seq(60,120,1))
test_vector_dt_max <- c(seq(0.5,59.75,0.25),seq(60,120,1))
simul_anova_list_dt <- simul_annova(test_vector_dt_min,test_vector_dt_max)
simul_anova_time_dt <- simul_anova_list_dt[[1]]
simul_anova_freq_dt <- simul_anova_list_dt[[2]]


# vector in minutes
test_vector_dt_min <- c(seq(1,119.50,0.50),seq(120,240,2))
test_vector_dt_max <- rep(0,299)
#test_vector_dt_min <- seq(1,400,10)
#test_vector_dt_max <- rep(0,40)
simul_anova_list_dt_min <- simul_annova(test_vector_dt_min,test_vector_dt_max)
simul_anova_time_dt_min <- simul_anova_list_dt_min[[1]]
simul_anova_freq_dt_min <- simul_anova_list_dt_min[[2]]

# vector in minutesvision

test_vector_dt_min <- rep(0,299)
test_vector_dt_max <- c(seq(1,119.50,0.50),seq(120,240,2))
#test_vector_dt_min <- rep(0,40)
#test_vector_dt_max <- seq(1,400,10)
simul_anova_list_dt_max <- simul_annova(test_vector_dt_min,test_vector_dt_max)
simul_anova_time_dt_max <- simul_anova_list_dt_max[[1]]
simul_anova_freq_dt_max <- simul_anova_list_dt_max[[2]]

simul_anova <- list(simul_anova_time_dt,simul_anova_freq_dt,simul_anova_time_dt_min,simul_anova_freq_dt_min,simul_anova_time_dt_max,simul_anova_freq_dt_max)
save(simul_anova, file = paste0(database_path, "/Backup_Workspace/simul_anova_init_full", date, ".RData"))
#load(file = paste0(database_path, "/Backup_Workspace/", "simul_anova2016-11-27_init", ".RData"))