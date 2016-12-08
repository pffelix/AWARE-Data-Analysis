library(prettyR)
library(psych)
library(agricolae)
library(clinfun)
library(FSA)
database_path = "C:/Users/Felix/Dropbox/Apps/Aware/Database"
date <- Sys.Date()




simul_annova <- function(vector_dt_min,vector_dt_max) {
  #anova_simul_time <- data.frame(F_=NA,sig=NA,dt_min=NA,dt_max=NA,M1=NA,M2=NA,M3=NA,M4=NA,M5=NA,sig1=NA,sig2=NA,sig3=NA,sig4=NA,sig5=NA,t1=NA,t2=NA,t3=NA,t4=NA,t5=NA,p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,df1=NA,df2=NA,df3=NA,df4=NA,df5=NA,N1=NA,N2=NA,N3=NA,N4=NA,N5=NA,p_bartlett=NA,p_fligner=NA)
  #anova_simul_freq <- data.frame(F_=NA,sig=NA,dt_min=NA,dt_max=NA,M1=NA,M2=NA,M3=NA,M4=NA,M5=NA,sig1=NA,sig2=NA,sig3=NA,sig4=NA,sig5=NA,t1=NA,t2=NA,t3=NA,t4=NA,t5=NA,p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,df1=NA,df2=NA,df3=NA,df4=NA,df5=NA,N1=NA,N2=NA,N3=NA,N4=NA,N5=NA,p_bartlett=NA,p_fligner=NA)
  anova_simul_time <- data.frame(H_=NA,sig=NA,dt_min=NA,dt_max=NA,M1=NA,M2=NA,M3=NA,M4=NA,M5=NA,sig1=NA,sig2=NA,sig3=NA,sig4=NA,sig5=NA,U1=NA,U2=NA,U3=NA,U4=NA,U5=NA,Z1=NA,Z2=NA,Z3=NA,Z4=NA,Z5=NA,p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,r1=NA,r2=NA,r3=NA,r4=NA,r5=NA,df1=NA,df2=NA,df3=NA,df4=NA,df5=NA,N1=NA,N2=NA,N3=NA,N4=NA,N5=NA,JT=NA,JT_p=NA)
  anova_simul_freq <- data.frame(H_=NA,sig=NA,dt_min=NA,dt_max=NA,M1=NA,M2=NA,M3=NA,M4=NA,M5=NA,sig1=NA,sig2=NA,sig3=NA,sig4=NA,sig5=NA,U1=NA,U2=NA,U3=NA,U4=NA,U5=NA,Z1=NA,Z2=NA,Z3=NA,Z4=NA,Z5=NA,p1=NA,p2=NA,p3=NA,p4=NA,p5=NA,r1=NA,r2=NA,r3=NA,r4=NA,r5=NA,df1=NA,df2=NA,df3=NA,df4=NA,df5=NA,N1=NA,N2=NA,N3=NA,N4=NA,N5=NA,JT=NA,JT_p=NA)
  source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/tools.R")
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
    
    #anova_time <- aov(formula = usage_time ~ Group, data = state.data)
    #anova_freq <- aov(formula = usage_freq ~ Group, data = state.data)

    #sum_anova_time <- summary(anova_time)
    #sum_anova_freq <- summary(anova_freq)
    #sum_tukey_time <- TukeyHSD(anova_time)
    #sum_tukey_freq <- TukeyHSD(anova_freq)
    kruskal_time <- kruskal.test(formula = usage_time ~ Group, data = state.data)
    kruskal_freq <- kruskal.test(formula = usage_freq ~ Group, data = state.data)
    
    sum_dun_time <- dunnTest(usage_time ~ Group, data = state.data,method="bh") # or non
    sum_dun_freq <- dunnTest(usage_freq ~ Group, data = state.data,method="bh") # or non
    jonckheere_time <- jonckheere.test(state.data$usage_time,state.data$arousal) # post hoc trend
    jonckheere_freq <- jonckheere.test(state.data$usage_freq,state.data$arousal) # post hoc trend
    #wilcox_time <- pairwise.wilcox.test(data_kruskal$usage_time,data_kruskal$arousal, p.adjust.method="none")
    #wilcox_freq <- pairwise.wilcox.test(data_kruskal$usage_freq,data_kruskal$arousal, p.adjust.method="none")
    
    # T-test self-assed , actual usage time / freq
    # if t.test(a,b) is independent (2 group) -> paired=FALSE (standard) + do var.test(a,b) if p<0.05 -> var.equal = FALSE (standard)
    # if t.test(a,meanb) is dependent (1 group) -> paired=TRUE
    # sum_var_time <- list()
    # sum_var_freq <- list()
    # sum_t_time <- list()
    # sum_t_freq <- list()
    
    # for (ar in 1:5) {
    #   comp_1 <- ar
    #   comp_2 <- 3
    #   if (comp_1 == 3){
    #     comp_1 <- 1
    #     comp_2 <- 5
    #   }
    #   sum_var_time[[ar]] <-var.test(sub_state[[comp_1]]$usage_time,sub_state[[comp_2]]$usage_time)$p.value
    #   if (sum_var_time[ar] < 0.05){
    #     sum_t_time[[ar]] <- as.numeric(t.test(sub_state[[comp_1]]$usage_time,sub_state[[comp_2]]$usage_time, paired=FALSE, var.equal = FALSE)[c("statistic","parameter","p.value")])
    #   } else {
    #     sum_t_time[[ar]] <- as.numeric(t.test(sub_state[[comp_1]]$usage_time,sub_state[[comp_2]]$usage_time, paired=FALSE, var.equal = TRUE)[c("statistic","parameter","p.value")])
    #   }
    #   sum_var_freq[[ar]] <-var.test(sub_state[[comp_1]]$usage_freq,sub_state[[comp_2]]$usage_freq)$p.value
    #   if (sum_var_freq[ar] < 0.05){
    #     sum_t_freq[[ar]] <- as.numeric(t.test(sub_state[[comp_1]]$usage_freq,sub_state[[comp_2]]$usage_freq, paired=FALSE, var.equal = FALSE)[c("statistic","parameter","p.value")])
    #   } else {
    #     sum_t_freq[[ar]] <- as.numeric(t.test(sub_state[[comp_1]]$usage_freq,sub_state[[comp_2]]$usage_freq, paired=FALSE, var.equal = TRUE)[c("statistic","parameter","p.value")])
    #   }
    # }
    
    sum_mwu_time <- list()
    sum_mwu_freq <- list()
    for (ar in 1:5) {
      comp_1 <- ar
      comp_2 <- 3
      if (comp_1 == 3){
        comp_1 <- 1
        comp_2 <- 5
      }

      sum_mwu_time[[ar]] <- mwu_test_speed(sub_state[[comp_1]]$usage_time,sub_state[[comp_2]]$usage_time)
      sum_mwu_freq[[ar]] <- mwu_test_speed(sub_state[[comp_1]]$usage_freq,sub_state[[comp_2]]$usage_freq)

    }

    sum_describe_time <- describeBy(state.data$usage_time,state.data$Group)
    sum_describe_freq <- describeBy(state.data$usage_freq,state.data$Group)
    #print(sum_anova)
    #print(sum_describe)
    print(test_pos)
    anova_simul_time[test_pos,"dt_min"] <-vector_dt_min[test_pos]
    anova_simul_time[test_pos,"dt_max"] <-vector_dt_max[test_pos]
    anova_simul_time[test_pos,"H_"] <-as.numeric(kruskal_time$statistic)
    anova_simul_time[test_pos,"sig"] <-kruskal_time$p.value
    anova_simul_time[test_pos,"M1"] <-sum_describe_time[[1]]$mean
    anova_simul_time[test_pos,"M2"] <-sum_describe_time[[2]]$mean
    anova_simul_time[test_pos,"M3"] <-sum_describe_time[[3]]$mean
    anova_simul_time[test_pos,"M4"] <-sum_describe_time[[4]]$mean
    anova_simul_time[test_pos,"M5"] <-sum_describe_time[[5]]$mean
    anova_simul_time[test_pos,"sig1"] <-sum_dun_time[[2]]$P.adj[2]
    anova_simul_time[test_pos,"sig2"] <-sum_dun_time[[2]]$P.adj[3]
    anova_simul_time[test_pos,"sig3"] <-sum_dun_time[[2]]$P.adj[7]
    anova_simul_time[test_pos,"sig4"] <-sum_dun_time[[2]]$P.adj[6]
    anova_simul_time[test_pos,"sig5"] <-sum_dun_time[[2]]$P.adj[9]
    anova_simul_time[test_pos,"U1"] <-sum_mwu_time[[1]]$U
    anova_simul_time[test_pos,"U2"] <-sum_mwu_time[[2]]$U
    anova_simul_time[test_pos,"U3"] <-sum_mwu_time[[3]]$U
    anova_simul_time[test_pos,"U4"] <-sum_mwu_time[[4]]$U
    anova_simul_time[test_pos,"U5"] <-sum_mwu_time[[5]]$U
    anova_simul_time[test_pos,"Z1"] <-sum_mwu_time[[1]]$Z
    anova_simul_time[test_pos,"Z2"] <-sum_mwu_time[[2]]$Z
    anova_simul_time[test_pos,"Z3"] <-sum_mwu_time[[3]]$Z
    anova_simul_time[test_pos,"Z4"] <-sum_mwu_time[[4]]$Z
    anova_simul_time[test_pos,"Z5"] <-sum_mwu_time[[5]]$Z
    anova_simul_time[test_pos,"p1"] <-sum_mwu_time[[1]]$p
    anova_simul_time[test_pos,"p2"] <-sum_mwu_time[[2]]$p
    anova_simul_time[test_pos,"p3"] <-sum_mwu_time[[3]]$p
    anova_simul_time[test_pos,"p4"] <-sum_mwu_time[[4]]$p
    anova_simul_time[test_pos,"p5"] <-sum_mwu_time[[5]]$p
    anova_simul_time[test_pos,"r1"] <-sum_mwu_time[[1]]$r
    anova_simul_time[test_pos,"r2"] <-sum_mwu_time[[2]]$r
    anova_simul_time[test_pos,"r3"] <-sum_mwu_time[[3]]$r
    anova_simul_time[test_pos,"r4"] <-sum_mwu_time[[4]]$r
    anova_simul_time[test_pos,"r5"] <-sum_mwu_time[[5]]$r
    anova_simul_time[test_pos,"df1"] <-sum_mwu_time[[1]]$df
    anova_simul_time[test_pos,"df2"] <-sum_mwu_time[[2]]$df
    anova_simul_time[test_pos,"df3"] <-sum_mwu_time[[3]]$df
    anova_simul_time[test_pos,"df4"] <-sum_mwu_time[[4]]$df
    anova_simul_time[test_pos,"df5"] <-sum_mwu_time[[5]]$df
    anova_simul_time[test_pos,"N1"] <-sum_describe_time[[1]]$n
    anova_simul_time[test_pos,"N2"] <-sum_describe_time[[2]]$n
    anova_simul_time[test_pos,"N3"] <-sum_describe_time[[3]]$n
    anova_simul_time[test_pos,"N4"] <-sum_describe_time[[4]]$n
    anova_simul_time[test_pos,"N5"] <-sum_describe_time[[5]]$n
    anova_simul_time[test_pos,"JT"] <- as.numeric(jonckheere_time$statistic)
    anova_simul_time[test_pos,"JT_p"] <- jonckheere_time$p.value
    
    anova_simul_freq[test_pos,"dt_min"] <-vector_dt_min[test_pos]
    anova_simul_freq[test_pos,"dt_max"] <-vector_dt_max[test_pos]
    anova_simul_freq[test_pos,"H_"] <-as.numeric(kruskal_freq$statistic)
    anova_simul_freq[test_pos,"sig"] <-kruskal_freq$p.value
    anova_simul_freq[test_pos,"M1"] <-sum_describe_freq[[1]]$mean
    anova_simul_freq[test_pos,"M2"] <-sum_describe_freq[[2]]$mean
    anova_simul_freq[test_pos,"M3"] <-sum_describe_freq[[3]]$mean
    anova_simul_freq[test_pos,"M4"] <-sum_describe_freq[[4]]$mean
    anova_simul_freq[test_pos,"M5"] <-sum_describe_freq[[5]]$mean
    anova_simul_freq[test_pos,"sig1"] <-sum_dun_freq[[2]]$P.adj[2]
    anova_simul_freq[test_pos,"sig2"] <-sum_dun_freq[[2]]$P.adj[3]
    anova_simul_freq[test_pos,"sig3"] <-sum_dun_freq[[2]]$P.adj[7]
    anova_simul_freq[test_pos,"sig4"] <-sum_dun_freq[[2]]$P.adj[6]
    anova_simul_freq[test_pos,"sig5"] <-sum_dun_freq[[2]]$P.adj[9]
    anova_simul_freq[test_pos,"U1"] <-sum_mwu_freq[[1]]$U
    anova_simul_freq[test_pos,"U2"] <-sum_mwu_freq[[2]]$U
    anova_simul_freq[test_pos,"U3"] <-sum_mwu_freq[[3]]$U
    anova_simul_freq[test_pos,"U4"] <-sum_mwu_freq[[4]]$U
    anova_simul_freq[test_pos,"U5"] <-sum_mwu_freq[[5]]$U
    anova_simul_freq[test_pos,"Z1"] <-sum_mwu_freq[[1]]$Z
    anova_simul_freq[test_pos,"Z2"] <-sum_mwu_freq[[2]]$Z
    anova_simul_freq[test_pos,"Z3"] <-sum_mwu_freq[[3]]$Z
    anova_simul_freq[test_pos,"Z4"] <-sum_mwu_freq[[4]]$Z
    anova_simul_freq[test_pos,"Z5"] <-sum_mwu_freq[[5]]$Z
    anova_simul_freq[test_pos,"p1"] <-sum_mwu_freq[[1]]$p
    anova_simul_freq[test_pos,"p2"] <-sum_mwu_freq[[2]]$p
    anova_simul_freq[test_pos,"p3"] <-sum_mwu_freq[[3]]$p
    anova_simul_freq[test_pos,"p4"] <-sum_mwu_freq[[4]]$p
    anova_simul_freq[test_pos,"p5"] <-sum_mwu_freq[[5]]$p
    anova_simul_freq[test_pos,"r1"] <-sum_mwu_freq[[1]]$r
    anova_simul_freq[test_pos,"r2"] <-sum_mwu_freq[[2]]$r
    anova_simul_freq[test_pos,"r3"] <-sum_mwu_freq[[3]]$r
    anova_simul_freq[test_pos,"r4"] <-sum_mwu_freq[[4]]$r
    anova_simul_freq[test_pos,"r5"] <-sum_mwu_freq[[5]]$r
    anova_simul_freq[test_pos,"df1"] <-sum_mwu_freq[[1]]$df
    anova_simul_freq[test_pos,"df2"] <-sum_mwu_freq[[2]]$df
    anova_simul_freq[test_pos,"df3"] <-sum_mwu_freq[[3]]$df
    anova_simul_freq[test_pos,"df4"] <-sum_mwu_freq[[4]]$df
    anova_simul_freq[test_pos,"df5"] <-sum_mwu_freq[[5]]$df
    anova_simul_freq[test_pos,"N1"] <-sum_describe_freq[[1]]$n
    anova_simul_freq[test_pos,"N2"] <-sum_describe_freq[[2]]$n
    anova_simul_freq[test_pos,"N3"] <-sum_describe_freq[[3]]$n
    anova_simul_freq[test_pos,"N4"] <-sum_describe_freq[[4]]$n
    anova_simul_freq[test_pos,"N5"] <-sum_describe_freq[[5]]$n
    anova_simul_freq[test_pos,"JT"] <- as.numeric(jonckheere_freq$statistic)
    anova_simul_freq[test_pos,"JT_p"] <- jonckheere_freq$p.value

    
    print(anova_simul_freq[1,"N5"])
  }
  return(list(anova_simul_time, anova_simul_freq))
}


#vector in minutes
test_vector_dt_min <- c(seq(60,0.25,-0.25),rep(0,240))
test_vector_dt_max <- c(rep(0,240),seq(0.25,60,0.25))
simul_anova_list_dt <- simul_annova(test_vector_dt_min,test_vector_dt_max)
simul_anova_time_dt <- simul_anova_list_dt[[1]]
simul_anova_freq_dt <- simul_anova_list_dt[[2]]

# #vector in minutes
# test_vector_dt_min <- c(seq(0.5,59.75,0.25),seq(60,120,1))
# test_vector_dt_max <- c(seq(0.5,59.75,0.25),seq(60,120,1))
# simul_anova_list_dt <- simul_annova(test_vector_dt_min,test_vector_dt_max)
# simul_anova_time_dt <- simul_anova_list_dt[[1]]
# simul_anova_freq_dt <- simul_anova_list_dt[[2]]
# 
# 
# # vector in minutes
# test_vector_dt_min <- c(seq(1,119.50,0.50),seq(120,240,2))
# test_vector_dt_max <- rep(0,299)
# #test_vector_dt_min <- seq(1,400,10)
# #test_vector_dt_max <- rep(0,40)
# simul_anova_list_dt_min <- simul_annova(test_vector_dt_min,test_vector_dt_max)
# simul_anova_time_dt_min <- simul_anova_list_dt_min[[1]]
# simul_anova_freq_dt_min <- simul_anova_list_dt_min[[2]]
# 
# # vector in minutes
# 
# test_vector_dt_min <- rep(0,299)
# test_vector_dt_max <- c(seq(1,119.50,0.50),seq(120,240,2))
# #test_vector_dt_min <- rep(0,40)
# #test_vector_dt_max <- seq(1,400,10)
# simul_anova_list_dt_max <- simul_annova(test_vector_dt_min,test_vector_dt_max)
# simul_anova_time_dt_max <- simul_anova_list_dt_max[[1]]
# simul_anova_freq_dt_max <- simul_anova_list_dt_max[[2]]

simul_anova <- list(simul_anova_time_dt,simul_anova_freq_dt)
save(simul_anova, file = paste0(database_path, "/Backup_Workspace/simul_kruskal_init_full_2", date, ".RData"))
#load(file = paste0(database_path, "/Backup_Workspace/", "simul_kruskal2016-11-27_init", ".RData"))
simul_anova_time_dt <- simul_anova[[1]]
simul_anova_freq_dt <- simul_anova[[2]]
