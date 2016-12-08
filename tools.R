library(coin)
t_test <- function(vector1,vector2){
    
  sum_var <-var.test(vector1,vector2)$p.value
  if (sum_var < 0.05){
    warning("Variances not equal")
    #sum_t <- as.numeric(t.test(vector1,vector2, paired=FALSE, var.equal = FALSE)[c("statistic","parameter","p.value")])
    sum_t <- t.test(vector1,vector2, paired=FALSE, var.equal = FALSE)
    
    } else {
      warning("Variances equal")
    #sum_t <- as.numeric(t.test(vector1,vector2, paired=FALSE, var.equal = TRUE)[c("statistic","parameter","p.value")])
    sum_t <- t.test(vector1,vector2, paired=FALSE, var.equal = TRUE)
    
    }
    return(sum_t)
}


aov_test <- function(data_aov,indep_aov,dep_aov){
  #homoskedasticity (ie test for homogeneity of variances)  
  bartlett_sum <- bartlett.test(dep_aov ~ indep_aov, data = data_aov)
  p_bartlett <- bartlett_sum$p.value
  df_bartlett <- as.numeric(temp$bartlett_sum)
  k_bartlett <-  as.numeric(temp$bartlett_sum)
  
  if(p_bartlett <=0.05){
    warning("bartlett variances inhomogenous")
  }
  fligner_sum <- fligner.test(dep_aov ~ indep_aov, data = data_aov)
  p_fligner <- fligner_sum$p.value
  df_fligner <- as.numeric(temp$fligner_sum)
  chi_fligner <-  as.numeric(temp$fligner_sum)

  if(df_fligner <=0.05){
    warning("fligner variances inhomogenous")
  }
  
  aov_sum <- aov(dep_aov ~ indep_aov, data = data_aov)
  return(aov_sum)
  
}

mwu_test <- function(GroupA_wilcox_U,GroupB_wilcox_U){
  wilcox_standard <- wilcox.test(GroupA_wilcox_U,GroupB_wilcox_U) # independent paired=FALSE standard - gnu r standard
  print(wilcox_standard)
  p <- wilcox_standard$p.value
  g <- factor(c(rep("GroupA_wilcox_U", length(GroupA_wilcox_U)), rep("GroupB_wilcox_U", length(GroupB_wilcox_U))))
  v <- c(GroupA_wilcox_U, GroupB_wilcox_U)
  wilcox <- wilcox_test(v ~ g, distribution="exact")
  print(wilcox) # coin library
  U <- wilcox_standard$statistic
  print(paste0("U=",U))
  Z <- wilcox@statistic@teststatistic
  r <- wilcox@statistic@teststatistic/sqrt(length(c(GroupA_wilcox_U,GroupB_wilcox_U)))
  print(paste0("r=", r))
  df <- length(GroupA_wilcox_U)+length(GroupB_wilcox_U)-2
  print(paste0("df=", df))
  
  
  return(data.frame("U"=U,"Z"=Z,"p"=p,"r"=r,"df"=df))
}

mwu_test_speed <- function(GroupA_wilcox_U,GroupB_wilcox_U){
  wilcox_standard <- wilcox.test(GroupA_wilcox_U,GroupB_wilcox_U) # independent paired=FALSE standard - gnu r standard
  #print(wilcox_standard)
  p <- wilcox_standard$p.value
  #g <- factor(c(rep("GroupA_wilcox_U", length(GroupA_wilcox_U)), rep("GroupB_wilcox_U", length(GroupB_wilcox_U))))
  #v <- c(GroupA_wilcox_U, GroupB_wilcox_U)
  #wilcox <- wilcox_test(v ~ g, distribution="exact")
  #print(wilcox) # coin library
  U <- wilcox_standard$statistic
  #print(paste0("U=",U))
  Z <- NA
  r <- NA
  #print(paste0("r=", r))
  df <- length(GroupA_wilcox_U)+length(GroupB_wilcox_U)-2
  #print(paste0("df=", df))
  
  
  return(data.frame("U"=U,"Z"=Z,"p"=p,"r"=r,"df"=df))
}

