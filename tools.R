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
  