library(psych)
library(prettyR)



item <- c("week_stress","week_boredom","feedback_overall","feedback_app","feedback_questions","feedback_esm_identify","feedback_esm_frequency","feedback_esm_reliability","feedback_esm_intuitive","feedback_monitoring","feedback_reactivity","feedback_extra","feedback_continue")
for(pos in 1:length(item)) {
  sub <- subset(db, variable == item[pos])
  print(item[pos])
  print(describe(as.numeric(as.character(sub$value))))
  print(freq(as.character(sub$value)))
}