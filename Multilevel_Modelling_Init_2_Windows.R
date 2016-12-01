# window 2
dt_min <<- 30*60
dt_max <<- 0
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/nr_2_onoff_calc.R")
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/nr_3_arousal_calc.R")

db_2 <- db
db_2[,"window_0_1"]<- as.integer(1)
db_2[,"id"] <- db_2[,"id"]+20000

# window 1
dt_min <<- 10*60
dt_max <<- 0
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/nr_2_onoff_calc.R")
source("C:/Users/Felix/Dropbox/Apps/Aware/Database/R Scripts/nr_3_arousal_calc.R")
db_1 <- db
db_1[,"window_0_1"]<- as.integer(0)
db_1[,"id"] <- db_1[,"id"]+10000


# add together to one dataframe
db_2w <- rbind(db_1,db_2)
db_2w <- dplyr::arrange(db_2w, timestamp)
db_2w <- db_2w[order(db_2w[,"id"],db_2w[,"timestamp_end"]),]
db_2w_N= nrow(db_2w)
rownames(db_2w) <- seq(length=db_2w_N)

