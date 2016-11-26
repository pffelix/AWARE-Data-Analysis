database_path = "C:/Users/Felix/Dropbox/Apps/Aware/Database"
date <- Sys.Date()

db_sql = dbConnect(MySQL(), user=user_string, password=password_string, dbname=dbname_string, host=host_string)

# Backup full database, run after Init.R

# Backup database als R dataframe
for (i in 1:length(tables_database)) {
  db_full[[i]] <- dbReadTable(conn = db_sql, name = tables_database[[i]])
  
}
names(db_full) <- tables_database
save(db_full, file = paste0(database_path, "/Backup_Workspace/db_full", date, ".RData"))

# Backup formatted database als R dataframe
save(db, file = paste0(database_path, "/Backup_Workspace/db", date, ".RData"))

# Backup whole workspace
save.image(file = paste0(database_path, "/Backup_Workspace/workspace", date, ".RData"))

#Disconnect from Aware Remote Database
cons <- dbListConnections(MySQL())
for(con in cons) {
  dbDisconnect(con)
}

# Backup database as MYSQL database
command <- shQuote(paste0(database_path, "/mysqldump.exe -A --user=", user_string, " --single-transaction --host=", host_string, " --password=", password_string, " --port=", port_string, " > ", database_path, "/Backup_MySQL/MySQL", date, ".sql"), type = "cmd" )
shell( command )

