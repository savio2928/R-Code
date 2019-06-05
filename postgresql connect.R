
# ----------- connect to PostgreSQL ----------------
if(!require("RPostgreSQL")){ # check for package existence 
  install.packages("RPostgreSQL")
}
library("RPostgreSQL")
#save password
pw = 123
# load PostgreSQL driver
drv = dbDriver("PostgreSQL") 
# create a connection

con = dbConnect(drv, dbname = "consumer complaints", host = "127.0.0.1", port = 5434, 
                user = "postgres", password = )


# ---------------- read and write data to the database ----------------
a=dbListConnections (PostgreSQL ())
# get the data from postgreSQL
data = dbGetQuery(con, "select * from consumer_complaints")
dbSendQuery(con, "CREATE TABLE console_dates2 (
           platform_name char(120),
           first_retail_availability date,
           discontinued date,
           units_sold_mill float8,
           platform_comment varchar(120)    
);
           ")
head(data)
tail(data)

