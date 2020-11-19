# This section must be activated once 
# during the first execution.
{
	#-- a disguised connection information
	dbdriver <- dbDriver("PostgreSQL")
	host  <- "****"
	port  <- "****"
	user  <- "****"
	password <- "****"
	dbname <- "****"
}


#-- SQL connection
con <- dbConnect(dbdriver, dbname = dbname, host = host, port = port, user = user, password = password) #

