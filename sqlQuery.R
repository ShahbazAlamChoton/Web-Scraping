sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- dbConnect(MySQL(), user="____", password="____",
                  dbname="____", host="____") 
  # close db connection after function call exits
  on.exit(dbDisconnect(DB))
  # send Query to obtain result set
  rs <- dbSendQuery(DB, query)
  # get elements from result sets and convert to dataframe
  result <- fetch(rs, -1)
  # return the dataframe
  return(result)
}
