setServerLogLevel <- function(conn, logLevel=2)
{
  if(!inherits(conn, 'twsConnection'))
    stop('twsConnection object required')
  VERSION <- "1"
  con <- conn[[1]]
  writeBin(c(.twsOutgoingMSG$SET_SERVER_LOGLEVEL,
             VERSION, as.character(logLevel)), con)
}
