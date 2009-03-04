`reqCurrentTime` <-
function(conn) {
  if(!inherits(conn,'twsConnection'))
    stop('requires twsConnection object')

  con <- conn[[1]]

  writeBin(.twsOutgoingMSG$REQ_CURRENT_TIME,con)
  writeBin('1',con)

  e_current_time <- eWrapper()
  e_current_time$currentTime <- function(curMsg, msg, timestamp, file, ...) { msg[2] }
  while (TRUE) {
    curMsg <- readBin(con, character(), 1)
    if (length(curMsg) < 1) 
      next
    currentTime <- processMsg(curMsg,
                              con,
                              eWrapper=e_current_time,
                              timestamp=NULL, file="")
    if(curMsg == .twsIncomingMSG$CURRENT_TIME)
      break
  }

  tz <- Sys.getenv("TZ")
  on.exit(Sys.setenv(TZ=tz))
  Sys.setenv(TZ='GMT')
  xts:::as.POSIXct.numeric(as.numeric(currentTime),tz='GMT',origin='1970-01-01')
}

