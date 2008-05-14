`reqCurrentTime` <-
function(conn) {
  if(!inherits(conn,'twsConnection'))
    stop('requires twsConnection object')

  con <- conn[[1]]

  writeBin(.twsOutgoingMSG$REQ_CURRENT_TIME,con)
  writeBin('1',con)

  waiting <- TRUE
  response <- character(0)

  while(waiting) {
    # suppressWarnings to handle readBin issues in Windows
    # not returning character(0) when it should
    curChar <- suppressWarnings(readBin(con,character(),1))
    
    if(length(curChar) > 0) {
      if(curChar==.twsIncomingMSG$CURRENT_TIME) {
        currentTime <- readBin(con,character(),2)[2]
        waiting <- FALSE
      }
    }
  }
  tz <- Sys.getenv("TZ")
  on.exit(Sys.setenv(TZ=tz))
  Sys.setenv(TZ='GMT')
  xts:::as.POSIXct.numeric(as.numeric(currentTime),tz='GMT',origin='1970-01-01')
}

