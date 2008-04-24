`reqCurrentTime` <-
function(con) {
  con <- con[[1]]
  readBin(con,character(),100) # flush any residual TWS messages

  writeBin(.twsOutgoingMSG$REQ_CURRENT_TIME,con)
  writeBin('1',con)

  # removes the need to Sys.sleep
  waiting <- TRUE
  response <- character(0)

  while(waiting) {
    curChar <- readBin(con,character(),1)
    if(waiting & identical(curChar,character(0))) next
    waiting <- FALSE
    
    if(curChar==.twsIncomingMSG$CURRENT_TIME) {
      currentTime <- readBin(con,character(),2)[2]
      break
    }

  }
  tz <- Sys.getenv("TZ")
  on.exit(Sys.setenv(TZ=tz))
  Sys.setenv(TZ='GMT')
  xts:::as.POSIXct.numeric(as.numeric(currentTime),tz='GMT',origin='1970-01-01')
}

