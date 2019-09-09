.reqCurrentTime <-
  function(conn) {
    if (!isConnected(conn)) {
      stop("peer has gone away. check your IB connection", call. = FALSE)
    }
    if (!is.twsConnection(conn)) {
      stop("requires twsConnection object")
    }

    con <- conn[[1]]

    writeBin(.twsOutgoingMSG$REQ_CURRENT_TIME, con)
    writeBin("1", con)
  }

reqCurrentTime <-
  function(twsconn) {
    .reqCurrentTime(twsconn)
    con <- twsconn[[1]]
    e_current_time <- eWrapper()
    e_current_time$currentTime <- function(curMsg, msg, timestamp, file, ...) {
      msg[2]
    }
    while (isConnected(twsconn)) {
      socketSelect(list(con), FALSE, NULL)
      curMsg <- readBin(con, character(), 1)
      currentTime <- processMsg(curMsg,
        con,
        eWrapper = e_current_time,
        twsconn = twsconn,
        timestamp = NULL, file = ""
      )
      if (curMsg == .twsIncomingMSG$CURRENT_TIME) {
        break
      }
    }
    structure(as.numeric(currentTime), class = c("POSIXt", "POSIXct"))
    #  tz <- Sys.getenv("TZ")
    #  on.exit(Sys.setenv(TZ=tz))
    #  Sys.setenv(TZ='GMT')
    #  xts:::as.POSIXct.numeric(as.numeric(currentTime),tz='GMT',origin='1970-01-01')
  }
