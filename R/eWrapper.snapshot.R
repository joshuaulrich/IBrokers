eWrapper.snapshot <- function() {
  eW <- eWrapper(NULL)
  eW$assign.Data("EOD", FALSE)
  sapply(c("bidSize","bidPrice",
           "askPrice","askSize",
           "lastPrice",
           "Open","High","Low","Close",
           "lastSize","Volume","lastTimeStamp"), function(X) eW$assign.Data(X, NA))
  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...)
  {
    tickType = msg[3]
    if(tickType == .twsTickType$BID) {
      eW$assign.Data("bidPrice", as.numeric(msg[4]))
      eW$assign.Data("bidSize" , as.numeric(msg[5])) 
    } else
    if(tickType == .twsTickType$ASK) {
      eW$assign.Data("askPrice", as.numeric(msg[4]))
      eW$assign.Data("askSize" , as.numeric(msg[5])) 
    } else
    if(tickType == .twsTickType$LAST) {
      eW$assign.Data("lastPrice", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$OPEN) {
      eW$assign.Data("Open", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$HIGH) {
      eW$assign.Data("High", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$LOW) {
      eW$assign.Data("Low", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$CLOSE) {
      eW$assign.Data("Close", as.numeric(msg[4]))
    }
  }
  eW$tickSize <- function(curMsg, msg, timestamp, file, ...)
  {
    tickType <- msg[3]
    if(tickType == .twsTickType$LAST_SIZE) {
      eW$assign.Data("lastSize", as.numeric(msg[4]))
    } else
    if(tickType == .twsTickType$VOLUME) {
      eW$assign.Data("Volume", as.numeric(msg[4]))
    }
  }
  eW$tickString <- function(curMsg, msg, timestamp, file, ...)
  {
    tickType <- msg[3]
    eW$assign.Data("lastTimeStamp", structure(as.numeric(msg[4]),
                                        class=c("POSIXt","POSIXct")))
  }
  eW$tickSnapshotEnd <- function(curMsg, msg, timestamp, file, ...)
  {
    eW$assign.Data("EOD", TRUE)
  }
  return(eW)
}
