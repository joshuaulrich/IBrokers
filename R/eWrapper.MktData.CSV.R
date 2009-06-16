eWrapper.MktData.CSV <- function() {
  # internally updated data
#  .data. <- character(8)
#  
#  get.data <- function() return(.data.)
#
  eW <- eWrapper(NULL)  # use basic template
  eW$assign.Data("data", rep(NA, 8))

  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) 
  {
    tickType = msg[3]
    data <- eW$get.Data("data")
    
    data[1] <- timestamp
    if(tickType == .twsTickType$BID) {
      cat(paste(timestamp,
                msg[5], #bidSize
                msg[4], #bidPrice
                "",     #askPrice
                "",     #askSize
                "",     #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[2:3] <- msg[5:4]
    } else
    if(tickType == .twsTickType$ASK) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                msg[4], #askPrice
                msg[5], #askSize
                "",     #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[4:5] <- msg[4:5]
    } else
    if(tickType == .twsTickType$LAST) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                "",     #askPrice
                "",     #askSize
                msg[4], #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[6] <- msg[4]
    }
    eW$assign.Data("data", data)
  }
  eW$tickSize  <- function(curMsg, msg, timestamp, file, ...) 
  { 
    data <- eW$get.Data("data")
    tickType = msg[3]
    data[1] <- timestamp
    if(tickType == .twsTickType$BID_SIZE) {
      cat(paste(timestamp,
                msg[4], #bidSize
                "",     #bidPrice
                "",     #askPrice
                "",     #askSize
                "",     #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[2] <- msg[4]
    } else
    if(tickType == .twsTickType$ASK_SIZE) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                "",     #askPrice
                msg[4], #askSize
                "",     #lastPrice
                "",     #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[5] <- msg[4]
    } else 
    if(tickType == .twsTickType$LAST_SIZE) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                "",     #askPrice
                "",     #askSize
                "",     #lastPrice
                msg[4], #lastSize
                "",     #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[7] <- msg[4]
    } else
    if(tickType == .twsTickType$VOLUME) {
      cat(paste(timestamp,
                "",     #bidSize
                "",     #bidPrice
                "",     #askPrice
                "",     #askSize
                "",     #lastPrice
                "",     #lastSize
                msg[4], #Volume
                sep=","), "\n", file=file, append=TRUE)
      data[8] <- msg[4]
    }
    eW$assign.Data("data", data)
  }

  return(eW)
}
