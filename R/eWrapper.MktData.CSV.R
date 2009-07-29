eWrapper.RealTimeBars.CSV <- function() {
  eW <- eWrapper(NULL)

  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...)
  {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    cat(paste(msg[3],  # timestamp in POSIXct
              msg[4],  # Open
              msg[5],  # High
              msg[6],  # Low
              msg[7],  # Close
              msg[8],  # Volume
              msg[9],  # WAP
              msg[10], # Count
              sep=","), "\n", file=file, append=TRUE)
    data[[id]][1:8] <- as.numeric(msg[3:10])
    eW$assign.Data("data", data)
  }
  return(eW)
}

eWrapper.MktData.CSV <- function() {
  # internally updated data
#  .data. <- character(8)
#  
#  get.data <- function() return(.data.)
#
  eW <- eWrapper(NULL)  # use basic template

  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) 
  {
    tickType = msg[3]
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data") #[[1]]  # list position of symbol (by id == msg[2])
    data[[id]][1] <- timestamp
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
      data[[id]][2:3] <- msg[5:4]
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
      data[[id]][4:5] <- msg[4:5]
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
      data[[id]][6] <- msg[4]
    }
    #data[[as.numeric(msg[2])]] <- data
    eW$assign.Data("data", data)
  }
  eW$tickSize  <- function(curMsg, msg, timestamp, file, ...) 
  { 
    #data <- eW$get.Data("data")
    data <- eW$get.Data("data")
    tickType = msg[3]
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data[[id]][1] <- timestamp
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
      data[[id]][2] <- msg[4]
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
      data[[id]][5] <- msg[4]
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
      data[[id]][7] <- msg[4]
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
      data[[id]][8] <- msg[4]
    }
    eW$assign.Data("data", data)
  }

  return(eW)
}
