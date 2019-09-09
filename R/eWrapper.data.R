# eWrapper.data is a event wrapper that
# updates an in memory data base of values
# upon new input from the TWS
#
# This is only implemented for realtimeBars callbacks
# at present, but will be extended in the near future
# to include all events

eWrapper.data <- function(n) {
  # internally updated data
  #  .data. <- character(8)
  #
  #  get.data <- function() return(.data.)
  #
  eW <- eWrapper(NULL) # use basic template
  eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0),
    .Dimnames = list(
      NULL,
      c(
        "BidSize", "BidPrice",
        "AskPrice", "AskSize",
        "Last", "LastSize", "Volume"
      )
    )
  )), n))

  eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) {
    tickType <- msg[3]
    msg <- as.numeric(msg)
    id <- msg[2] # as.numeric(msg[2])
    data <- eW$get.Data("data") # [[1]]  # list position of symbol (by id == msg[2])
    attr(data[[id]], "index") <- as.numeric(Sys.time())
    #    data[[1]] <- rbind(data[[1]],.xts(matrix(rep(NA_real_,7),nc=7), Sys.time()))
    nr.data <- NROW(data[[id]])
    # data[[id]][1] <- as.numeric(Sys.time()) #timestamp
    if (tickType == .twsTickType$BID) {
      data[[id]][nr.data, 1:2] <- msg[5:4]
    } else
    if (tickType == .twsTickType$ASK) {
      data[[id]][nr.data, 3:4] <- msg[4:5]
    } else
    if (tickType == .twsTickType$LAST) {
      data[[id]][nr.data, 5] <- msg[4]
    }
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  eW$tickSize <- function(curMsg, msg, timestamp, file, ...) {
    data <- eW$get.Data("data")
    tickType <- msg[3]
    msg <- as.numeric(msg)
    id <- as.numeric(msg[2])
    #    data[[1]] <- rbind(data[[1]],.xts(matrix(rep(NA_real_,7),nc=7), Sys.time()))
    attr(data[[id]], "index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    # data[[id]][1] <- as.numeric(Sys.time()) #timestamp
    if (tickType == .twsTickType$BID_SIZE) {
      data[[id]][nr.data, 1] <- msg[4]
    } else
    if (tickType == .twsTickType$ASK_SIZE) {
      data[[id]][nr.data, 4] <- msg[4]
    } else
    if (tickType == .twsTickType$LAST_SIZE) {
      data[[id]][nr.data, 6] <- msg[4]
    } else
    if (tickType == .twsTickType$VOLUME) {
      data[[id]][nr.data, 7] <- msg[4]
    }
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  return(eW)
}

eWrapper.RealTimeBars <- function(nbars = 1, nsymbols = 1) {
  eW <- eWrapper(NULL) # use basic template

  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    data <- eW$get.Data("data") # [[1]]  # list position of symbol (by id == msg[2])
    data[[id]][1] <- as.numeric(msg[3])
    data[[id]][2:8] <- as.numeric(msg[4:10])
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  return(eW)
}
