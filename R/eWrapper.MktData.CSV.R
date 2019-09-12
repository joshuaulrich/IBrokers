eWrapper.RealTimeBars.CSV <- function(n = 1) {
  eW <- eWrapper(NULL)
  eW$assign.Data(
    "data",
    rep(list(structure(.xts(matrix(rep(NA_real_, 7), ncol = 7), 0),
      .Dimnames = list(
        NULL,
        c(
          "Open", "High",
          "Low", "Close",
          "Volume", "WAP", "Count"
        )
      )
    )), n)
  )

  eW$realtimeBars <- function(curMsg, msg, timestamp, file, ...) {
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data")
    attr(data[[id]], "index") <- as.numeric(msg[3])
    nr.data <- NROW(data[[id]])
    cat(paste(msg[3], # timestamp in POSIXct
      msg[4], # Open
      msg[5], # High
      msg[6], # Low
      msg[7], # Close
      msg[8], # Volume
      msg[9], # WAP
      msg[10], # Count
      sep = ","
    ), "\n", file = file, append = TRUE)
    data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  return(eW)
}

eWrapper.MktData.CSV <- function(n = 1) {
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
    id <- as.numeric(msg[2])
    file <- file[[id]]
    data <- eW$get.Data("data") # [[1]]  # list position of symbol (by id == msg[2])
    attr(data[[id]], "index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    # data[[id]][1] <- timestamp
    if (tickType == .twsTickType$BID) {
      cat(paste(timestamp,
        msg[5], # bidSize
        msg[4], # bidPrice
        "", # askPrice
        "", # askSize
        "", # lastPrice
        "", # lastSize
        "", # Volume
        sep = ","
      ), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 1:2] <- msg[5:4]
    } else
    if (tickType == .twsTickType$ASK) {
      cat(paste(timestamp,
        "", # bidSize
        "", # bidPrice
        msg[4], # askPrice
        msg[5], # askSize
        "", # lastPrice
        "", # lastSize
        "", # Volume
        sep = ","
      ), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 3:4] <- msg[4:5]
    } else
    if (tickType == .twsTickType$LAST) {
      cat(paste(timestamp,
        "", # bidSize
        "", # bidPrice
        "", # askPrice
        "", # askSize
        msg[4], # lastPrice
        "", # lastSize
        "", # Volume
        sep = ","
      ), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 5] <- msg[4]
    }
    # data[[as.numeric(msg[2])]] <- data
    eW$assign.Data("data", data)
    c(curMsg, msg)
  }
  eW$tickSize <- function(curMsg, msg, timestamp, file, ...) {
    data <- eW$get.Data("data")
    tickType <- msg[3]
    msg <- as.numeric(msg)
    id <- as.numeric(msg[2])
    file <- file[[id]]
    attr(data[[id]], "index") <- as.numeric(Sys.time())
    nr.data <- NROW(data[[id]])
    # data[[id]][1] <- timestamp
    if (tickType == .twsTickType$BID_SIZE) {
      cat(paste(timestamp,
        msg[4], # bidSize
        "", # bidPrice
        "", # askPrice
        "", # askSize
        "", # lastPrice
        "", # lastSize
        "", # Volume
        sep = ","
      ), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 1] <- msg[4]
    } else
    if (tickType == .twsTickType$ASK_SIZE) {
      cat(paste(timestamp,
        "", # bidSize
        "", # bidPrice
        "", # askPrice
        msg[4], # askSize
        "", # lastPrice
        "", # lastSize
        "", # Volume
        sep = ","
      ), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 4] <- msg[4]
    } else
    if (tickType == .twsTickType$LAST_SIZE) {
      cat(paste(timestamp,
        "", # bidSize
        "", # bidPrice
        "", # askPrice
        "", # askSize
        "", # lastPrice
        msg[4], # lastSize
        "", # Volume
        sep = ","
      ), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 6] <- msg[4]
    } else
    if (tickType == .twsTickType$VOLUME) {
      cat(paste(timestamp,
        "", # bidSize
        "", # bidPrice
        "", # askPrice
        "", # askSize
        "", # lastPrice
        "", # lastSize
        msg[4], # Volume
        sep = ","
      ), "\n", file = file, append = TRUE)
      data[[id]][nr.data, 7] <- msg[4]
    }
    eW$assign.Data("data", data)
    c(curMsg, msg) # processMsg sees this raw vector
  }

  return(eW)
}
