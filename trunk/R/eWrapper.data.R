# eWrapper.data is a event wrapper that
# updates an in memory data base of values
# upon new input from the TWS
#
# This is only implemented for realtimeBars callbacks
# at present, but will be extended in the near future
# to include all events

eWrapper.data <- function() {
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
    data <- eW$get.Data("data") #[[1]]  # list position of symbol (by id == msg[2])
    data[[id]][1] <- timestamp
    if(tickType == .twsTickType$BID) {
      data[[id]][2:3] <- msg[5:4]
    } else
    if(tickType == .twsTickType$ASK) {
      data[[id]][4:5] <- msg[4:5]
    } else
    if(tickType == .twsTickType$LAST) {
      data[[id]][6] <- msg[4]
    }
    eW$assign.Data("data", data)
  }
  eW$tickSize  <- function(curMsg, msg, timestamp, file, ...) 
  { 
    data <- eW$get.Data("data")
    tickType = msg[3]
    id <- as.numeric(msg[2])
    data[[id]][1] <- timestamp
    if(tickType == .twsTickType$BID_SIZE) {
      data[[id]][2] <- msg[4]
    } else
    if(tickType == .twsTickType$ASK_SIZE) {
      data[[id]][5] <- msg[4]
    } else 
    if(tickType == .twsTickType$LAST_SIZE) {
      data[[id]][7] <- msg[4]
    } else
    if(tickType == .twsTickType$VOLUME) {
      data[[id]][8] <- msg[4]
    }
    eW$assign.Data("data", data)
  }
  return(eW)
}
