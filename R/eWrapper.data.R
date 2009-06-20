# eWrapper.data is a event wrapper that
# updates an in memory data base of values
# upon new input from the TWS
#
# This is only implemented for realtimeBars callbacks
# at present, but will be extended in the near future
# to include all events

eWrapper.data <- function() {
  eW <- eWrapper(NULL) # a template
  eW$assign.Data("data", list())
  eW$reatimeBars <- function(curMsg, msg, timestamp, file, symbols, ...) {
    # curMsg: header value.  In this case "50" per TWS
    # msg is the remaining values sent
    # in this case:
    #  Version (unused),Id,time,open,high,low,close,volume,wap,count
    data <- eW$get.Data("data")
browser()
    data[[symbols[as.numeric(msg[2])]]] <- msg[-(1:2)]
    eW$assign.Data("data", data)
  }
  eW
}
