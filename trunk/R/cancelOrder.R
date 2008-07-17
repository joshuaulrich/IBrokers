`cancelOrder` <-
function(conn, orderId, verbose=TRUE) {
  if(!inherits(conn,'twsConnection'))
    stop('requires twsConnection object')

  con <- conn[[1]]
  VERSION <- "1"

  writeBin(.twsOutgoingMSG$CANCEL_ORDER, con)
  writeBin(VERSION,con)
  writeBin(as.character(orderId),con)

  waiting <- TRUE

  while(waiting) {
    curChar <- readBin(con,character(),1)
        
    if(length(curChar) > 0) {
      if(curChar==.twsIncomingMSG$ERR_MSG) {
        if(!errorHandler(con, verbose, OK = c(165, 202, 300, 366, 2104,2106,2107))) {
          stop("Unable to complete TWS request")
        }   
      }   

      if(curChar==.twsIncomingMSG$ORDER_STATUS) {
        orderStatus <- readBin(con, character(), 11)
        ### need an orderStatus handler here
        waiting <- FALSE
      }
    }   
  }
  return(orderStatus)
}
