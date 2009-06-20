.reqOpenOrders <-
function(conn) {
  if(!inherits(conn,'twsConnection'))
    stop('requires twsConnection object')

  con <- conn[[1]]

  VERSION <- "1"

  writeBin(.twsOutgoingMSG$REQ_OPEN_ORDERS, con)
  writeBin(VERSION, con)

}

