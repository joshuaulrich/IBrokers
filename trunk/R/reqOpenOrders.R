reqOpenOrders <-
function(twsconn) {
  #if(!inherits(conn,'twsConnection'))
  if(!is.twsConnection(twsconn))
    stop('requires twsConnection object')

  con <- twsconn[[1]]

  VERSION <- "1"

  writeBin(.twsOutgoingMSG$REQ_OPEN_ORDERS, con)
  writeBin(VERSION, con)

}

