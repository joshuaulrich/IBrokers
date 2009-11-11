.cancelOrder <- function(conn, orderId)
{
  if(!is.twsConnection(conn))
    stop('requires twsConnection object')

  con <- conn[[1]]
  VERSION <- "1"

  writeBin(.twsOutgoingMSG$CANCEL_ORDER, con)
  writeBin(VERSION,con)
  writeBin(as.character(orderId),con)
}

`cancelOrder` <-
function(conn, orderId, verbose=TRUE) {

  .cancelOrder(conn, orderId)

  con <- conn[[1]]
  while(1) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con,character(),1)

        
    if(curMsg==.twsIncomingMSG$ORDER_STATUS) {
      orderStatus <- readBin(con, character(), 11)
    } else
    processMsg(curMsg, con, eWrapper(), timestamp=NULL, file="")
  }
  return(orderStatus)
}
