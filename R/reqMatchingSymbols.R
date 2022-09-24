.reqMatchingSymbols <-
function(conn, pattern) {
  if(!isConnected(conn))
    stop('peer has gone away. check your IB connection',call.=FALSE)
  if(!is.twsConnection(conn))
    stop('requires twsConnection object')

  con <- conn[[1]]
  reqId <- 1
  
  request <- c(.twsOutgoingMSG$REQ_MATCHING_SYMBOLS,
               reqId,
               pattern)
  writeBin(request, con)
}

reqMatchingSymbols <-
function(twsconn, pattern) {
  .reqMatchingSymbols(twsconn, pattern)
  con <- twsconn[[1]]
  e_matching_symbols <- eWrapper()
  e_matching_symbols$symbolSamples <- function(curMsg, msg, timestamp, file, ...) { msg[2] }
  while (isConnected(twsconn)) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1)
    symbolSamples <- processMsg(curMsg,
                              con,
                              eWrapper=e_matching_symbols,
                              twsconn=twsconn,
                              timestamp=NULL, file="")
    if(curMsg == .twsIncomingMSG$SYMBOL_SAMPLES)
      break
  }
  
  return(symbolSamples)
}

