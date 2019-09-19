reqExecutions <- function(twsconn, reqId="0", ExecutionFilter) {
  if(!is.twsConnection(twsconn))
    stop("invalid 'twsConnection' object") 

  con <- twsconn[[1]]

  VERSION <- "3"
  outgoing <- c(.twsOutgoingMSG$REQ_EXECUTIONS,
                VERSION,
                as.character(reqId),
                ExecutionFilter$clientId,
                ExecutionFilter$acctCode,
                ExecutionFilter$time,
                ExecutionFilter$symbol,
                ExecutionFilter$secType,
                ExecutionFilter$exchange,
                ExecutionFilter$side)
  writeBin(outgoing, con)
}

# New utility function. Call immediately after a reqExecutions() call.
readExecutions <- function(twsconn) {
  # .reqOpenOrders(twsconn)
  con <- twsconn[[1]]
  eW <- eWrapper()
  while (TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1L)
    processMsg(curMsg, con, eW)
    if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) break
  }
}
