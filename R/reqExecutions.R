reqExecutions <- function(twsconn, reqId = "0", ExecutionFilter) {
  if (!is.twsConnection(twsconn)) {
    stop("invalid 'twsConnection' object")
  }

  con <- twsconn[[1]]

  VERSION <- "3"
  outgoing <- c(
    .twsOutgoingMSG$REQ_EXECUTIONS,
    VERSION,
    as.character(reqId),
    ExecutionFilter$clientId,
    ExecutionFilter$acctCode,
    ExecutionFilter$time,
    ExecutionFilter$symbol,
    ExecutionFilter$secType,
    ExecutionFilter$exchange,
    ExecutionFilter$side
  )
  writeBin(outgoing, con)
}
