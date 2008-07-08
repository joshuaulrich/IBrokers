`reqAccountUpdates` <- 
function (conn, account="1")
{
  if (!inherits(conn, "twsConnection")) 
      stop("requires twsConnection object")
  con <- conn[[1]]
  VERSION <- "2"
  writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, con)
  writeBin(VERSION, con)
  writeBin(as.character(1), con)
  writeBin(as.character(account), con)
  waiting <- TRUE
  msg <- NULL
  while (waiting) {
    curMsg <- readBin(con, character(), 1)
    if (curMsg == .twsIncomingMSG$ERR_MSG) {
      if (!errorHandler(con, verbose, OK = c(165, 300, 
        366, 2104, 2106, 2107))) {
        cat("\n")
        stop("Unable to complete Account Update request")
      }
    }
    if (curMsg == .twsIncomingMSG$ACCT_VALUE) {
      msg <- c(msg,readBin(con, character(), 4)[-1])
    }
    if (curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
      waiting <- FALSE
      writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, con)
      writeBin(VERSION, con)
      writeBin(as.character(as.integer(0)), con)
      writeBin(as.character(account), con)
    }
  }
  data.frame(matrix(msg,nc=3,byrow=TRUE))
}

