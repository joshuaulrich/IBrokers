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
  msg <- msgPV <- NULL
  while (waiting) {
    curMsg <- readBin(con, character(), 1)
    if (curMsg == .twsIncomingMSG$ERR_MSG) {
      if (!errorHandler(con, TRUE, OK = c(165, 300, 
        366, 2104, 2106, 2107))) {
        cat("\n")
        stop("Unable to complete Account Update request")
      }
    }
    if (curMsg == .twsIncomingMSG$ACCT_VALUE) {
      msg <- c(msg,readBin(con, character(), 4)[-1])
    }
    if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
      # code contributed courtesy of Adrian Dragulescu.  Thanks
      msgPV <- readBin(con, character(), 18)[-c(1,18)]
    }
    if (curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
      if(length(msgPV > 1)) {
        waiting <- FALSE
        writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, con)
        writeBin(VERSION, con)
        writeBin(as.character(as.integer(0)), con)
        writeBin(as.character(account), con)
      }
    }
  }
  
  msgAV <- data.frame(matrix(msg,nc=3,byrow=TRUE))
  colnames(msgAV) <- c("key","value","currency")

  msgPV <- data.frame(matrix(msgPV, nc=length(msgPV), byrow=TRUE)) 
  colnames(msgPV) <- c("ContractId","Symbol","secType",
                       "Expiry","Strike","Right","Multiplier","primaryExch","localSymbol",
                       "Currency","Position","marketPrice","marketValue","averageCost","unrealizedPNL","realizedPNL")
  list(accountValue=msgAV, portfolioValue=msgPV)
}

