`reqAccountUpdates` <- 
function (conn,
          account="1",
          eventAccountValue,
          eventPortfolioValue,
          eventAccountTime,
          CALLBACK, ...)
{
  if (!inherits(conn, "twsConnection")) 
      stop("requires twsConnection object")

  con <- conn[[1]]
  VERSION <- "2"

  # send messages to TWS
  writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, con)
  writeBin(VERSION, con)
  writeBin(as.character(1), con)
  writeBin(as.character(account), con)

  if(missing(CALLBACK)) {
    if(missing(eventAccountValue))
      eventAccountValue   <- e_account_value

    if(missing(eventPortfolioValue))
      eventPortfolioValue <- e_portfolio_value

    if(missing(eventAccountTime))
      eventAccountTime    <- e_account_time
  } else
  if(is.null(CALLBACK)) {
      eventAccountValue   <- NULL
      eventPortfolioValue <- NULL
      eventAccountTime    <- NULL
  }

  waiting <- TRUE
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
      contents <- readBin(con, character(), 4)
      if(is.null(eventAccountValue)) {
        cat(curMsg, paste(contents), "\n")
      } else cat(str(eventAccountValue(curMsg, contents, ...)))
    }
    if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
      # code suggested by Adrian Dragulescu.
      contents <- readBin(con, character(), 15)
      if(is.null(eventPortfolioValue)) {
        cat(curMsg, paste(contents), "\n")
      } else cat(str(eventPortfolioValue(curMsg, contents, ...)))
    }
    if (curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
      contents <- readBin(con, character(), 2)
      if(is.null(eventPortfolioValue)) {
        cat(curMsg, paste(contents), "\n")
      } else cat(str(eventAccountTime(curMsg, contents, ...)))
#      if(length(msgPV > 1)) {
#        waiting <- FALSE
#        writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, con)
#        writeBin(VERSION, con)
#        writeBin(as.character(as.integer(0)), con)
#        writeBin(as.character(account), con)
#      }
    }
  }
  
#  msgAV <- data.frame(matrix(msg,nc=3,byrow=TRUE))
#  colnames(msgAV) <- c("key","value","currency")
#
#  msgPV <- data.frame(matrix(msgPV, nc=length(msgPV), byrow=TRUE)) 
#  colnames(msgPV) <- c("ContractId","Symbol","secType",
#                       "Expiry","Strike","Right","Multiplier","primaryExch","localSymbol",
#                       "Currency","Position","marketPrice","marketValue","averageCost","unrealizedPNL","realizedPNL")
#  list(accountValue=msgAV, portfolioValue=msgPV)
}

