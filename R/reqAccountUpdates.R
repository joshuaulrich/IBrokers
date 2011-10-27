cancelAccountUpdates <- function(conn, acctCode="1")
{
  if(!is.twsConnection(conn))
    stop("requires twsConnection object")
  .reqAccountUpdates(conn, "0", acctCode)
}

.reqAccountUpdates <- function(conn, subscribe=TRUE, acctCode="1")
{
  if (!is.twsConnection(conn))
      stop("requires twsConnection object")

  con <- conn[[1]]
  VERSION <- "2"

  # send messages to TWS
  writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, con)
  writeBin(VERSION, con)
  writeBin(as.character(as.numeric(subscribe)), con) 
  writeBin(as.character(acctCode), con)
}

reqAccountUpdates <- 
function (conn,
          subscribe=TRUE,
          acctCode="1",
          eventWrapper=eWrapper(),
          CALLBACK=twsCALLBACK, ...)
{
  if (!is.twsConnection(conn))
      stop("requires twsConnection object")

  .reqAccountUpdates(conn, subscribe, acctCode)

  on.exit(.reqAccountUpdates(conn, "0", acctCode))

  verbose <- FALSE
  acct <- list()
  con <- conn[[1]]
  eW <- eWrapper(NULL)
  eW$assign.Data("data", structure(list(), class="eventAccountValue"))
  eW$updatePortfolio <- 
  function (curMsg, msg, ...) {
    version <- as.numeric(msg[1])
    contract <- twsContract(conId = msg[2], symbol = msg[3], 
      sectype = msg[4], exch = msg[9], primary = msg[9], expiry = msg[5], 
      strike = msg[6], currency = msg[10], right = msg[7], 
      local = msg[11], multiplier = msg[8], combo_legs_desc = "", 
      comboleg = "", include_expired = "")
    portfolioValue <- list()
    portfolioValue$position <- as.numeric(msg[12])
    portfolioValue$marketPrice <- as.numeric(msg[13])
    portfolioValue$marketValue <- as.numeric(msg[14])
    portfolioValue$averageCost <- as.numeric(msg[15])
    portfolioValue$unrealizedPNL <- as.numeric(msg[16])
    portfolioValue$realizedPNL <- as.numeric(msg[17])
    portfolioValue$accountName <- msg[18]
    p <- structure(list(contract = contract, portfolioValue = portfolioValue), 
         class = "eventPortfolioValue")
    p
  }
  eW$updateAccountValue <-
  function (curMsg, msg, ...) {
    data <- eW$get.Data("data")
    data[[msg[2]]] <- c(value=msg[3], currency=msg[4])
    eW$assign.Data("data", data)
  }


#  acct_msgs <- with(.twsIncomingMSG, 
#                   c(ACCT_VALUE,PORTFOLIO_VALUE,ACCT_UPDATE_TIME))
  
  while (TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1)
#    if (!curMsg %in% acct_msgs) {
#      if (curMsg == .twsIncomingMSG$ERR_MSG) {
#        if (!errorHandler(con, verbose, OK = c(165, 300, 
#            366, 2104, 2106, 2107))) {
#            warning("error in reqAccountUpdates details")
#              break
#            }
#        }
#        else {
#            processMsg(curMsg, con, eW, timestamp, file)
#              if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) 
#                break
#        }
#    }
    if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
      acct[[length(acct) + 1]] <- processMsg(curMsg, con, eW, timestamp, file)
    } else {
      processMsg(curMsg, con, eW, timestamp, file)
   }
   if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) 
      break
  }
  return(structure(list(eW$get.Data("data"), acct), class="AccountUpdate"))


 # CALLBACK(conn, eventWrapper, NULL, file)
}
  

