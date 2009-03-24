# single event-loop modeled after official TWS-API
# can be used as CALLBACK argument or modified
# to process selected/all possible incoming messages

twsDEBUG <- function(twsCon, eWrapper, timestamp, file, playback=1, ...)
{
  eWrapper <- eWrapper(debug=TRUE)
  twsCALLBACK(twsCon, eWrapper, timestamp, file, playback, ...)
}

twsCALLBACK <- function(twsCon, eWrapper, timestamp, file, playback=1, ...)
{
  if(missing(eWrapper))
    eWrapper <- eWrapper()
  con <- twsCon[[1]]

  if(inherits(twsCon, 'twsPlayback')) {
    sys.time <- NULL
    while(TRUE) {
      last.time <- sys.time
      sys.time <- as.POSIXct(strptime(paste(readBin(con, character(), 2), collapse=' '), timestamp))
      if(!is.null(last.time)) {
        Sys.sleep((sys.time-last.time)*playback)
      }
      curMsg <- readBin(con, character(), 1)
      if(length(curMsg) < 1)
        next
      if(!is.null(timestamp)) {
        processMsg(curMsg, con, eWrapper, format(sys.time, timestamp), file, ...)
      } else {
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
      }
    }
  } 
  else { 
    while(TRUE) {
      curMsg <- readBin(con, character(), 1)
      if(length(curMsg) < 1)
        next
      if(!is.null(timestamp)) {
        processMsg(curMsg, con, eWrapper, format(Sys.time(), timestamp), file, ...)
      } else {
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
      }
    }
  }
}

processMsg <- function(curMsg, con, eWrapper, timestamp, file, ...)
{
  if(curMsg == .twsIncomingMSG$TICK_PRICE) {
    msg <- readBin(con, character(), 6)
    eWrapper$tickPrice(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_SIZE) {
    msg <- readBin(con, character(), 4)
    eWrapper$tickSize(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ORDER_STATUS) {
    msg <- readBin(con, character(), 11)
    eWrapper$orderStatus(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ERR_MSG) {
    msg <- readBin(con, character(), 4)
  } else
  if(curMsg == .twsIncomingMSG$OPEN_ORDER) {
    msg <- readBin(con, character(), 78)
    eWrapper$openOrder(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_VALUE) {
    msg <- readBin(con, character(), 5)
    eWrapper$accountValue(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
    msg <- readBin(con, character(), 18)
    eWrapper$portfolioValue(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
    msg <- readBin(con, character(), 2)
    eWrapper$accountUpdateTime(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
    msg <- readBin(con, character(), 2)
    eWrapper$nextValidId(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CONTRACT_DATA) {
    msg <- readBin(con, character(), 17)
    if(msg[1] > 4)
      msg <- c(msg, readBin(con, character(), 2)) # makes up for id, and underConId
    eWrapper$contractData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$EXECUTION_DATA) {
    msg <- readBin(con, character(), 21)
    eWrapper$execData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DEPTH) {
    msg <- readBin(con, character(), 7)
    eWrapper$updateMktDepth(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
    msg <- readBin(con, character(), 8)
    eWrapper$updateMktDepthL2(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$NEWS_BULLETINS) {
    msg <- readBin(con, character(), 5)
    eWrapper$newsBulletins(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
    msg <- readBin(con, character(), 2)
    eWrapper$managedAccounts(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$RECEIVE_FA) {
    msg <- readBin(con, character(), 2) # 3 with xml string
    stop("xml data currently unsupported")
    eWrapper$receiveFA(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
    header <- readBin(con, character(), 5)
    nbin <- as.numeric(header[5]) * 9
    msg <- readBin(con, character(), nbin)
    eWrapper$historicalData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) {
    stop("unimplemented as of yet")
    eWrapper$bondContractData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) {
    stop("unimplemented as of yet")
    eWrapper$scannerParameters(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$SCANNER_DATA) {
    stop("unimplemented as of yet")
    eWrapper$scannerData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
    msg <- readBin(con, character(), 5)
    if(msg[3] == .twsTickType$MODEL_OPTION) {
      msg <- c(msg, readBin(con, character(), 2))
    } else msg <- c(msg,NA,NA)
    eWrapper$tickOptionComputation(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_GENERIC) {
    msg <- readBin(con, character(), 4)
    eWrapper$tickGeneric(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_STRING) {
    msg <- readBin(con, character(), 4)
    eWrapper$tickString(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_EFP) {
    msg <- readBin(con, character(), 10)
    eWrapper$tickEFP(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CURRENT_TIME) {
    msg <- readBin(con, character(), 2)
    eWrapper$currentTime(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
    msg <- readBin(con, character(), 10)
    eWrapper$realtimeBars(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {
    msg <- readBin(con, character(), 3)
    eWrapper$fundamentalData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
    msg <- readBin(con, character(), 2)
    eWrapper$contractDataEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$OPEN_ORDER_END) {
    msg <- readBin(con, character(), 1)
    eWrapper$openOrderEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {
    msg <- readBin(con, character(), 2)
    eWrapper$accountDownloadEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {
    msg <- readBin(con, character(), 2)
    eWrapper$execDetailsEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {
    msg <- readBin(con, character(), 5)
    eWrapper$deltaNeutralValidation(curMsg, msg, timestamp, file, ...)
  } else {
    # default handler/error
    stop("unknown incoming message. reset connection", call.=FALSE)
  }
  # end of messages
}
