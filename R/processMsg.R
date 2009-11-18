# single event-loop modeled after official TWS-API
# can be used as CALLBACK argument or modified
# to process selected/all possible incoming messages

twsDEBUG <- function(twsCon, eWrapper, timestamp, file, playback=1, ...)
{
  eWrapper <- eWrapper(debug=TRUE)
  twsCALLBACK(twsCon, eWrapper, timestamp, file, playback, ...)
}

#  tws <- twsConnect();CON2 <- twsConnect(2)
#  reqMktData(CON2, twsSTK("AAPL"), event=eWrapper(TRUE), CALLBACK=NA, tickerId="2")
#  reqMktData(tws, twsSTK("IBM"), event=eWrapper(symbols=c("AAPL","IBM")))
#  close(tws); close(CON2)
#  uncomment the elements of twsCALLBACK as appropriate
twsCALLBACK <- function(twsCon, eWrapper, timestamp, file, playback=1, ...)
{
  if(missing(eWrapper))
    eWrapper <- eWrapper()
  con <- twsCon[[1]]

  if(inherits(twsCon, 'twsPlayback')) {
    sys.time <- NULL
    while(TRUE) {
      if(!is.null(timestamp)) {
        # MktData
        last.time <- sys.time
        sys.time <- as.POSIXct(strptime(paste(readBin(con, character(), 2), collapse=' '), timestamp))
        if(!is.null(last.time)) {
          Sys.sleep((sys.time-last.time)*playback)
        }
        #curMsg <- readBin(con, character(), 1)
        curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        if(length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, format(sys.time, timestamp), file, ...)
      } else {
        # RealTimeBars
        curMsg <- readBin(con, character(), 1)
        if(length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
        if(curMsg == .twsIncomingMSG$REAL_TIME_BARS) Sys.sleep(5 * playback)
      }
    }
  } 
  else { 
    #dataCON <- get("DATACON", .GlobalEnv)[[1]]
    while(TRUE) {
      socketSelect(list(con), FALSE, NULL)
      #curMsg <- readBin(con, character(), 1)
      curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
      if(!is.null(timestamp)) {
        processMsg(curMsg, con, eWrapper, format(Sys.time(), timestamp), file, ...)
      } else {
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
      }
      # data processing
      # depending on the data provider, processMsg will be replaced with the message processing
      # loop required.
#      if(length(curMsg2) < 1) {
#        p <- min(p*1.01, 0.01)
#        Sys.sleep(p)
#      } else {
#        p <- 0.001
#        if(!is.null(timestamp)) {
#          processMsg(curMsg2, dataCON, eWrapper, format(Sys.time(), timestamp), file, ...)
#        } else {
#          processMsg(curMsg2, dataCON, eWrapper, timestamp, file, ...)
#        }
#     }
      # INSERT TRADE LOGIC HERE
    }
  }
}

processMsg <- function(curMsg, con, eWrapper, timestamp, file, ...)
{
  if(curMsg == .twsIncomingMSG$TICK_PRICE) {
    #msg <- readBin(con, character(), 6)
    msg <- .Internal(readBin(con, "character", 6L, NA_integer_, TRUE, FALSE))
    eWrapper$tickPrice(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_SIZE) {
    #msg <- readBin(con, character(), 4)
    msg <- .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE))
    eWrapper$tickSize(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ORDER_STATUS) {
    #msg <- readBin(con, character(), 11)
    msg <- .Internal(readBin(con, "character", 11L, NA_integer_, TRUE, FALSE))
    eWrapper$orderStatus(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ERR_MSG) {
    #msg <- readBin(con, character(), 4)
    msg <- .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE))
    eWrapper$errorMessage(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$OPEN_ORDER) {
    #msg <- readBin(con, character(), 84)
    msg <- .Internal(readBin(con, "character", 84L, NA_integer_, TRUE, FALSE))
    eWrapper$openOrder(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_VALUE) {
    #msg <- readBin(con, character(), 5)
    msg <- .Internal(readBin(con, "character", 5L, NA_integer_, TRUE, FALSE))
    eWrapper$updateAccountValue(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
    #msg <- readBin(con, character(), 18)
    msg <- .Internal(readBin(con, "character", 18L, NA_integer_, TRUE, FALSE))
    eWrapper$updatePortfolio(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
    #msg <- readBin(con, character(), 2)
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    eWrapper$updateAccountTime(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
    #msg <- readBin(con, character(), 2)
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    eWrapper$nextValidId(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CONTRACT_DATA) {
    #msg <- readBin(con, character(), 21)
    msg <- .Internal(readBin(con, "character", 21L, NA_integer_, TRUE, FALSE))
    eWrapper$contractDetails(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$EXECUTION_DATA) {
    #msg <- readBin(con, character(), 24)
    msg <- .Internal(readBin(con, "character", 24L, NA_integer_, TRUE, FALSE))
    eWrapper$execDetails(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DEPTH) {
    #msg <- readBin(con, character(), 7)
    msg <- .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE))
    eWrapper$updateMktDepth(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
    #msg <- readBin(con, character(), 8)
    msg <- .Internal(readBin(con, "character", 8L, NA_integer_, TRUE, FALSE))
    eWrapper$updateMktDepthL2(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$NEWS_BULLETINS) {
    #msg <- readBin(con, character(), 5)
    msg <- .Internal(readBin(con, "character", 5L, NA_integer_, TRUE, FALSE))
    eWrapper$newsBulletins(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
    #msg <- readBin(con, character(), 2)
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    eWrapper$managedAccounts(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$RECEIVE_FA) {
    #msg <- readBin(con, character(), 2) # 3 with xml string
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    stop("xml data currently unsupported")
    eWrapper$receiveFA(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
    header <- readBin(con, character(), 5)
    nbin <- as.numeric(header[5]) * 9
    #msg <- readBin(con, character(), nbin)
    msg <- .Internal(readBin(con, "character", as.integer(nbin), NA_integer_, TRUE, FALSE))
    eWrapper$historicalData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) {
    stop("unimplemented as of yet")
    eWrapper$bondContractDetails(curMsg, msg, timestamp, file, ...)
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
    #msg <- readBin(con, character(), 5)
    msg <- .Internal(readBin(con, "character", 5L, NA_integer_, TRUE, FALSE))
    if(msg[3] == .twsTickType$MODEL_OPTION) {
      #msg <- c(msg, readBin(con, character(), 2))
      msg <- c(msg,.Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)))
    } else msg <- c(msg,NA,NA)
    eWrapper$tickOptionComputation(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_GENERIC) {
    #msg <- readBin(con, character(), 4)
    msg <- .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE))
    eWrapper$tickGeneric(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_STRING) {
    #msg <- readBin(con, character(), 4)
    msg <- .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE))
    eWrapper$tickString(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_EFP) {
    #msg <- readBin(con, character(), 10)
    msg <- .Internal(readBin(con, "character", 10L, NA_integer_, TRUE, FALSE))
    eWrapper$tickEFP(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CURRENT_TIME) {
    #msg <- readBin(con, character(), 2)
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    eWrapper$currentTime(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
    #msg <- readBin(con, character(), 10)
    msg <- .Internal(readBin(con, "character", 10L, NA_integer_, TRUE, FALSE))
    eWrapper$realtimeBars(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {
    #msg <- readBin(con, character(), 3)
    msg <- .Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE))
    eWrapper$fundamentalData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
    #msg <- readBin(con, character(), 2)
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    eWrapper$contractDetailsEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$OPEN_ORDER_END) {
    #msg <- readBin(con, character(), 1)
    msg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
    eWrapper$openOrderEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {
    #msg <- readBin(con, character(), 2)
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    eWrapper$accountDownloadEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {
    #msg <- readBin(con, character(), 2)
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    eWrapper$execDetailsEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {
    #msg <- readBin(con, character(), 5)
    msg <- .Internal(readBin(con, "character", 5L, NA_integer_, TRUE, FALSE))
    eWrapper$deltaNeutralValidation(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
    #msg <- readBin(con, character(), 2)
    msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
    eWrapper$tickSnapshotEnd(curMsg, msg, timestamp, file, ...)
  } else {
    # default handler/error
    warning(paste("Unknown incoming message: ",curMsg,". Please reset connection",sep=""), call.=FALSE)
  }
  # end of messages
}
