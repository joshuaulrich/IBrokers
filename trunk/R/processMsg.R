# single event-loop modeled after official TWS-API
# can be used as CALLBACK argument or modified
# to process all possible incoming messages

twsDEBUG <- function(con, ...)
{
  while(1) {
    curMsg <- readBin(con, character(), 1)
    print(processMsg(curMsg, con, ...))
  }
}

twsCALLBACK <- function(con, ...)
{
  while(1) {
    curMsg <- readBin(con, character(), 1)
    processMsg(curMsg, con, ...)
  }
}

processMsg <- function(curMsg, con, ...)
{
  if(curMsg == .twsIncomingMSG$TICK_PRICE) {
    msg <- readBin(con, character(), 6)
  } else
  if(curMsg == .twsIncomingMSG$TICK_SIZE) {
    msg <- readBin(con, character(), 4)
  } else
  if(curMsg == .twsIncomingMSG$ORDER_STATUS) {
    msg <- readBin(con, character(), 11)
  } else
  if(curMsg == .twsIncomingMSG$ERR_MSG) {
    msg <- readBin(con, character(), 4)
  } else
  if(curMsg == .twsIncomingMSG$OPEN_ORDER) {
    msg <- readBin(con, character(), 78)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_VALUE) {
    msg <- readBin(con, character(), 5)
  } else
  if(curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
    msg <- readBin(con, character(), 15)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
    msg <- readBin(con, character(), 2)
  } else
  if(curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
    msg <- readBin(con, character(), 2)
  } else
  if(curMsg == .twsIncomingMSG$CONTRACT_DATA) {
    msg <- readBin(con, character(), 17)
  } else
  if(curMsg == .twsIncomingMSG$EXECUTION_DATA) {
    msg <- readBin(con, character(), 21)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DEPTH) {
    msg <- readBin(con, character(), 7)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
    msg <- readBin(con, character(), 8)
  } else
  if(curMsg == .twsIncomingMSG$NEWS_BULLETINS) {
    msg <- readBin(con, character(), 5)
  } else
  if(curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
    msg <- readBin(con, character(), 2)
  } else
  if(curMsg == .twsIncomingMSG$RECEIVE_FA) {
    msg <- readBin(con, character(), 2) # 3 with xml string
    stop("xml data currently unsupported")
  } else
  if(curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
    header <- readBin(con, character(), 5)
    nbin <- as.numeric(header[5]) * 9
    msg <- readBin(con, character(), nbin)
  } else
  if(curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) {
    stop("unimplemented as of yet")
  } else
  if(curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) {
    stop("unimplemented as of yet")
  } else
  if(curMsg == .twsIncomingMSG$SCANNER_DATA) {
    stop("unimplemented as of yet")
  } else
  if(curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
    msg <- readBin(con, character(), 5)
  } else
  if(curMsg == .twsIncomingMSG$TICK_GENERIC) {
    msg <- readBin(con, character(), 4)
  } else
  if(curMsg == .twsIncomingMSG$TICK_STRING) {
    msg <- readBin(con, character(), 4)
  } else
  if(curMsg == .twsIncomingMSG$TICK_EFP) {
    msg <- readBin(con, character(), 13)
  } else
  if(curMsg == .twsIncomingMSG$CURRENT_TIME) {
    msg <- readBin(con, character(), 2)
  } else
  if(curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
    msg <- readBin(con, character(), 10)
  } else
  if(curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {
    msg <- readBin(con, character(), 3)
  } else
  if(curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
    msg <- readBin(con, character(), 2)
  } else
  if(curMsg == .twsIncomingMSG$OPEN_ORDER_END) {
    msg <- readBin(con, character(), 1)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {
    msg <- readBin(con, character(), 2)
  } else
  if(curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {
    msg <- readBin(con, character(), 2)
  } else
  if(curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {
    msg <- readBin(con, character(), 5)
  } 
  invisible(msg)
  # end of messages
}
