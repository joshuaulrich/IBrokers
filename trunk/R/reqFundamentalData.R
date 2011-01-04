reqFundamentalData <- function(twsconn, reqId, contract, reportType) {
  if( !is.twsConnection(twsconn))
    stop('invalid twsConnection')
  if( !is.twsContract(contract))
    stop('invalid twsContract')

  VERSION <- "1"

  msg <- c( .twsOutgoingMSG$REQ_FUNDAMENTAL_DATA,
            VERSION,
            reqId,
            
            # contract fields
            contract$symbol,
            contract$sectype,
            contract$exch,
            contract$primary,
            contract$currency,
            contract$local,

            reportType)

  writeBin( as.character(msg), twsconn[[1]])
}
