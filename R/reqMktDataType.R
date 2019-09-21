reqMktDataType <- function(conn, mktDataType = 3) {
  if(!is.twsConnection(conn))
    stop('invalid twsConnection')

#  1: REALTIME       Frozen, Delayed, and Delayed-Frozen data are disabled
#  2: FROZEN         Frozen data is enabled
#  3: DELAYED        Delayed data is enabled, Delayed-Frozen data is disabled
#  4: DELAYED_FROZEN Delayed and Delayed-Frozen data are enabled

  if(!(mktDataType %in% 1:4)) {
    stop('mktDataType ', mktDataType, ' is not supported\n  Set to ',
         '1 (real-time), 2 (frozen), 3 (delayed), or 4 (delayed-frozen)')
  }

  VERSION <- "1"

  msg <- c(.twsOutgoingMSG$REQ_MARKET_DATA_TYPE,
           VERSION,
           mktDataType)

  writeBin(as.character(msg), conn[[1]])

  invisible(NULL)
}
