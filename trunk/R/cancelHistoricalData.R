`cancelHistoricalData` <-
function(con,tickerId=1) {
  if(!isOpen(con)) stop('invalid TWS connection')
  
  writeBin(.twsOutgoingMSG$CANCEL_HISTORICAL_DATA,con)
  writeBin('1',con)
  writeBin(as.character(tickerId),con)
}
