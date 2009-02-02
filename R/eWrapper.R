eWrapper <- function(debug=FALSE) {
  if(!debug) {
  tickPrice <- function(msg, timestamp, file, ...) 
  {
    e_tick_price(NULL,msg,timestamp,file,...) 
  }
  tickSize  <- function(msg, timestamp, file, ...) 
  { 
    e_tick_size(NULL, msg, timestamp, file, ...) 
  }
  tickOptionComputation <- function(msg, timestamp, file, ...)
  {
    e_tick_option(NULL, msg, timestamp, file, ...)
  }
  tickGeneric  <- function(msg, timestamp, file, ...) 
  {
    e_tick_generic(NULL, msg, timestamp, file, ...)
  }
  tickString <- function(msg, timestamp, file, ...) 
  { 
    e_tick_string(NULL, msg, timestamp, file, ...) 
  }
  tickEFP  <- function(msg, timestamp, file, ...) 
  { 
    e_tick_EFP(NULL, msg, timestamp, file, ...)
  }
  orderStatus <- function(msg, timestamp, file, ...) { msg }
  openOrder  <- function(msg, timestamp, file, ...) { msg }
  openOrderEnd <- function(msg, timestamp, file, ...) { msg }
  updateAccountValue  <- function(msg, timestamp, file, ...) { msg }
  updatePortfolio <- function(msg, timestamp, file, ...) { msg }
  updateAccountTime  <- function(msg, timestamp, file, ...) { msg }
  accountDownloadEnd  <- function(msg, timestamp, file, ...) { msg }
  nextValidId  <- function(msg, timestamp, file, ...) { msg }
  contractDetails  <- function(msg, timestamp, file, ...) { msg }
  bondContractDetails  <- function(msg, timestamp, file, ...) { msg }
  contractDetailsEnd  <- function(msg, timestamp, file, ...) { msg }
  execDetails  <- function(msg, timestamp, file, ...) { msg }
  updateMktDepth  <- function(msg, timestamp, file, ...) 
  { 
    e_update_mkt_depth(NULL, msg, timestamp, file, ...)
  }
  updateMktDepthL2  <- function(msg, timestamp, file, ...) 
  { 
    e_update_mkt_depthL2(NULL, msg, timestamp, file, ...)
  }
  updateNewsBulletin  <- function(msg, timestamp, file, ...) { msg }
  managedAccounts  <- function(msg, timestamp, file, ...) { msg }
  receiveFA  <- function(msg, timestamp, file, ...) { msg }
  historicalData  <- function(msg, timestamp, file, ...) { msg }
  scannerParameters  <- function(msg, timestamp, file, ...) { msg }
  scannerData  <- function(msg, timestamp, file, ...) { msg }
  scannerDataEnd  <- function(msg, timestamp, file, ...) { msg }
  realtimeBar  <- function(msg, timestamp, file, ...) 
  { 
    e_real_time_bars(NULL, msg, file=file, ...) 
  }
  currentTime  <- function(msg, timestamp, file, ...) { msg }
  fundamentalData  <- function(msg, timestamp, file, ...) { msg }
  deltaNeutralValidation  <- function(msg, timestamp, file, ...) { msg }
  } else {
  tickPrice <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  tickSize  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  tickOptionComputation <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  tickGeneric  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  tickString <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  tickEFP  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  orderStatus <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  openOrder  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  openOrderEnd <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  updateAccountValue  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  updatePortfolio <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  updateAccountTime  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  accountDownloadEnd  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  nextValidId  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  contractDetails  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  bondContractDetails  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  contractDetailsEnd  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  execDetails  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  updateMktDepth  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  updateMktDepthL2  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  updateNewsBulletin  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  managedAccounts  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  receiveFA  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  historicalData  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  scannerParameters  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  scannerData  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  scannerDataEnd  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  realtimeBar  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  currentTime  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  fundamentalData  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  deltaNeutralValidation  <- function(msg, timestamp, file, ...) { cat(as.character(timestamp),msg,"\n",file=file, ...) }
  }

  return(list(
  tickPrice =  tickPrice ,
  tickSize  =  tickSize  ,
  tickOptionComputation =  tickOptionComputation ,
  tickGeneric  =  tickGeneric  ,
  tickString =  tickString ,
  tickEFP  =  tickEFP  ,
  orderStatus =  orderStatus ,
  openOrder  =  openOrder  ,
  openOrderEnd =  openOrderEnd ,
  updateAccountValue  =  updateAccountValue  ,
  updatePortfolio =  updatePortfolio ,
  updateAccountTime  =  updateAccountTime  ,
  accountDownloadEnd  =  accountDownloadEnd  ,
  nextValidId  =  nextValidId  ,
  contractDetails  =  contractDetails  ,
  bondContractDetails  =  bondContractDetails  ,
  contractDetailsEnd  =  contractDetailsEnd  ,
  execDetails  =  execDetails  ,
  updateMktDepth  =  updateMktDepth  ,
  updateMktDepthL2  =  updateMktDepthL2  ,
  updateNewsBulletin  =  updateNewsBulletin  ,
  managedAccounts  =  managedAccounts  ,
  receiveFA  =  receiveFA  ,
  historicalData  =  historicalData  ,
  scannerParameters  =  scannerParameters  ,
  scannerData  =  scannerData  ,
  scannerDataEnd  =  scannerDataEnd  ,
  realtimeBar  =  realtimeBar  ,
  currentTime  =  currentTime  ,
  fundamentalData  =  fundamentalData  ,
  deltaNeutralValidation  =  deltaNeutralValidation))
}
