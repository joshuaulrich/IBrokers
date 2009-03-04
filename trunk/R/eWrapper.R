eWrapper <- function(debug=FALSE, symbols=NULL) {
  # a possibly non-NULL vector of symbol names/ids
  symbols <- symbols
  get.symbols <- function() return(symbols)

  # two branches:
  #   the first is the non-debug version
  #   the second is the debug version (raw data)

  if(!debug) {
    tickPrice <- function(curMsg, msg, timestamp, file, ...) 
    {
      e_tick_price(NULL,msg,timestamp,file, get.symbols(),...) 
    }
    tickSize  <- function(curMsg, msg, timestamp, file, ...) 
    { 
      e_tick_size(NULL, msg, timestamp, file, get.symbols(), ...) 
    }
    tickOptionComputation <- function(curMsg, msg, timestamp, file, ...)
    {
      e_tick_option(NULL, msg, timestamp, file, get.symbols(), ...)
    }
    tickGeneric  <- function(curMsg, msg, timestamp, file, ...) 
    {
      e_tick_generic(NULL, msg, timestamp, file, get.symbols(), ...)
    }
    tickString <- function(curMsg, msg, timestamp, file, ...) 
    { 
      e_tick_string(NULL, msg, timestamp, file, get.symbols(), ...) 
    }
    tickEFP  <- function(curMsg, msg, timestamp, file, ...) 
    { 
      e_tick_EFP(NULL, msg, timestamp, file, get.symbols(), ...)
    }
    orderStatus <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    openOrder  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    openOrderEnd <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    updateAccountValue  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    updatePortfolio <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    updateAccountTime  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    accountDownloadEnd  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    nextValidId  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    contractDetails  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    bondContractDetails  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    contractDetailsEnd  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    execDetails  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    updateMktDepth  <- function(curMsg, msg, timestamp, file,  ...) 
    { 
      e_update_mkt_depth(NULL, msg, timestamp, file, get.symbols(), ...)
    }
    updateMktDepthL2  <- function(curMsg, msg, timestamp, file, ...) 
    { 
      e_update_mkt_depthL2(NULL, msg, timestamp, file, get.symbols(), ...)
    }
    updateNewsBulletin  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    managedAccounts  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    receiveFA  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    historicalData  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    scannerParameters  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    scannerData  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    scannerDataEnd  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    realtimeBars  <- function(curMsg, msg, timestamp, file,  ...) 
    { 
      e_real_time_bars(NULL, msg, file=file, get.symbols(), ...) 
    }
    currentTime  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    fundamentalData  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    deltaNeutralValidation  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
  } else {
    # DEBUG code [ twsDEBUG ]
    tickPrice <- tickSize <-
    tickOptionComputation <- tickGeneric <-
    tickString <- tickEFP <-
    orderStatus <- openOrder <- openOrderEnd <-
    updateAccountValue <- updateAccountTime <- updatePortfolio <- 
    accountDownloadEnd <- nextValidId <-
    contractDetails <- bondContractDetails <-
    contractDetailsEnd <- execDetails <-
    updateMktDepth <- updateMktDepthL2 <-
    updateNewsBulletin <- managedAccounts <-
    receiveFA <- historicalData <-
    scannerParameters <- scannerData <- scannerDataEnd <-
    realtimeBars <- currentTime <- fundamentalData <-
    deltaNeutralValidation  <- 
      function(curMsg, msg, timestamp, file, ...) {
        cat(as.character(timestamp),curMsg, msg,"\n",file=file, append=TRUE,...) 
      }
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
  realtimeBars  =  realtimeBars  ,
  currentTime  =  currentTime  ,
  fundamentalData  =  fundamentalData  ,
  deltaNeutralValidation  =  deltaNeutralValidation))
}
