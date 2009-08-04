eWrapper <- function(debug=FALSE) {
  # environment for data to be stored/accessed between messages
  # an example of this functionality is for the "symbols" variable
  # that can be set (by default) to display contract names
  .Data <- new.env()
  get.Data <- function(x) get(x,.Data)
  assign.Data <- function(x, value) assign(x, value, .Data)
  remove.Data <- function(x) remove(x, .Data)

  # three branches:
  #   the first is a version that returns nothing. Useful as a template.
  #   the second is the non-debug version (defaults console output)
  #   the third is the debug version (raw data console output)

  if(is.null(debug)) {
    errorMessage <- function(curMsg, msg, timestamp, file, ...)
    {
      cat(msg,"\n")
      #c(curMsg, msg)
    }
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
    deltaNeutralValidation  <- tickSnapshotEnd <- 
    function(curMsg, msg, timestamp, file,  ...) { c(curMsg,msg) }
  } else
  if(!debug) {
    tickPrice <- function(curMsg, msg, timestamp, file, ...) 
    {
      symbols <- get.Data("symbols")
      e_tick_price(NULL,msg,timestamp,file, symbols,...) 
    }
    tickSize  <- function(curMsg, msg, timestamp, file, ...) 
    { 
      symbols <- get.Data("symbols")
      e_tick_size(NULL, msg, timestamp, file, symbols, ...) 
    }
    tickOptionComputation <- function(curMsg, msg, timestamp, file, ...)
    {
      symbols <- get.Data("symbols")
      e_tick_option(NULL, msg, timestamp, file, symbols, ...)
    }
    tickGeneric  <- function(curMsg, msg, timestamp, file, ...) 
    {
      symbols <- get.Data("symbols")
      e_tick_generic(NULL, msg, timestamp, file, symbols, ...)
    }
    tickString <- function(curMsg, msg, timestamp, file, ...) 
    { 
      symbols <- get.Data("symbols")
      e_tick_string(NULL, msg, timestamp, file, symbols, ...) 
    }
    tickEFP  <- function(curMsg, msg, timestamp, file, ...) 
    { 
      symbols <- get.Data("symbols")
      e_tick_EFP(NULL, msg, timestamp, file, symbols, ...)
    }
    orderStatus <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    errorMessage <- function(curMsg, msg, timestamp, file, ...)
    {
      msg
    }
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
      symbols <- get.Data("symbols")
      e_update_mkt_depth(NULL, msg, timestamp, file, symbols, ...)
    }
    updateMktDepthL2  <- function(curMsg, msg, timestamp, file, ...) 
    { 
      symbols <- get.Data("symbols")
      e_update_mkt_depthL2(NULL, msg, timestamp, file, symbols, ...)
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
      symbols <- get.Data("symbols")
      e_real_time_bars(curMsg, msg, symbols, file, ...) 
    }
    currentTime  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    fundamentalData  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    deltaNeutralValidation  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
    tickSnapshotEnd  <- function(curMsg, msg, timestamp, file,  ...) { c(curMsg, msg) }
  } else {
    # DEBUG code [ twsDEBUG ]
    tickPrice <- tickSize <-
    tickOptionComputation <- tickGeneric <-
    tickString <- tickEFP <-
    orderStatus <- errorMessage <- openOrder <- openOrderEnd <-
    updateAccountValue <- updateAccountTime <- updatePortfolio <- 
    accountDownloadEnd <- nextValidId <-
    contractDetails <- bondContractDetails <-
    contractDetailsEnd <- execDetails <-
    updateMktDepth <- updateMktDepthL2 <-
    updateNewsBulletin <- managedAccounts <-
    receiveFA <- historicalData <-
    scannerParameters <- scannerData <- scannerDataEnd <-
    realtimeBars <- currentTime <- fundamentalData <-
    deltaNeutralValidation  <- tickSnapshotEnd <- 
      function(curMsg, msg, timestamp, file, ...) {
        cat(as.character(timestamp),curMsg, msg,"\n",file=file[[1]], append=TRUE,...) 
      }
  }

  return(list(
  .Data = .Data, get.Data = get.Data, assign.Data = assign.Data, remove.Data = remove.Data,
  tickPrice =  tickPrice ,
  tickSize  =  tickSize  ,
  tickOptionComputation =  tickOptionComputation ,
  tickGeneric  =  tickGeneric  ,
  tickString =  tickString ,
  tickEFP  =  tickEFP  ,
  orderStatus =  orderStatus ,
  errorMessage = errorMessage, 
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
  deltaNeutralValidation  =  deltaNeutralValidation,
  tickSnapshotEnd = tickSnapshotEnd))
}
