eWrapper.MktDepth.CSV <- function() {
  # internally updated data
  .data. <- character(21)
  
  get.data <- function() return(.data.)

  updateMktDepth <- function(curMsg, msg, timestamp, file, ...)
  {
    BID <- 1; ASK <- 2
    string <- character(21)
    .data.[1] <<- timestamp
    string[1] <-  timestamp
    if(msg[3] == "0") {
      if(msg[4] == "2") {
#        msg[6] <- NA
#        msg[7] <- NA
      }
      if(msg[5] == BID) {
        .data.[11] <- msg[6] # price
        .data.[10] <- msg[7] # size
        string[11:10] <- msg[6:7]
      } else {
        .data.[12] <- msg[6]
        .data.[13] <- msg[7]
        string[12:13] <- msg[6:7]
      }
      cat(paste(string, collapse=","),"\n", file=file, append=TRUE, ...)
    } else
    if(msg[3] == "1") {
      if(msg[4] == "2") {
#        msg[6] <- NA
#        msg[7] <- NA
      }
      if(msg[5] == BID) {
        .data.[9] <- msg[6] # price
        .data.[8] <- msg[7] # size
        string[9:8] <- msg[6:7]
      } else {
        .data.[14] <- msg[6]
        .data.[15] <- msg[7]
        string[14:15] <- msg[6:7]
      }
      cat(paste(string, collapse=","),"\n", file=file, append=TRUE, ...)

    } else
    if(msg[3] == "2") {
      if(msg[4] == "2") {
#        msg[6] <- NA
#        msg[7] <- NA
      }
      if(msg[5] == BID) {
        .data.[7] <- msg[6] # price
        .data.[6] <- msg[7] # size
        string[7:6] <- msg[6:7]
      } else {
        .data.[16] <- msg[6]
        .data.[17] <- msg[7]
        string[16:17] <- msg[6:7]
      }
      cat(paste(string, collapse=","),"\n", file=file, append=TRUE, ...)
    } else
    if(msg[3] == "3") {
      if(msg[4] == "2") {
#        msg[6] <- NA
#        msg[7] <- NA
      }
      if(msg[5] == BID) {
        .data.[5] <- msg[6] # price
        .data.[4] <- msg[7] # size
        string[5:4] <- msg[6:7]
      } else {
        .data.[18] <- msg[6]
        .data.[19] <- msg[7]
        string[18:19] <- msg[6:7]
      }
      cat(paste(string, collapse=","),"\n", file=file, append=TRUE, ...)
    } else
    if(msg[3] == "4") {
      if(msg[4] == "2") {
#        msg[6] <- NA
#        msg[7] <- NA
      }
      if(msg[5] == BID) {
        .data.[3] <- msg[6] # price
        .data.[2] <- msg[7] # size
        string[3:2] <- msg[6:7]
      } else {
        .data.[20] <- msg[6]
        .data.[21] <- msg[7]
        string[20:21] <- msg[6:7]
      }
      cat(paste(string, collapse=","),"\n", file=file, append=TRUE, ...)

    }

  }
  # unimplemented for export to CSV
  tickPrice <- tickSize <-
  tickOptionComputation <- tickGeneric  <- 
  tickString <- tickEFP  <- orderStatus <- 
  openOrder  <- openOrderEnd <- updateAccountValue  <-
  updatePortfolio <- updateAccountTime  <- accountDownloadEnd  <-
  nextValidId  <- contractDetails  <- bondContractDetails  <- 
  contractDetailsEnd  <- execDetails  <- #updateMktDepth  <- 
  updateMktDepthL2  <- updateNewsBulletin  <- managedAccounts  <-
  receiveFA  <- historicalData  <- scannerParameters  <-
  scannerData  <- scannerDataEnd  <- realtimeBars  <-
  currentTime  <- fundamentalData  <-
  deltaNeutralValidation  <- function(curMsg, msg, timestamp, file, ...) { }

  return(list(
  get.data = get.data,
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
