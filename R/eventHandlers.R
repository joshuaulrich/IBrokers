`e_tick_price`    <- function(msg,string,timeStamp,file,...) {
  tickType <- string[3]
  if(!is.null(timeStamp)) cat('<',format(Sys.time(),timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," ",sep='',file=file,append=TRUE)
  if(tickType == .twsTickType$BID) {
    cat('bidPrice:',string[4],' ',file=file,append=TRUE)
    cat('bidSize:',string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$ASK) {
    cat('askPrice:',string[4],' ',file=file,append=TRUE)
    cat('askSize:',string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LAST) {
    cat('lastPrice:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$HIGH) {
    cat('highPrice:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LOW) {
    cat('lowPrice:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$CLOSE) {
    cat('closePrice:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$OPEN) {
    cat('openPrice:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LOW_13_WEEK) {
    cat('13-week Low:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$HIGH_13_WEEK) {
    cat('13-week High:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LOW_26_WEEK) {
    cat('26-week Low:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$HIGH_26_WEEK) {
    cat('26-week High:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LOW_52_WEEK) {
    cat('52-week Low:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$HIGH_52_WEEK) {
    cat('52-week High:',string[4],'\n',file=file,append=TRUE)
  } else {
    # something missed?? 
    cat('<default tickPrice> ')
    cat(msg,paste(string),'\n',file=file,append=TRUE)
  }
}

`e_tick_size`    <- function(msg,string,timeStamp,file,...) {
  tickType <- string[3] 
  if(!is.null(timeStamp)) cat('<',format(Sys.time(),timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," ",sep='',file=file,append=TRUE)
  if(tickType == .twsTickType$BID_SIZE) {
    cat('bidSize:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$ASK_SIZE) {
    cat('askSize:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LAST_SIZE) {
    cat('lastSize:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$VOLUME) {
    cat('Volume:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$AVG_VOLUME) {
    cat('averageVolume:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$OPTION_CALL_OPEN_INTEREST) {
    cat('optionCallOpenInterest:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$OPTION_PUT_OPEN_INTEREST) {
    cat('optionPutOpenInterest:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$OPTION_CALL_VOLUME) {
    cat('optionCallVolume:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$OPTION_PUT_VOLUME) {
    cat('optionPutVolume:',string[4],'\n',file=file,append=TRUE)
  } else {
    cat('<default size> ',file=file,append=TRUE)
    cat(paste(string),'\n',file=file,append=TRUE)
  }
}

`e_tick_option`  <- function(msg,string,timeStamp,file,...) {
  tickType <- string[3] 
  if(!is.null(timeStamp)) cat('<',format(Sys.time(),timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," ",sep='',file=file,append=TRUE)
  if(tickType == .twsTickType$BID_OPTION) { #10
    cat('bidOption:',string[4],string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$ASK_OPTION) { #11
    cat('askOption:',string[4],string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LAST_OPTION) { #12
    cat('lastOption:',string[4],string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$MODEL_OPTION) { #13
    cat('modelOption:',string[4],string[5],'\n',file=file,append=TRUE)
  } else {
    cat('<default option> ',file=file,append=TRUE)
    cat(paste(string),'\n',file=file,append=TRUE)
  }
}

`e_tick_generic` <- function(msg,string,timeStamp,file,...) {
  tickType <- string[3] 
  if(!is.null(timeStamp)) cat('<',format(Sys.time(),timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," ",sep='',file=file,append=TRUE)
  if(tickType == .twsTickType$OPTION_IMPLIED_VOL) { #24
    cat('indexFuturePremium:',string[4],'\n',file=file,append=TRUE)
  } else 
  if(tickType == .twsTickType$OPTION_HISTORICAL_VOL) { #23
    cat('optionHistoricalVol:',string[4],string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$INDEX_FUTURE_PREMIUM) { #31
    cat('optionImpliedVol:',string[4],string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$SHORTABLE) { #46
    cat('shortable:',string[4],'\n',file=file,append=TRUE)
  } else {
    cat('<default generic> ',file=file,append=TRUE)
    cat(paste(string),'\n',file=file,append=TRUE)
  }
}

`e_tick_string`  <- function(msg,contents,timeStamp,file,...) {
  tickType <- contents[3] 
  if(!is.null(timeStamp)) cat('<',format(Sys.time(),timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",contents[2]," ",sep='',file=file,append=TRUE)
  if(tickType == .twsTickType$BID_EXCH) { #32
    cat('bidExchange:',contents[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$ASK_EXCH) { #33
    cat('askExchange:',contents[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LAST_TIMESTAMP) { #45
    cat('lastTimestamp:',contents[4],'\n',file=file,append=TRUE)
  } else {
    cat('<default string> ',file=file,append=TRUE)
    cat(paste(contents),'\n',file=file,append=TRUE)
  }
}

`e_tick_EFP`     <- function(msg,contents,timeStamp,file,...) {
    cat('<default EFP> ',file=file,append=TRUE)
    cat(paste(contents),'\n',file=file,append=TRUE)
}

######################################################################
#
#  default event handlers for reqMktDepth
#
######################################################################

`e_update_mkt_depth` <-  function(msg,contents,timeStamp,file,...) {
  if(!is.null(timeStamp)) cat('<',format(Sys.time(),timeStamp),'>,',sep='',file=file,append=TRUE)
  id <- contents[2]
  position <- contents[3]
  operation <- switch(contents[4],
                      '0' = 'insert',
                      '1' = 'update',
                      '2' = 'delete')
  side <- ifelse(contents[5]=='0','bid','ask')
  price <- contents[6]
  size <- contents[7]

  cat(paste("id=",id,',',sep=''),file=file,append=TRUE)
  cat(paste("pos=",position,',',sep=''),file=file,append=TRUE)
  cat(paste("operation=",operation,',',sep=''),file=file,append=TRUE)
  cat(paste("side=",side,',',sep=''),file=file,append=TRUE)
  cat(paste("price=",price,',',sep=''),file=file,append=TRUE)
  cat(paste("size=",size,sep=''),'\n',file=file,append=TRUE)
}

`e_update_mkt_depthL2` <-  function(msg,contents,timeStamp,file,...) {
  if(!is.null(timeStamp)) cat('<',format(Sys.time(),timeStamp),'>,',sep='',file=file,append=TRUE)
  id <- contents[2]
  position <- contents[3]
  marketMaker <- contents[4]
  operation <- switch(contents[5],
                      '0' = 'insert',
                      '1' = 'update',
                      '2' = 'delete')
  side <- ifelse(contents[6]=='0','bid','ask')
  price <- contents[7]
  size <- contents[8]

  cat(paste("id=",id,',',sep=''),file=file,append=TRUE)
  cat(paste("pos=",position,',',sep=''),file=file,append=TRUE)
  cat(paste("marketMaker=",marketMaker,',',sep=''),file=file,append=TRUE)
  cat(paste("operation=",operation,',',sep=''),file=file,append=TRUE)
  cat(paste("side=",side,',',sep=''),file=file,append=TRUE)
  cat(paste("price=",price,',',sep=''),file=file,append=TRUE)
  cat(paste("size=",size,sep=''),'\n',file=file,append=TRUE)
}
