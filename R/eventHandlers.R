`e_tick_price`    <- function(msg,string) {
  tickType <- string[3]
  if(tickType == .twsTickType$BID) {
    cat('bidPrice:',string[4],' ')
    cat('bidSize:',string[5],'\n')
  }
  if(tickType == .twsTickType$ASK) {
    cat('askPrice:',string[4],' ')
    cat('askSize:',string[5],'\n')
  }
  if(tickType == .twsTickType$LAST) {
    cat('lastPrice:',string[4],'\n')
  }
  if(tickType == .twsTickType$HIGH) {
    cat('highPrice:',string[4],'\n')
  }
  if(tickType == .twsTickType$LOW) {
    cat('lowPrice:',string[4],'\n')
  }
  if(tickType == .twsTickType$CLOSE) {
    cat('closePrice:',string[4],'\n')
  }
  if(tickType == .twsTickType$OPEN) {
    cat('openPrice:',string[4],'\n')
  }
  if(tickType == .twsTickType$LOW_13_WEEK) {
    cat('13-week Low:',string[4],'\n')
  }
  if(tickType == .twsTickType$HIGH_13_WEEK) {
    cat('13-week High:',string[4],'\n')
  }
  if(tickType == .twsTickType$LOW_26_WEEK) {
    cat('26-week Low:',string[4],'\n')
  }
  if(tickType == .twsTickType$HIGH_26_WEEK) {
    cat('26-week High:',string[4],'\n')
  }
  if(tickType == .twsTickType$LOW_52_WEEK) {
    cat('52-week Low:',string[4],'\n')
  }
  if(tickType == .twsTickType$HIGH_52_WEEK) {
    cat('52-week High:',string[4],'\n')
  }
  
  #cat('<default tickPrice> ')
  #cat(msg,paste(string),'\n')
}

`e_tick_size`    <- function(msg,string) {
  tickType <- string[3] 
  if(tickType == .twsTickType$BID_SIZE) {
    cat('bidSize:',string[4],'\n')
  }
  if(tickType == .twsTickType$ASK_SIZE) {
    cat('askSize:',string[4],'\n')
  }
  if(tickType == .twsTickType$LAST_SIZE) {
    cat('lastSize:',string[4],'\n')
  }
  if(tickType == .twsTickType$VOLUME) {
    cat('Volume:',string[4],'\n')
  }
  if(tickType == .twsTickType$AVG_VOLUME) {
    cat('averageVolume:',string[4],'\n')
  }
  if(tickType == .twsTickType$OPTION_CALL_OPEN_INTEREST) {
    cat('optionCallOpenInterest:',string[4],'\n')
  }
  if(tickType == .twsTickType$OPTION_PUT_OPEN_INTEREST) {
    cat('optionPutOpenInterest:',string[4],'\n')
  }
  if(tickType == .twsTickType$OPTION_CALL_VOLUME) {
    cat('optionCallVolume:',string[4],'\n')
  }
  if(tickType == .twsTickType$OPTION_PUT_VOLUME) {
    cat('optionPutVolume:',string[4],'\n')
  }
}

`e_tick_option`  <- function(msg,string) {

}

`e_tick_generic` <- function(con) {

}

`e_tick_string`  <- function(con) {

}

`e_tick_EFP`     <- function(con) {

}
