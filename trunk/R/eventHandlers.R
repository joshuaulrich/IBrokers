#####  TICK_PRICE ##### {{{
`e_tick_price`    <- function(msg,string,timeStamp,file,...) {
  tickType <- string[3]
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
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
#####  END TICK_PRICE ##### }}}

`e_tick_size`    <- function(msg,string,timeStamp,file,...) {
  tickType <- string[3] 
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
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
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
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
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
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
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
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
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>,',sep='',file=file,append=TRUE)
  id <- contents[2]
  position <- contents[3]
  operation <- switch(contents[4],
                      '0' = 'insert',
                      '1' = 'update',
                      '2' = 'delete')
  side <- ifelse(contents[5]=='1','bid','ask')
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
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>,',sep='',file=file,append=TRUE)
  id <- contents[2]
  position <- contents[3]
  marketMaker <- contents[4]
  operation <- switch(contents[5],
                      '0' = 'insert',
                      '1' = 'update',
                      '2' = 'delete')
  side <- ifelse(contents[6]=='1','bid','ask')
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


######################################################################
#
#  default event handlers for reqRealTimeBars
#
######################################################################

`e_real_time_bars` <- function(msg,contents,file=file,...) {
  columns <- c("Id","time","open","high","low","close","volume",
               "wap","count")
  cat(paste(columns,"=",contents[-1],sep=""),'\n',file=file,append=TRUE)
}


######################################################################
#
#  default event handlers for orderStatus callbacks (from placeOrder, )
#
######################################################################

`e_order_status` <- function(msg, contents, ...) {
   eos <- list(orderId = contents[2],
               status  = contents[3],
               filled  = contents[4],
               remaining = contents[5],
               averageFillPrice = contents[6],
               permId = contents[7],
               parentId = contents[8],
               lastFillPrice = contents[9],
               clientId = contents[10],
               whyHeld  = contents[11]
  )
  structure(eos)
}


`e_open_order` <- function(msg, contents, ...) {
  eoo <- list(
         # need to add contractId to twsContract...
              contract   = twsContract(
                            #contract = contents[3], #contractId not yet added :(
                             symbol  = contents[4],
                             sectype = contents[5],
                             expiry  = contents[6],
                             strike  = contents[7],
                             right   = contents[8],
                             exch    = contents[9],
                             currency= contents[10],
                             local   = contents[11],
                             combo_legs_desc = contents[66],
                             # the following are required to correctly specify a contract
                             primary = NULL,
                             include_expired = NULL,
                             comboleg = NULL,
                             multiplier = NULL
                           ),

              order      = twsOrder(
                             orderId = contents[2],
                             action  = contents[12],                           
                             totalQuantity = contents[13],
                             orderType     = contents[14],
                             lmtPrice      = contents[15],
                             auxPrice      = contents[16],
                             tif           = contents[17],
                             ocaGroup      = contents[18],
                             account       = contents[19],
                             openClose     = contents[20],
                             origin        = contents[21],
                             orderRef      = contents[22],
                             clientId      = contents[23],
                             permId        = contents[24],
                             outsideRTH    = contents[25],
                             hidden        = contents[26],
                             discretionaryAmt = contents[27],
                             goodAfterTime = contents[28],
                             # skip deprecated amount contents[29]
                             faGroup       = contents[30],
                             faMethod      = contents[31],
                             faPercentage  = contents[32],
                             faProfile     = contents[33],
                             goodTillDate  = contents[34],
                             rule80A       = contents[35],
                             settlingFirm  = contents[36],
                             shortSaleSlot = contents[37],
                             designatedLocation = contents[38],
                             auctionStrategy = contents[39],
                             startingPrice = contents[40],
                             stockRefPrice = contents[41],
                             delta         = contents[42],
                             stockRangeLower = contents[43],
                             stockRangeUpper = contents[44],
                             displaySize   = contents[45],
                             # rthOnly (version 18) = contents[46],
                             blockOrder    = contents[47],
                             sweepToFill   = contents[48],
                             allOrNone     = contents[49],
                             minQty        = contents[50],
                             ocaType       = contents[51],
                             eTradeOnly    = contents[52],
                             firmQuoteOnly = contents[53],
                             nbboPriceCap  = contents[54],
                             parentId      = contents[55],
                             triggerMethod = contents[56],
                             volatility    = contents[57],
                             volatilityType = contents[58],
                             deltaNeutralOrderType = contents[59],
                             deltaNeutralAuxPrice  = contents[60],
                             continuousUpdate = contents[61],
                             referencePriceType = contents[62],
                             trailStopPrice     = contents[63],
                             basisPoints        = contents[64],
                             basisPointsType    = contents[65],
                             # part of contract #66
                             scaleNumComponents = contents[67],
                             scaleComponentSize = contents[68],
                             scalePriceIncrement = contents[69],
                             clearingAccount = contents[70],
                             clearingIntent  = contents[71],
                             whatIf          = contents[72],
                           ),

              orderstate = twsOrderState(
                             status      = contents[73],
                             initMargin  = contents[74],
                             maintMargin = contents[75],
                             equityWithLoan = contents[76],
                             commission  = contents[77],
                             minCommission = contents[78],
                             maxCommission = contents[79],
                             commissionCurrency = contents[80],
                             warningText = contents[81]
                           )
         )
  structure(eoo, class='eventOpenOrder')
}

#####  EXECUTION_DATA ##### {{{
`e_execution_data` <-
function(msg, contents, ...) {

       eed <- list(
              contract   = twsContract(
                            #contract = contents[3], #contractId not yet added :(
                             symbol  = contents[4],
                             sectype = contents[5],
                             expiry  = contents[6],
                             strike  = contents[7],
                             right   = contents[8],
                             exch    = contents[9],
                             currency= contents[10],
                             local   = contents[11],
                             # the following are required to correctly specify a contract
                             combo_legs_desc = NULL,
                             primary = NULL,
                             include_expired = NULL,
                             comboleg = NULL,
                             multiplier = NULL
                           ),
              execution  = twsExecution(orderId = contents[2],
                                        execId  = contents[12],
                                        time    = contents[13],
                                        acctNumber = contents[14],
                                        exchange   = contents[15],
                                        side       = contents[16],
                                        shares     = contents[17],
                                        price      = contents[18],
                                        permId     = contents[19],
                                        clientId   = contents[20],
                                        liquidation= contents[21]
                                       )
              )
  structure(eed, class="eventExecutionData")
}
#####  END EXECUTION_DATA ##### }}}

##### ACCOUNT_VALUE #### {{{
`e_account_value` <-
function(msg, contents, ...) {
  # key          value        currency
  c(contents[2], contents[3], contents[4])
}
##### END ACCOUNT_VALUE #### }}}


##### ACCOUNT_TIME #### {{{
`e_account_time` <-
function(msg, contents, ...) {
  contents[3]
}
##### END ACCOUNT_TIME #### }}}


##### PORTFOLIO_VALUE #### {{{
`e_portfolio_value` <-
function(msg, contents, ...) {
  version          <- as.numeric(contents[1])

  contract         <- twsContract()
 #contract$conId   <- contents[2]  NOT YET IMPLEMETED IN IBrokers
  contract$symbol  <- contents[3]
  contract$sectype <- contents[4]
  contract$expiry  <- contents[5]
  contract$strike  <- contents[6]
  contract$right   <- contents[7]
  contract$currency<- contents[8]
  contract$local   <- contents[9]
  
  portfolioValue <- list()
  portfolioValue$position      <- contents[10]
  portfolioValue$marketPrice   <- contents[11]
  portfolioValue$marketValue   <- contents[12]
  portfolioValue$averageCost   <- contents[13]
  portfolioValue$unrealizedPNL <- contents[14]
  portfolioValue$realizedPNL   <- contents[15]
  portfolioValue$accountName   <- contents[15]

  structure(list(contract       = contract,
                 portfolioValue = portfolioValue),
            class="eventPortfolioValue") 
}
##### END ACCOUNT_DATA #### }}}
