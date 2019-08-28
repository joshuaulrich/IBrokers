#####  TICK_PRICE ##### {{{
`e_tick_price`    <- function(msg,string,timeStamp,file,symbols,...) {
  tickType <- string[3]
  id <- as.numeric(string[2])
  file <- file[[id]]
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," symbol=",symbols[as.numeric(string[2])]," ",sep='',file=file,append=TRUE)
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

`e_tick_size`    <- function(msg,string,timeStamp,file,symbols,...) {
  tickType <- string[3] 
  id <- as.numeric(string[2])
  file <- file[[id]]
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," symbol=",symbols[as.numeric(string[2])]," ",sep='',file=file,append=TRUE)
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

`e_tick_option`  <- function(msg,string,timeStamp,file,symbols,...) {
  tickType <- string[3] 
  id <- as.numeric(string[2])
  file <- file[[id]]
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," symbol=",symbols[as.numeric(string[2])]," ",sep='',file=file,append=TRUE)
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
    cat('modelOption: impVol: ',string[4],' delta:',string[5],
        ' modelPrice: ',string[6],' pvDiv: ',string[7],
        ' gamma: ',string[8],' vega: ',string[9],
        ' theta: ',string[10],' undPrice: ',string[11],'\n',file=file,append=TRUE)
  } else {
    cat('<default option> ',file=file,append=TRUE)
    cat(paste(string),'\n',file=file,append=TRUE)
  }
}

`e_tick_generic` <- function(msg,string,timeStamp,file,symbols,...) {
  tickType <- string[3] 
  id <- as.numeric(string[2])
  file <- file[[id]]
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," symbol=",symbols[as.numeric(string[2])]," ",sep='',file=file,append=TRUE)
  if(tickType == .twsTickType$OPTION_IMPLIED_VOL) { #24
    cat('optionImpliedVol:',string[4],'\n',file=file,append=TRUE)
  } else 
  if(tickType == .twsTickType$OPTION_HISTORICAL_VOL) { #23
    cat('optionHistoricalVol:',string[4],string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$INDEX_FUTURE_PREMIUM) { #31
    cat('indexFuturePremium:',string[4],string[5],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$SHORTABLE) { #46
    cat('shortable:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$HALTED) { #49
    cat('Halted:',string[4],'\n',file=file,append=TRUE)
  } else {
    cat('<default generic>',file=file,append=TRUE)
    cat(paste(string),'\n',file=file,append=TRUE)
  }
}

`e_tick_string`  <- function(msg,string,timeStamp,file,symbols,...) {
  tickType <- string[3] 
  id <- as.numeric(string[2])
  file <- file[[id]]
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>',sep='',file=file,append=TRUE)
  cat(" id=",string[2]," symbol=",symbols[as.numeric(string[2])]," ",sep='',file=file,append=TRUE)
  if(tickType == .twsTickType$BID_EXCH) { #32
    cat('bidExchange:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$ASK_EXCH) { #33
    cat('askExchange:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$LAST_TIMESTAMP) { #45
    cat('lastTimestamp:',string[4],'\n',file=file,append=TRUE)
  } else
  if(tickType == .twsTickType$RT_VOLUME) { #48
    cat('RTVolume:',string[4],'\n',file=file,append=TRUE)
  } else {
    cat('<default string>',file=file,append=TRUE)
    cat(paste(string),'\n',file=file,append=TRUE)
  }
}

`e_tick_EFP`     <- function(msg,string,timeStamp,file,symbols,...) {
file <- file[[1]]  # FIXME
    cat('<default EFP> ',file=file,append=TRUE)
    cat(paste(string),'\n',file=file,append=TRUE)
}

######################################################################
#
#  default event handlers for reqMktDepth
#
######################################################################

`e_update_mkt_depth` <-  function(msg,contents,timeStamp,file,...) {
  id <- as.numeric(contents[2])
  file <- file[[id]]
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>,',sep='',file=file,append=TRUE)
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
  id <- contents[2]
  if(!is.null(timeStamp)) cat('<',as.character(timeStamp),'>,',sep='',file=file,append=TRUE)
  id <- as.numeric(contents[2])
  file <- file[[id]]
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

`e_real_time_bars` <- function(curMsg, msg, symbols, file, ...) {
  # msg[1] is VERSION
  columns <- c("Id","time","open","high","low","close","volume",
               "wap","count")
  id <- as.numeric(msg[2])
  file <- file[[id]]
  msg[2] <- symbols[as.numeric(msg[2])]
  msg[3] <- strftime(structure(as.numeric(msg[3]), class=c("POSIXt","POSIXct")))
  cat(paste(columns,"=",msg[-1],sep=""),'\n',file=file,append=TRUE)
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
  eos <- structure(eos,class="eventOrderStatus")
  cat("TWS OrderStatus:",
      paste("orderId=",eos$orderId,sep=""),
      paste("status=",eos$status,sep=""),
      paste("filled=",eos$filled,sep=""),
      paste("remaining=",eos$remaining,sep=""),
      paste("averageFillPrice=",eos$averageFillPrice,sep=""),"\n")
  eos
}


`e_open_order` <- function(msg) {
  contents <- msg
  eoo <- list(
         # need to add contractId to twsContract...
              contract   = twsContract(
                             conId   = contents[3],
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
                             percentOffset = contents[36],
                             settlingFirm  = contents[37],
                             shortSaleSlot = contents[38],
                             designatedLocation = contents[39],
                             auctionStrategy = contents[40],
                             startingPrice = contents[41],
                             stockRefPrice = contents[42],
                             delta         = contents[43],
                             stockRangeLower = contents[44],
                             stockRangeUpper = contents[45],
                             displaySize   = contents[46],
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
                             scaleInitLevelSize = contents[67],
                             scaleSubsLevelSize = contents[68],
                             scalePriceIncrement = contents[69],
                             clearingAccount = contents[70],
                             clearingIntent  = contents[71],
                             notHeld         = contents[72],
                             # this contingent on UnderComp Not Yet Available in IBrokers [74+]
                             # algoStrategy [75+]
                             whatIf          = contents[75]
                           ),

              orderstate = twsOrderState(
                             status      = contents[76],
                             initMargin  = contents[77],
                             maintMargin = contents[78],
                             equityWithLoan = contents[79],
                             commission  = contents[80],
                             minCommission = contents[81],
                             maxCommission = contents[82],
                             commissionCurrency = contents[83],
                             warningText = contents[84]
                           )
         )
  eoo <- structure(eoo, class='eventOpenOrder')
  cat("TWS OpenOrder:",
      paste("orderId=",eoo$order$orderId,sep=""),
      paste("conId=",eoo$contract$conId,sep=""),
      paste("symbol=",eoo$contract$symbol,sep=""),
      paste("status=",eoo$orderstate$status,sep=""),"\n")
  eoo
}

#####  EXECUTION_DATA ##### {{{
e_execDetails <- e_execution_data <- function(curMsg, msg, file, ...) {
       version = msg[1]
       reqId   = msg[2]
       orderId   = msg[3]
       eed <- list(
              contract   = twsContract(
                             conId   = msg[4],
                             symbol  = msg[5],
                             sectype = msg[6],
                             expiry  = msg[7],
                             strike  = msg[8],
                             right   = msg[9],
                             exch    = msg[10],
                             currency= msg[11],
                             local   = msg[12],
                             # the following are required to correctly specify a contract
                             combo_legs_desc = NULL,
                             primary = NULL,
                             include_expired = NULL,
                             comboleg = NULL,
                             multiplier = NULL
                           ),
              execution  = twsExecution(orderId = orderId,
                                        execId  = msg[13],
                                        time    = msg[14],
                                        acctNumber = msg[15],
                                        exchange   = msg[16],
                                        side       = msg[17],
                                        shares     = msg[18],
                                        price      = msg[19],
                                        permId     = msg[20],
                                        clientId   = msg[21],
                                        liquidation= msg[22],
                                        cumQty     = msg[23],
                                        avgPrice   = msg[24]
                                       )
              )
  eed <- structure(eed, class="eventExecutionData")
  cat("TWS Execution:", 
      paste("orderId=",eed$execution$orderId,sep=""),
      paste("time=",strptime(eed$execution$time,"%Y%m%d  %H:%M:%S"),sep=""),
      paste("side=",eed$execution$side,sep=""),
      paste("shares=",eed$execution$shares,sep=""),
      paste("symbol=",eed$contract$symbol,sep=""),
      paste("conId=",eed$contract$conId,sep=""),
      paste("price=",eed$execution$price,sep=""),"\n")
  eed
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
  contents[2]
}
##### END ACCOUNT_TIME #### }}}


##### PORTFOLIO_VALUE #### {{{
`e_portfolio_value` <-
function(curMsg, msg, ...) {
  version          <- as.numeric(msg[1])

  contract         <- twsContract(conId=msg[2],
                                  symbol=msg[3],
                                  sectype=msg[4],
                                  exch=NULL,
                                  primary=msg[9],
                                  expiry=msg[5],
                                  strike=msg[6],
                                  currency=msg[10],
                                  right=msg[7],
                                  local=msg[11],
                                  multiplier=msg[8],
                                  combo_legs_desc=NULL,comboleg=NULL,include_expired=NULL)
#  contract$conId   <- msg[2]
#  contract$symbol  <- msg[3]
#  contract$sectype <- msg[4]
#  contract$expiry  <- msg[5]
#  contract$strike  <- msg[6]
#  contract$right   <- msg[7]
#  contract$multiplier <- msg[8]
#  contract$primary <- msg[9]
#  contract$currency<- msg[10]
#  contract$local   <- msg[11]
  
  portfolioValue <- list()
  portfolioValue$position      <- msg[12]
  portfolioValue$marketPrice   <- msg[13]
  portfolioValue$marketValue   <- msg[14]
  portfolioValue$averageCost   <- msg[15]
  portfolioValue$unrealizedPNL <- msg[16]
  portfolioValue$realizedPNL   <- msg[17]
  portfolioValue$accountName   <- msg[18]

  p <- structure(list(contract       = contract,
                 portfolioValue = portfolioValue),
            class="eventPortfolioValue") 
  str(p)
  p
}
##### END ACCOUNT_DATA #### }}}

e_scannerData <- function(curMsg, reqId, rank, contract,
                          distance, benchmark, projection, legsStr) {
  con <- contract$contract
  cat("id=",reqId,
      " rank=",rank,
      " symbol=",con$symbol,
      " expiry=",con$expiry,
      " strike=",con$strike,
      " right=",con$right,
      " exchange=",con$exch,
      " currency=",con$currency,
      " local=",con$local,
      " marketName=",contract$marketName,
      " tradingClass=",contract$tradingClass,
      " distance=",distance,
      " benchmark=",benchmark,
      " projection=",projection,
      " legsStr=",legsStr,"\n")
}

e_fundamentalData <- function(curMsg, msg) {
  cat( paste("id=",msg[2]," len=",nchar(msg[3]),"\n",msg[3], sep="") )
} 
