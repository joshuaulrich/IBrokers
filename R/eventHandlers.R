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


`e_open_order` <- function(msg, contents, ...) {
  tmp <- NULL
  shift <- 0
  version <- as.integer(contents[1])
  if (version < 32) stop("e_open_order: only version>=32 supported at this time")
  if (contents[62] == "") stop('e_open_order: Expecting "None" or another non-empty value for for deltaNeutralOrderType')
  if (contents[79] != "0") stop("e_open_order: Expecting comboLegsCount==0. NO comboLegs supoprted at the moment")
  if (contents[80] != "0") stop("e_open_order: Expecting orderComboLegsCount==0. NO orderComboLegs supoprted at the moment")
  if (contents[81] != "0") stop("e_open_order: Expecting smartComboRoutingParamsCount==0. NO smartComboRoutingParams supoprted at the moment")
  if (contents[84] != "") stop('e_open_order: Expecting scalePriceIncrement=="". NO scalePriceIncrement supoprted at the moment')
  if (contents[85] == "") { # hedgeType=''
    if (contents[90] != "0") stop('e_open_order: Expecting underCompPresent=="0". NO underCompPresent supoprted at the moment')
    if (contents[91] != "") stop('e_open_order: Expecting algoStrategy=="". NO algoStrategy supoprted at the moment')
  } else { # hedgeType='P', etc.... inserts a shift by one for the fields that follow.
    shift <- 1
    if (contents[91] != "0") stop('e_open_order: Expecting underCompPresent=="0". NO underCompPresent supoprted at the moment')
    if (contents[92] != "") stop('e_open_order: Expecting algoStrategy=="". NO algoStrategy supoprted at the moment')
  }

  eoo <- list(
         # need to add contractId to twsContract...
              contract   = twsContract(
                             conId   = contents[3],
                             symbol  = contents[4],
                             sectype = contents[5],
                             expiry  = contents[6],
                             strike  = contents[7],
                             right   = contents[8],
                             multiplier = contents[9],
                             exch = contents[10],
                             currency = contents[11],
                             local = contents[12],
                             tradingClass = contents[13],
                             combo_legs_desc = contents[78], # value defined in order, further below...
                             primary = NULL,
                             include_expired = NULL,
                             comboleg = NULL
                           ),

              order      = twsOrder(
                             orderId = contents[2],
                             action = contents[14], # 14
                             totalQuantity = contents[15],
                             orderType = contents[16],
                             lmtPrice = contents[17],
                             auxPrice = contents[18],
                             tif = contents[19],
                             ocaGroup = contents[20],
                             account = contents[21],
                             openClose = contents[22],
                             origin = contents[23],
                             orderRef = contents[24],
                             clientId = contents[25],
                             permId = contents[26],
                             outsideRTH = contents[27],
                             hidden = contents[28],
                             discretionaryAmt = contents[29],
                             goodAfterTime = contents[30],
                             # 31 is deprecated
                             faGroup = contents[32],
                             faMethod = contents[33],
                             faPercentage = contents[34],
                             faProfile = contents[35],
                             goodTillDate = contents[36],
                             rule80A = contents[37],
                             percentOffset = contents[38],
                             settlingFirm = contents[39],
                             shortSaleSlot = contents[40],
                             designatedLocation = contents[41],
                             exemptCode = contents[42],
                             auctionStrategy = contents[43],
                             startingPrice = contents[44],
                             stockRefPrice = contents[45],
                             delta = contents[46],
                             stockRangeLower = contents[47],
                             stockRangeUpper = contents[48],
                             displaySize = contents[49],
                             blockOrder = contents[50],
                             sweepToFill = contents[51],
                             allOrNone = contents[52],
                             minQty = contents[53],
                             ocaType = contents[54],
                             eTradeOnly = contents[55],
                             firmQuoteOnly = contents[56],
                             nbboPriceCap = contents[57],
                             parentId = contents[58],
                             triggerMethod = contents[59],
                             volatility = contents[60],
                             volatilityType = contents[61],
                             deltaNeutralOrderType = contents[62],
                             deltaNeutralAuxPrice = contents[63], #

                             deltaNeutralConId = contents[64],
                             deltaNeutralSettlingFirm = contents[65],
                             deltaNeutralClearingAccount = contents[66],
                             deltaNeutralClearingIntent = contents[67],

                             deltaNeutralOpenClose = contents[68],
                             deltaNeutralShortSale = contents[69],
                             deltaNeutralShortSaleSlot = contents[70],
                             deltaNeutralDesignatedLocation = contents[71],

                             continuousUpdate = contents[72],
                             referencePriceType = contents[73],
                             trailStopPrice = contents[74],

                             trailingPercent = contents[75],

                             basisPoints = contents[76],
                             basisPointsType = contents[77],
                             # comboLegsDescrip = contents[78], # 78  # see above in Contract...combo_legs_desc

                             comboLegsCount = contents[79], # 79
                             comboLegs = NULL,
      # ~                      comboLegs = if (version >= 29 & !is.null(tmp)) {
      # ~                                                      if (as.integer(tmp)>0) {
      # ~                                                              shift <- shift + 1   #does not work. shift is only modified locally !?
      # ~                                                              ll <- list()
      # ~                                                              ll[1:(8L*as.integer(tmp))] <- contents[shift+66 +1:(8L*as.integer(tmp))]
      # ~                                                              shift <- shift + 8L*as.integer(tmp) -1
      # ~                                                              ll #not entirely correct since this should be a list of lists of length 8...
      # ~                                                      } else NULL
      # ~                                              }  else NULL,

                             orderComboLegsCount = contents[80], # 80
                             orderComboLegs = NULL,
      # ~                      orderComboLegs = if (version >= 29 & !is.null(tmp)) {
      # ~                                                              if (as.integer(tmp)>0) {
      # ~                                                                      shift <- shift + 1 #does not work. shift is only modified locally !?
      # ~                                                                      ll <- list()
      # ~                                                                      ll[1:(1L*as.integer(tmp))] <- contents[shift+66 +1:(1L*as.integer(tmp))]
      # ~                                                                      shift <- shift + 1L*as.integer(tmp) -1
      # ~                                                                      ll #not entirely correct since this should be a list of lists of length 1...
      # ~                                                              } else NULL
      # ~                                                      }  else NULL,

                             smartComboRoutingParamsCount = contents[81], # 81
                             smartComboRoutingParams = NULL,
      # ~                      smartComboRoutingParams = if (version >= 26 & !is.null(tmp)) {
      # ~                                                              if (as.integer(tmp)>0) {
      # ~                                                                      shift <- shift + 1 #does not work. shift is only modified locally !?
      # ~                                                                      ll <- list()
      # ~                                                                      ll[1:(2L*as.integer(tmp))] <- contents[shift+66 +1:(2L*as.integer(tmp))]
      # ~                                                                      shift <- shift + 2L*as.integer(tmp) -1
      # ~                                                                      ll #not entirely correct since this should be a list of lists of length 2...
      # ~                                                              } else NULL
      # ~                                                      }  else NULL,

                             scaleInitLevelSize = contents[82],
                             scaleSubsLevelSize = contents[83],
                             scalePriceIncrement = contents[84],

                             scalePriceAdjustValue = NULL,
                             scalePriceAdjustInterval = NULL,
                             scaleProfitOffset = NULL,
                             scaleAutoReset = NULL,
                             scaleInitPosition = NULL,
                             scaleInitFillQty = NULL,
                             scaleRandomPercent = NULL,

                             hedgeType = contents[85] -> tmp,
                             hedgeParam = if (tmp != "") contents[86] else NULL,

                             optOutSmartRouting = contents[shift + 86],

                             clearingAccount = contents[shift + 87],

                             # clearingIntent = {cat('clearingIntent',contents[shift+88],' shift:',shift,'\n');contents[shift+88]},
                             clearingIntent = contents[shift + 88],

                             # notHeld = {cat('notHeld',contents[shift+89],'\n');contents[shift+89]},
                             notHeld = contents[shift + 89],

                             # underCompPresent = contents[shift+90],
                             # underCompConId = NULL,
      # ~                      underCompConId = if (!is.null(tmp)) {
      # ~                                      if (!is.na(tmp2<-as.integer(tmp))) {
      # ~                                              if (tmp2>0) {shift<-shift+1;contents[shift+72]} else NULL  #does not work. shift is only modified locally !?
      # ~                                      } else NULL
      # ~                              } else NULL,
                             # underCompDelta = NULL,
      # ~                      underCompDelta = if (!is.null(tmp)) {
      # ~                                      if (!is.na(tmp2<-as.integer(tmp))) {
      # ~                                              if (tmp2>0) {shift<-shift+1;contents[shift+72]} else NULL #does not work. shift is only modified locally !?
      # ~                                      } else NULL
      # ~                              } else NULL,
                             # underCompPrice =NULL,
      # ~                      underCompPrice = if (!is.null(tmp)) {
      # ~                                      if (!is.na(tmp2<-as.integer(tmp))) {
      # ~                                              if (tmp2>0) {shift<-shift+1;contents[shift+72]} else NULL  #does not work. shift is only modified locally !?
      # ~                                      } else NULL
      # ~                              } else NULL,

                             # algoStrategy = {cat('algoStrategy',contents[shift+91],'\n');contents[shift+91]},
                             algoStrategy = contents[shift + 91],

      # algoParamsCount = (if (!is.null(tmp)) {if (tmp!='') {shift<-shift+1;contents[shift+72]} else NULL} else NULL)->tmp2,
      # ~                      algoParams = if (!is.null(tmp2)) {
      # ~                                                              if (as.integer(tmp2)>0) {
      # ~                                                                      shift <- shift + 1  #does not work. shift is only modified locally !?
      # ~                                                                      ll <- list()
      # ~                                                                      ll[1:(2L*as.integer(tmp2))] <- contents[shift+72 +1:(2L*as.integer(tmp2))]
      # ~                                                                      shift <- shift + 2L*as.integer(tmp2) -1
      # ~                                                                      ll #not entirely correct since this should be a list of lists of length 2...
      # ~                                                              } else NULL
      # ~                                                      }  else NULL,

                             # whatIf = {cat('whatIf',contents[shift+92],'\n');contents[shift+92]}
                             whatIf = contents[shift + 92]
                           ),

              orderstate = twsOrderState(
                             status = contents[shift + 93], # Submitted, etc...!!!
                             initMargin = contents[shift + 94],
                             maintMargin = contents[shift + 95],
                             equityWithLoan = contents[shift + 96],
                             commission = contents[shift + 97],
                             minCommission = contents[shift + 98],
                             maxCommission = contents[shift + 99],
                             commissionCurrency = contents[shift + 100],
                             warningText = contents[shift + 101]
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
                             multiplier = msg[10], # NEW
                             exch = msg[11],
                             currency = msg[12],
                             local = msg[13],
                             tradingClass = msg[14],
                             combo_legs_desc = NULL,
                             primary = NULL,
                             include_expired = NULL,
                             comboleg = NULL
                           ),
              execution  = twsExecution(orderId = orderId,
                                        execId  = msg[15],
                                        time    = msg[16],
                                        acctNumber = msg[17],
                                        exchange   = msg[18],
                                        side       = msg[19],
                                        shares     = msg[20],
                                        price      = msg[21],
                                        permId     = msg[22],
                                        clientId   = msg[23],
                                        liquidation= msg[24],
                                        cumQty     = msg[25],
                                        avgPrice   = msg[26],
                                        orderRef   = msg[27],
                                        evRule     = msg[28],
                                        evMultiplier = msg[29]
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

#####  COMMISSION_REPORT  ##### {{{
e_commissionReport <- function(curMsg, msg, file, ...) {
  # ~                          DECODE_FIELD( version);
  # ~                          DECODE_FIELD( commissionReport.execId);
  # ~                          DECODE_FIELD( commissionReport.commission);
  # ~                          DECODE_FIELD( commissionReport.currency);
  # ~                          DECODE_FIELD( commissionReport.realizedPNL);
  # ~                          DECODE_FIELD( commissionReport.yield);
  # ~                          DECODE_FIELD( commissionReport.yieldRedemptionDate);
  version <- msg[1]
  execId <- msg[2]
  ecr <- list(
    commission = msg[3],
    currency = msg[4],
    realizedPNL = msg[5],
    yield = msg[6],
    yieldRedemptionDate = msg[7]
  )
  ecr <- structure(ecr, class = "eventCommissionReport")
  cat(
    "TWS Commission:",
    paste("commission=", ecr$commission, sep = ""),
    paste("currency=", ecr$currency, sep = ""),
    "\n"
  )
  ecr
}
##### END COMMISSION_REPORT  ##### }}}

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
