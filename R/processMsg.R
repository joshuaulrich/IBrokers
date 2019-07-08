# single event-loop modeled after official TWS-API
# can be used as CALLBACK argument or modified
# to process selected/all possible incoming messages

twsDEBUG <- function(twsCon, eWrapper, timestamp, file, playback=1, ...)
{
  eWrapper <- eWrapper(debug=TRUE)
  twsCALLBACK(twsCon, eWrapper, timestamp, file, playback, ...)
}

#  tws <- twsConnect();CON2 <- twsConnect(2)
#  reqMktData(CON2, twsSTK("AAPL"), event=eWrapper(TRUE), CALLBACK=NA, tickerId="2")
#  reqMktData(tws, twsSTK("IBM"), event=eWrapper(symbols=c("AAPL","IBM")))
#  close(tws); close(CON2)
#  uncomment the elements of twsCALLBACK as appropriate
twsCALLBACK <- function(twsCon, eWrapper, timestamp, file, playback=1, ...)
{
  if(missing(eWrapper))
    eWrapper <- eWrapper()
  con <- twsCon[[1]]

  if(inherits(twsCon, 'twsPlayback')) {
    sys.time <- NULL
    while(TRUE) {
      if(!is.null(timestamp)) {
        # MktData
        last.time <- sys.time
        sys.time <- as.POSIXct(strptime(paste(readBin(con, character(), 2), collapse=' '), timestamp))
        if(!is.null(last.time)) {
          Sys.sleep((sys.time-last.time)*playback)
        }
        curMsg <- readBin(con, character(), 1)
        if(length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, format(sys.time, timestamp), file, ...)
      } else {
        # RealTimeBars
        curMsg <- readBin(con, character(), 1)
        if(length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
        if(curMsg == .twsIncomingMSG$REAL_TIME_BARS) Sys.sleep(5 * playback)
      }
    }
  } 
  else { 
    #dataCON <- get("DATACON", .GlobalEnv)[[1]]
    tryCatch(
    while(isConnected(twsCon)) {
      if( !socketSelect(list(con), FALSE, 0.25))
        next
      curMsg <- readBin(con, "character", 1L)
      if(!is.null(timestamp)) {
        processMsg(curMsg, con, eWrapper, format(Sys.time(), timestamp), file, twsCon, ...)
      } else {
        processMsg(curMsg, con, eWrapper, timestamp, file, twsCon, ...)
      }
    }, error=function(e) { close(twsCon); stop("IB connection error. Connection closed", call.=FALSE) }
    )
  }
}

processMsg <- function(curMsg, con, eWrapper, timestamp, file, twsconn, ...)
{
  if(curMsg == .twsIncomingMSG$TICK_PRICE) {
    msg <- readBin(con, "character", 6)
    eWrapper$tickPrice(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_SIZE) {
    msg <- readBin(con, "character", 4)
    eWrapper$tickSize(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ORDER_STATUS) {
    msg <- readBin(con, "character", 11)
    eWrapper$orderStatus(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ERR_MSG) {
    msg <- readBin(con, "character", 4)
    eWrapper$errorMessage(curMsg, msg, timestamp, file, twsconn, ...)
  } else
  if(curMsg == .twsIncomingMSG$OPEN_ORDER) {
    msg <- readBin(con, "character", 1)
    version <- as.integer(msg[1])
    msg <- c(msg, readBin(con, "character", 7))
    if(version >= 32) {
      msg <- c(msg, readBin(con, "character", 1))
    }
    msg <- c(msg, readBin(con, "character", 3))
    if(version >= 32) {
      msg <- c(msg, readBin(con, "character", 1))
    }
    msg <- c(msg, tmp <- readBin(con, "character", 50))
    order.deltaNeutralOrderType <- tmp[49]
    if(order.deltaNeutralOrderType != "") {
      if(version >= 27 & !is.na(order.deltaNeutralOrderType)) {
        msg <- c(msg, readBin(con, "character", 4))
      }
      if(version >= 31 & !is.na(order.deltaNeutralOrderType)) {
        msg <- c(msg, readBin(con, "character", 4))
      }
    }
    msg <- c(msg, tmp <- readBin(con, "character", 3))
    if(version >= 30) {
      msg <- c(msg, readBin(con, "character", 1))
    }
    msg <- c(msg, tmp <- readBin(con, "character", 3))
    if(version >= 29) {
      msg <- c(msg, tmp <- readBin(con, "character", 1))
      if(tmp[1] != "") {
        comboLegsCount <- as.integer(tmp[1])
        if(comboLegsCount > 0) {
          msg <- c(msg, readBin(con, "character", comboLegsCount * 8))
        }
      }
      msg <- c(msg, tmp <- readBin(con, "character", 1))
      if(tmp[1] != "") {
        orderComboLegsCount <- as.integer(tmp[1])
        if(orderComboLegsCount > 0) {
          msg <- c(msg, readBin(con, "character", orderComboLegsCount * 1L))
        }
      }
    }
    if(version >= 26) {
      msg <- c(msg, tmp <- readBin(con, "character", 1))
      if(tmp[1] != "") {
        smartComboRoutingParamsCount <- as.integer(tmp[1])
        if(smartComboRoutingParamsCount > 0) {
          msg <- c(msg, readBin(con, "character", smartComboRoutingParamsCount * 2L))
        }
      }
    }
    msg <- c(msg, tmp <- readBin(con, "character", 3))
    order.scalePriceIncrement <- as.integer(tmp[3])
    if(!is.na(order.scalePriceIncrement)) {
      if(version >= 28 & order.scalePriceIncrement > 0) {
        msg <- c(msg, readBin(con, "character", 7))
      }
    }
    if(version >= 24) {
      msg <- c(msg, tmp <- readBin(con, "character", 1))
      order.hedgeType <- tmp[1]
      if(order.hedgeType != "") {
        msg <- c(msg, tmp <- readBin(con, "character", 1))
      }
    }
    if(version >= 25) {
      msg <- c(msg, tmp <- readBin(con, "character", 1))
    }
    msg <- c(msg, tmp <- readBin(con, "character", 2))
    if(version >= 22) {
      msg <- c(msg, tmp <- readBin(con, "character", 1))
    }
    if(version >= 20) {
      msg <- c(msg, tmp <- readBin(con, "character", 1))
      underCompPresent <- as.integer(tmp[1])
      if(!is.na(underCompPresent)) {
        if(underCompPresent > 0) {
          msg <- c(msg, tmp <- readBin(con, "character", 3))
        }
      }
    }
    if(version >= 21) {
      msg <- c(msg, tmp <- readBin(con, "character", 1))
      order.algoStrategy <- tmp[1]
      if(order.algoStrategy != "") {
        msg <- c(msg, tmp <- readBin(con, "character", 1))
        algoParamsCount <- as.integer(tmp[1])
        if(algoParamsCount > 0) {
          msg <- c(msg, tmp <- readBin(con, "character", algoParamsCount * 2))
        }
      }
    }
    msg <- c(msg, tmp <- readBin(con, "character", 10))
    eWrapper$openOrder(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_VALUE) {
    msg <- readBin(con, "character", 5)
    eWrapper$updateAccountValue(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
    msg <- readBin(con, "character", 19)
    eWrapper$updatePortfolio(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
    #msg <- readBin(con, character(), 2)
    msg <- readBin(con, "character", 2)
    eWrapper$updateAccountTime(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
    #msg <- readBin(con, character(), 2)
    msg <- readBin(con, "character", 2)
    eWrapper$nextValidId(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CONTRACT_DATA) {
    #msg <- readBin(con, character(), 28)
    msg <- readBin(con, "character", 1)
    version <- as.integer(msg[1])
    if (version >= 3) {
        msg <- c(msg, readBin(con, "character", 1))
    }
    msg <- c(msg, readBin(con, "character", 16))
    if (version >= 4) {
        msg <- c(msg, readBin(con, "character", 1))
    }
    if (version >= 5) {
        msg <- c(msg, readBin(con, "character", 2))
    }
    if (version >= 6) {
        msg <- c(msg, readBin(con, "character", 7))
    }
    if (version >= 8) {
        msg <- c(msg, readBin(con, "character", 2))
    }
    if (version >= 7) {
        msg <- c(msg, secIdListCount <- readBin(con, "character", 1))
        if (as.integer(secIdListCount) > 0) {
            msg <- c(msg, readBin(con, "character", as.integer(secIdListCount) * 2))
        }
    }
    eWrapper$contractDetails(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$EXECUTION_DATA) {
    #msg <- readBin(con, character(), 24)
    msg <- readBin(con, "character", 1)
    version <- as.integer(msg[1])
    if (version >= 7) {
        msg <- c(msg, readBin(con, "character", 1))
    }
    msg <- c(msg, readBin(con, "character", 7))
    if (version >= 9) {
        msg <- c(msg, readBin(con, "character", 1))
    }
    msg <- c(msg, readBin(con, "character", 3))
    if (version >= 10) {
        msg <- c(msg, readBin(con, "character", 1))
    }
    msg <- c(msg, readBin(con, "character", 10))
    if (version >= 6) {
        msg <- c(msg, readBin(con, "character", 2))
    }
    if (version >= 8) {
        msg <- c(msg, readBin(con, "character", 1))
    }
    if (version >= 9) {
        msg <- c(msg, readBin(con, "character", 2))
    }
    eWrapper$execDetails(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DEPTH) {
    #msg <- readBin(con, character(), 7)
    msg <- readBin(con, "character", 7)
    eWrapper$updateMktDepth(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
    #msg <- readBin(con, character(), 8)
    msg <- readBin(con, "character", 8)
    eWrapper$updateMktDepthL2(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$NEWS_BULLETINS) {
    #msg <- readBin(con, character(), 5)
    msg <- readBin(con, "character", 5)
    eWrapper$newsBulletins(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
    #msg <- readBin(con, character(), 2)
    msg <- readBin(con, "character", 2)
    eWrapper$managedAccounts(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$RECEIVE_FA) {
    #msg <- readBin(con, character(), 2) # 3 with xml string
    msg <- readBin(con, "character", 3)
    stop("xml data currently unsupported")
    eWrapper$receiveFA(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
    header <- readBin(con, character(), 5)
    nbin <- as.numeric(header[5]) * 9
    msg <- readBin(con, character(), as.integer(nbin))
    eWrapper$historicalData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) {
    stop("BOND_CONTRACT_DATA unimplemented as of yet")
    eWrapper$bondContractDetails(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) {
    version <- readBin(con, character(), 1L) 
    msg <- readBin(con, raw(), 1e6L)
    eWrapper$scannerParameters(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$SCANNER_DATA) {
    cD <- twsContractDetails()
    version <- readBin(con, character(), 1L)
    tickerId <- readBin(con, character(), 1L)
    numberOfElements <- as.integer( readBin(con, character(), 1L) )
    for(i in 1:numberOfElements) {
      msg <- readBin(con, character(), 16L)
      rank <- msg[1]
        cD$contract$conId <- msg[2]
        cD$contract$symbol <- msg[3]
        cD$contract$sectype <- msg[4]
        cD$contract$expiry <- msg[5]
        cD$contract$strike <- msg[6]
        cD$contract$right  <- msg[7]
        cD$contract$exch  <- msg[8]
        cD$contract$currency  <- msg[9]
        cD$contract$local  <- msg[10]
      cD$marketName  <- msg[11]
      cD$tradingClass  <- msg[12]
      distance <- msg[13]
      benchmark <- msg[14]
      projection <- msg[15]
      legsStr <- msg[16]
      eWrapper$scannerData(curMsg, tickerId, rank, cD, distance,
                           benchmark, projection, legsStr)
    }
  } else
  if(curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
    msg <- readBin(con, "character", 11)
#    if(msg[3] == .twsTickType$MODEL_OPTION) {
#      #msg <- c(msg, readBin(con, character(), 2))
#    } else msg <- c(msg,NA,NA)
    eWrapper$tickOptionComputation(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_GENERIC) {
    msg <- readBin(con, "character", 4)
    eWrapper$tickGeneric(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_STRING) {
    msg <- readBin(con, "character", 4)
    eWrapper$tickString(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_EFP) {
    msg <- readBin(con, "character", 10)
    eWrapper$tickEFP(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CURRENT_TIME) {
    msg <- readBin(con, "character", 2)
    eWrapper$currentTime(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
    msg <- readBin(con, "character", 10)
    eWrapper$realtimeBars(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {
    msg <- readBin(con, "character", 3)
    eWrapper$fundamentalData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$contractDetailsEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$OPEN_ORDER_END) {
    msg <- readBin(con, "character", 1)
    eWrapper$openOrderEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$accountDownloadEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$execDetailsEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {
    msg <- readBin(con, "character", 5)
    eWrapper$deltaNeutralValidation(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$tickSnapshotEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$MARKET_DATA_TYPE) {
    msg <- readBin(con, "character", 3)
    eWrapper$marketDataType(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$COMMISSION_REPORT) {
    msg <- readBin(con, "character", 7)
    eWrapper$commissionReport(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$POSITION_DATA) {
    msg <- readBin(con, "character", 15)
    eWrapper$positionData(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$POSITION_END) {
    msg <- readBin(con, "character", 1)
    eWrapper$positionEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCOUNT_SUMMARY) {
    msg <- readBin(con, "character", 6)
    eWrapper$accountSummary(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$ACCOUNT_SUMMARY_END) {
    msg <- readBin(con, "character", 2)
    eWrapper$accountSummaryEnd(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$VERIFY_MESSAGE_API) {
    msg <- readBin(con, "character", 2)
    eWrapper$verifyMessageAPI(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$VERIFY_COMPLETED) {
    msg <- readBin(con, "character", 3)
    eWrapper$verifyCompleted(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$DISPLAY_GROUP_LIST) {
    msg <- readBin(con, "character", 3)
    eWrapper$displayGroupList(curMsg, msg, timestamp, file, ...)
  } else
  if(curMsg == .twsIncomingMSG$DISPLAY_GROUP_UPDATED) {
    msg <- readBin(con, "character", 3)
    eWrapper$displayGroupUpdated(curMsg, msg, timestamp, file, ...)
  } else {
    # default handler/error
    warning(paste("Unknown incoming message: ",curMsg,". Please reset connection",sep=""), call.=FALSE)
  }
  # end of messages
}
