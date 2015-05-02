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
twsCALLBACK <- function (twsCon, eWrapper, timestamp, file, playback = 1, ...) 
{
    if (missing(eWrapper)) 
        eWrapper <- eWrapper()
    con <- twsCon[[1]]
    if (inherits(twsCon, "twsPlayback")) {
		stop('No playback support')
    }
    else {
        tryCatch(while (isConnected(twsCon)) {
            if (!socketSelect(list(con), FALSE, 0.25)) 
                next
            curMsg <- readBin(con, "character", 1L)
            if (!is.null(timestamp)) {
                processMsg(curMsg, con, eWrapper, format(Sys.time(), 
                  timestamp), file, twsCon, ...)
            }
            else {
                processMsg(curMsg, con, eWrapper, timestamp, 
                  file, twsCon, ...)
            }
        }, error = function(e) {
            close(twsCon)
            print(e)
            stop("IB connection error. Connection closed", call. = FALSE)
        })
    }
}





processMsg <- function (curMsg, con, eWrapper, timestamp, file, twsconn, ...)
{
    if (curMsg == .twsIncomingMSG$TICK_PRICE) { #v63 length OK.
        msg <- .Internal(readBin(con, "character", 6L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickPrice(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_SIZE) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 4L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickSize(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ORDER_STATUS) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 11L, NA_integer_,
            TRUE, FALSE))
        eWrapper$orderStatus(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ERR_MSG) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 4L, NA_integer_,
            TRUE, FALSE))
        eWrapper$errorMessage(curMsg, msg, timestamp, file, twsconn,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$OPEN_ORDER) {  #v63 length ADAPTED.
        #msg <- .Internal(readBin(con, "character", 101L, NA_integer_, TRUE, FALSE))
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        version <- as.integer(msg[1])        
		msg <- c(msg, .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE)) )  
		if (version >= 32) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )  
		}
		msg <- c(msg, .Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) )  
		if (version >= 32) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )  
		}		
		msg <- c(msg, tmp <-.Internal(readBin(con, "character", 50L, NA_integer_, TRUE, FALSE)) )  
		
		order.deltaNeutralOrderType <- tmp[49]
#cat('order.deltaNeutralOrderType',tmp[49],'\n')
		if (order.deltaNeutralOrderType != '') {
			if (version >= 27 & !is.na(order.deltaNeutralOrderType)) {
				msg <- c(msg, .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE)) )  
			}
			if (version >= 31 & !is.na(order.deltaNeutralOrderType)) {
				msg <- c(msg, .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE)) )  
			}
		}
		
		msg <- c(msg, tmp <-.Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) )  
		if (version >= 30) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		msg <- c(msg, tmp <-.Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) ) 	
		if (version >= 29) {		
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
#cat(tmp[1],'\n')			
			if (tmp[1]!='') {
				comboLegsCount <- as.integer(tmp[1])		
				if (comboLegsCount > 0) {
					msg <- c(msg, .Internal(readBin(con, "character", comboLegsCount*8L, NA_integer_, TRUE, FALSE)) )
				}
			}
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			if (tmp[1]!='') {
				orderComboLegsCount <- as.integer(tmp[1])					
				if (orderComboLegsCount > 0) {
					msg <- c(msg, .Internal(readBin(con, "character", orderComboLegsCount*1L, NA_integer_, TRUE, FALSE)) )
				}
			}
			
		}		
		if (version >= 26) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			if (tmp[1]!='') {
				smartComboRoutingParamsCount <- as.integer(tmp[1])
				if( smartComboRoutingParamsCount > 0) {
					msg <- c(msg, .Internal(readBin(con, "character", smartComboRoutingParamsCount*2L, NA_integer_, TRUE, FALSE)) )
				}
			}
		}	
		msg <- c(msg, tmp <-.Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) ) 
		order.scalePriceIncrement <- as.integer(tmp[3])
#cat('order.scalePriceIncrement','\n')				
#cat(tmp[3],'\n')		
		if (!is.na(order.scalePriceIncrement)) {
			if (version >= 28 & order.scalePriceIncrement > 0.0 ) {
				msg <- c(msg, .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE)) )
			}		
		}
		if( version >= 24) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			order.hedgeType <- tmp[1]
#cat('order.hedgeType','\n')				
#cat(tmp[1],'\n')					
			if( order.hedgeType != '' ) {
				msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			}
		}
		if( version >= 25) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		msg <- c(msg, tmp<-.Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		if( version >= 22) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}		
		if( version >= 20) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			underCompPresent <- as.integer(tmp[1])
			if( !is.na(underCompPresent)) {
				if (underCompPresent > 0) {
					msg <- c(msg, tmp<-.Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) )
				}
			}
		}
		if( version >= 21) {
			msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
			order.algoStrategy <- tmp[1]
			if( order.algoStrategy != '' ) {
				msg <- c(msg, tmp<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
				algoParamsCount <- as.integer(tmp[1])
				if( algoParamsCount > 0) {
					msg <- c(msg, tmp<-.Internal(readBin(con, "character", algoParamsCount*2L, NA_integer_, TRUE, FALSE)) )
				}
			}
		}
		msg <- c(msg, tmp<-.Internal(readBin(con, "character", 10L, NA_integer_, TRUE, FALSE)) )
		
		## Must read 8 more fields here .... !!! doing it in order.deltaNeutralOrderType. now OK !!!
		
#cat('FINAL',msg,'\n',sep='-')	
#cat(length(msg),'\n')		
#cat( paste(1:length(msg),msg,sep=':'), sep='\n')

        eWrapper$openOrder(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_VALUE) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 5L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updateAccountValue(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) { #v63 length ADAPTED.
        msg <- .Internal(readBin(con, "character", 19L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updatePortfolio(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updateAccountTime(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$nextValidId(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {  #v63 length ADAPTED.
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        version <- as.integer(msg[1])
        if( version >= 3) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		msg <- c(msg, .Internal(readBin(con, "character", 16L, NA_integer_, TRUE, FALSE)) )
		if( version >= 4) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 5) {
			msg <- c(msg, .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 6) {
			msg <- c(msg, .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 8) {
			msg <- c(msg, .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		}				
		if( version >= 7) {
			msg <- c(msg, secIdListCount<-.Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )		
			if( as.integer(secIdListCount) > 0) {
				msg <- c(msg, .Internal(readBin(con, "character", as.integer(secIdListCount)*2L, NA_integer_, TRUE, FALSE))  )
			}
		}
        eWrapper$contractDetails(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$EXECUTION_DATA) {  #v63 length ADAPTED.
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        version <- as.integer(msg[1])
        if( version >= 7) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
        msg <- c(msg, .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE)) )        
		if( version >= 9) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		msg <- c(msg, .Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE)) )
		if (version >= 10) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
        msg <- c(msg, .Internal(readBin(con, "character", 10L, NA_integer_, TRUE, FALSE)) )    
		if( version >= 6) {
			msg <- c(msg, .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 8) {
			msg <- c(msg, .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE)) )
		}
		if( version >= 9) {
			msg <- c(msg, .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE)) )
		} 
        eWrapper$execDetails(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$MARKET_DEPTH) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 7L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updateMktDepth(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 8L, NA_integer_,
            TRUE, FALSE))
        eWrapper$updateMktDepthL2(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$NEWS_BULLETINS) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 5L, NA_integer_,
            TRUE, FALSE))
        eWrapper$newsBulletins(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$managedAccounts(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$RECEIVE_FA) {  #v63 length ADAPTED, but xml implementation pending
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        stop("xml data currently unsupported")
        eWrapper$receiveFA(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$HISTORICAL_DATA) {  #v63 length OK.
        header <- readBin(con, character(), 5)
        nbin <- as.numeric(header[5]) * 9
        msg <- .Internal(readBin(con, "character", as.integer(nbin),
            NA_integer_, TRUE, FALSE))
        eWrapper$historicalData(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) { #Implementation pending. not trading.
        stop("unimplemented as of yet")
        eWrapper$bondContractDetails(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) {  #v63 length OK.
         version <- readBin(con, character(), 1L)
         msg <- readBin(con, raw(), 1000000L)
        eWrapper$scannerParameters(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$SCANNER_DATA) { #v63 length OK.
        cD <- twsContractDetails()
        version <- readBin(con, character(), 1L)
        tickerId <- readBin(con, character(), 1L)
        numberOfElements <- as.integer(readBin(con, character(), 1L))
        for (i in 1:numberOfElements) {
            msg <- readBin(con, character(), 16L)
            rank <- msg[1]
            cD$contract$conId <- msg[2]
            cD$contract$symbol <- msg[3]
            cD$contract$sectype <- msg[4]
            cD$contract$expiry <- msg[5]
            cD$contract$strike <- msg[6]
            cD$contract$right <- msg[7]
            cD$contract$exch <- msg[8]
            cD$contract$currency <- msg[9]
            cD$contract$local <- msg[10]
            cD$marketName <- msg[11]
            cD$tradingClass <- msg[12]
            distance <- msg[13]
            benchmark <- msg[14]
            projection <- msg[15]
            legsStr <- msg[16]
            eWrapper$scannerData(curMsg, tickerId, rank, cD,
                distance, benchmark, projection, legsStr)
        }
    }
    else if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 11L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickOptionComputation(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_GENERIC) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 4L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickGeneric(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_STRING) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 4L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickString(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_EFP) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 10L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickEFP(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$CURRENT_TIME) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$currentTime(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) { #v63 length OK.
        msg <- .Internal(readBin(con, "character", 10L, NA_integer_,
            TRUE, FALSE))
        eWrapper$realtimeBars(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$fundamentalData(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$contractDetailsEnd(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$OPEN_ORDER_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_,
            TRUE, FALSE))
        eWrapper$openOrderEnd(curMsg, msg, timestamp, file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$accountDownloadEnd(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$execDetailsEnd(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 5L, NA_integer_,
            TRUE, FALSE))
        eWrapper$deltaNeutralValidation(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {  #v63 length OK.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$tickSnapshotEnd(curMsg, msg, timestamp, file,
            ...)
    }
    else if (curMsg == .twsIncomingMSG$MARKET_DATA_TYPE) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$marketDataType(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$COMMISSION_REPORT) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 7L, NA_integer_,
            TRUE, FALSE))
        eWrapper$commissionReport(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$POSITION_DATA) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 15L, NA_integer_,
            TRUE, FALSE))
        eWrapper$positionData(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$POSITION_END) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 1L, NA_integer_,
            TRUE, FALSE))
        eWrapper$positionEnd(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCOUNT_SUMMARY) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 6L, NA_integer_,
            TRUE, FALSE))
        eWrapper$accountSummary(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$ACCOUNT_SUMMARY_END) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$accountSummaryEnd(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$VERIFY_MESSAGE_API) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 2L, NA_integer_,
            TRUE, FALSE))
        eWrapper$verifyMessageAPI(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$VERIFY_COMPLETED) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$verifyCompleted(curMsg, msg, timestamp,
            file, ...)  # ===> calls startApi() in the cpp implementation.
    }
    else if (curMsg == .twsIncomingMSG$DISPLAY_GROUP_LIST) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$displayGroupList(curMsg, msg, timestamp,
            file, ...)
    }
    else if (curMsg == .twsIncomingMSG$DISPLAY_GROUP_UPDATED) {  #v63 NEW.
        msg <- .Internal(readBin(con, "character", 3L, NA_integer_,
            TRUE, FALSE))
        eWrapper$displayGroupUpdated(curMsg, msg, timestamp,
            file, ...)
    }
    else {
        warning(paste("Unknown incoming message: ", curMsg, ". Please reset connection",
            sep = ""), call. = FALSE)
    }
}


