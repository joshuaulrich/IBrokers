placeOrder <- .placeOrder <-
function(twsconn,Contract,Order)
{
  if(!is.twsConnection(twsconn))
    stop('requires twsConnection object')

  if(!is.twsContract(Contract))
    stop('requires twsContract object for Contract arg')

  if(!inherits(Order, 'twsOrder'))
    stop('requires twsOrder object for Order arg')

  con <- twsconn[[1]]

  # This is the version of placeOrder() only. It has nothing to do with
  # the server version. But it does determine the fields that we need to
  # send for each order!
  # from official IB API

  #VERSION <- "28" # Version as of API 9.62
  #VERSION <- "29" # Version as of API 9.63
  #VERSION <- "30" # Version as of API 9.64
  VERSION <- "42"

  if(is.null(Order$hedgeType) || is.null(Order$hedgeParam))
    stop("NEW twsOrder has to be used")

  if(Order$orderId == "")
    Order$orderId <- reqIds(twsconn)

# write order {{{
  order <- c(.twsOutgoingMSG$PLACE_ORDER,
             VERSION,
             as.character(Order$orderId),
             as.character(Contract$conId),
             Contract$symbol,
             Contract$sectype,
             Contract$expiry,
             Contract$strike,
             Contract$right,
             Contract$multiplier,
             Contract$exch,
             Contract$primary,
             Contract$currency,
             Contract$local,
             {if(is.null(Contract$tradingClass)) "" else Contract$tradingClass},

             # as of 9.63
             Contract$secIdType,
             Contract$secId,

             Order$action,
             Order$totalQuantity,
             Order$orderType,
             Order$lmtPrice,
             Order$auxPrice,
             Order$tif,
             Order$ocaGroup,
             Order$account,
             Order$openClose,
             Order$origin,
             Order$orderRef,
             Order$transmit,
             Order$parentId,
             Order$blockOrder,
             Order$sweepToFill,
             Order$displaySize,
             Order$triggerMethod,
             Order$outsideRTH,
             Order$hidden)

  if(Contract$sectype == "BAG") {
    stop("BAG security type not supported")
    if(is.null(Contract$comboleg)) {
      order <- c(order, 0)
    } else {
      comboLeg <- Contract$comboleg
      order <- c(order, length(comboLeg))
      for(i in 1:length(comboLeg)) {
        Leg <- comboLeg[[i]]
        order <- c(order, 
                   Leg$conId, 
                   Leg$ratio, 
                   Leg$action, 
                   Leg$exch, 
                   Leg$openClose,
                   Leg$shortSaleSlot,
                   Leg$designatedLocation)
      }
    }   
  }

  order <- c(order,
             "", # DEPRECATED FIELD
             Order$discretionaryAmt,
             Order$goodAfterTime,
             Order$goodTillDate,
             Order$faGroup,
             Order$faMethod,
             Order$faPercentage,
             Order$faProfile,
             Order$shortSaleSlot,
             Order$designatedLocation,
             Order$exemptCode,
             Order$ocaType,
             Order$rule80A,
             Order$settlingFirm,
             Order$allOrNone,
             Order$minQty,
             Order$percentOffset,
             Order$eTradeOnly,
             Order$firmQuoteOnly,
             Order$nbboPriceCap,
             Order$auctionStrategy,
             Order$startingPrice,
             Order$stockRefPrice,
             Order$delta,
             Order$stockRangeLower,
             Order$stockRangeUpper,
             Order$overridePercentageConstraints,
             Order$volatility,
             Order$volatilityType,
             Order$deltaNeutralOrderType,
             Order$deltaNeutralAuxPrice,
             # ... some here if we have deltaNeutralOrderType. but not using it
             # "0",  #Order$deltaNeutralConId
             # "",   #Order$deltaNeutralSettlingFirm
             # "",   #Order$deltaNeutralClearingAccount
             # "",   #Order$deltaNeutralClearingIntent
             # "",   #Order$deltaNeutralOpenClose
             # "0",   #Order$deltaNeutralShortSale
             # "0",  #Order$deltaNeutralShortSaleSlot
             # "",   #Order$deltaNeutralDesignatedLocation
             Order$continuousUpdate,
             Order$referencePriceType,
             Order$trailStopPrice,
             Order$trailingPercent,
             Order$scaleInitLevelSize,
             Order$scaleSubsLevelSize,
             Order$scalePriceIncrement,
             # .... some here if we have scalePriceIncrement. but not using it
             # if (Order$scalePriceIncrement != "")
             #   order <- c(order,
             #     "1.0", #Order$scalePriceAdjustValue
             #     "1", #Order$scalePriceAdjustInterval
             #     "1.0", #Order$scaleProfitOffset
             #     "0", #Order$scaleAutoReset
             #     "1", #Order$scaleInitPosition
             #     "1", #Order$scaleInitFillQty
             #     "0" #Order$scaleRandomPercent
             #   )
             Order$scaleTable,
             Order$activeStartTime,
             Order$activeStopTime)

  if(Order$hedgeType != "") {
    order <- c(order, Order$hedgeType, Order$hedgeParam)
  } else {
    order <- c(order, Order$hedgeType)
  }

  order <- c(order,
             Order$optOutSmartRouting,
             Order$clearingAccount,
             Order$clearingIntent,
             Order$notHeld,
             "0", # Order$underComp .. not yet supported by IBrokers
             Order$algoStrategy,
             Order$algoParams,
             Order$whatIf,
             "")
# }}}

  writeBin(order, con)  
  assign(".Last.orderId", as.integer(Order$orderId), .IBrokersEnv)
  invisible(as.integer(Order$orderId))
}

..placeOrder <-
function(conn,
         Contract,
         Order,
         verbose=TRUE, 
         eventExecutionData,
         eventOpenOrder,
         eventOrderStatus,
         CALLBACK, ...) {

  if(!is.twsConnection(conn))
    stop('requires twsConnection object')

  if(!is.twsContract(Contract))
    stop('requires twsContract object for Contract arg')

  if(!inherits(Order, 'twsOrder'))
    stop('requires twsOrder object for Order arg')

  if(missing(CALLBACK)) {
    if(missing(eventOrderStatus)) 
      eventOrderStatus   <- e_order_status

    if(missing(eventOpenOrder)) 
      eventOpenOrder     <- e_open_order

    if(missing(eventExecutionData))
      eventExecutionData <- e_execution_data
  }
  else if(is.null(CALLBACK)) {
    eventOrderStatus   <- NULL
    eventOpenOrder     <- NULL
    eventExecutionData <- NULL
  }

  con <- conn[[1]]

  VERSION <- "28" # Version as of API 9.62

# write order {{{
  order <- c(.twsOutgoingMSG$PLACE_ORDER,
             VERSION,
             as.character(Order$orderId),
             Contract$symbol,
             Contract$sectype,
             Contract$expiry,
             Contract$strike,
             Contract$right,
             Contract$multiplier,
             Contract$exch,
             Contract$primary,
             Contract$currency,
             Contract$local,
             Order$action,
             Order$totalQuantity,
             Order$orderType,
             Order$lmtPrice,
             Order$auxPrice,
             Order$tif,
             Order$ocaGroup,
             Order$account,
             Order$openClose,
             Order$origin,
             Order$orderRef,
             Order$transmit,
             Order$parentId,
             Order$blockOrder,
             Order$sweepToFill,
             Order$displaySize,
             Order$triggerMethod,
             Order$outsideRTH,
             Order$hidden,
             "", # DEPRECATED FIELD
             Order$discretionaryAmt,
             Order$goodAfterTime,
             Order$goodTillDate,
             Order$faGroup,
             Order$faMethod,
             Order$faPercentage,
             Order$faProfile,
             Order$shortSaleSlot,
             Order$designatedLocation,
             Order$ocaType,
             Order$rule80A,
             Order$settlingFirm,
             Order$allOrNone,
             Order$minQty,
             Order$percentOffset,
             Order$eTradeOnly,
             Order$firmQuoteOnly,
             Order$nbboPriceCap,
             Order$auctionStrategy,
             Order$startingPrice,
             Order$stockRefPrice,
             Order$delta,
             Order$stockRangeLower,
             Order$stockRangeUpper,
             Order$overridePercentageConstraints,
             Order$volatility,
             Order$volatilityType,
             Order$deltaNeutralOrderType,
             Order$deltaNeutralAuxPrice,
             Order$continuousUpdate,
             Order$referencePriceType,
             Order$trailStopPrice,
             Order$scaleInitLevelSize,
             Order$scaleSubsLevelSize,
             Order$scalePriceIncrement,
             Order$clearingAccount,
             Order$clearingIntent,
             Order$notHeld,
             "0", # Order$underComp .. not yet supported by IBrokers
             "",  # Order$algoStrategy .. not yet supported by IBrokers
             Order$whatIf
             )
# }}}

  writeBin(order, con)  

  waiting <- TRUE

  if(missing(CALLBACK) || is.null(CALLBACK)) {
    while(waiting) {
      curMsg <- readBin(con,character(),1)
          
      if (length(curMsg) > 0) {
        if (curMsg == .twsIncomingMSG$ERR_MSG) {
          if (!errorHandler(con, verbose, OK = c(165, 
              399, 300, 366, 2104, 2106, 2107))) {
              cat("\n")
              stop("Unable to complete market data request")
          }
        }
        if (curMsg == .twsIncomingMSG$OPEN_ORDER) {
          contents <- readBin(con, character(), 84)
          if (is.null(eventOrderStatus)) {
            cat(curMsg, paste(contents), "\n")
          } else cat(str(eventOpenOrder(curMsg, contents, ...)))
        }
        if (curMsg == .twsIncomingMSG$ORDER_STATUS) {
          contents <- readBin(con, character(), 11)
          if (is.null(eventOrderStatus)) {
            cat(curMsg, paste(contents), "\n")
          } else eventOrderStatus(curMsg, contents, ...)
        }
        if (curMsg == .twsIncomingMSG$EXECUTION_DATA) {
          contents <- readBin(con, character(), 21)
          if (is.null(eventOrderStatus)) {
            cat(curMsg, paste(contents), "\n")
          } else cat(str(eventExecutionData(curMsg, contents, ...)))
        }
      }
    }
  } else CALLBACK(con, ...)
}
