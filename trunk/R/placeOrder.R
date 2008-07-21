`placeOrder` <-
function(conn,
         Contract,
         Order,
         verbose=TRUE, 
         eventExecutionData,
         eventOrderStatus,
         CALLBACK, ...) {

  if(!inherits(conn,'twsConnection'))
    stop('requires twsConnection object')

  if(!inherits(Contract, 'twsContract'))
    stop('requires twsContract object for Contract arg')

  if(!inherits(Order, 'twsOrder'))
    stop('requires twsOrder object for Order arg')

  if(missing(CALLBACK)) {
    if(missing(eventOrderStatus)) 
      eventOrderStatus <- e_order_status
    if(missing(eventExecutionData))
      eventExecutionData <- e_execution_data
  }
  else if(is.null(CALLBACK)) {
    eventOrderStatus <- NULL
    eventExecutionData <- NULL
  }

  con <- conn[[1]]

  VERSION <- "25" # Version as of API 9.40

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
             Order$scaleNumComponents,
             Order$scaleComponentSize,
             Order$scalePriceIncrement,
             Order$clearingAccount,
             Order$clearingIntent,
             Order$whatIf
             )
# }}}

  writeBin(order, con)  

  waiting <- TRUE

  if(missing(CALLBACK) || is.null(CALLBACK)) {
    while(waiting) {
      curChar <- readBin(con,character(),1)
          
      if (length(curMsg) > 0) {
        if (curMsg == .twsIncomingMSG$ERR_MSG) {
          if (!errorHandler(con, verbose, OK = c(165, 
              300, 366, 2104, 2106, 2107))) {
              cat("\n")
              stop("Unable to complete market data request")
          }
        }
        if (curMsg == .twsIncomingMSG$OPEN_ORDER) {
          contents <- readBin(con, character(), 11)
          if (is.null(eventOrderStatus)) {
            cat(curMsg, paste(contents), "\n")
          } else eventOpenOrder(curMsg, contents, ...)
        }
        if (curMsg == .twsIncomingMSG$ORDER_STATUS) {
          contents <- readBin(con, character(), 11)
          if (is.null(eventOrderStatus)) {
            cat(curMsg, paste(contents), "\n")
          } else eventOrderStatus(curMsg, contents, ...)
        }
        if (curMsg == .twsIncomingMSG$EXECUTION_DATA) {
          contents <- readBin(con, character(), 11)
          if (is.null(eventOrderStatus)) {
            cat(curMsg, paste(contents), "\n")
          } else eventExecutionData(curMsg, contents, ...)
        }
      }
    }
  } else CALLBACK(con, ...)
}
