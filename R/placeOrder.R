`placeOrder` <-
function(conn, Contract, Order, verbose=TRUE) {
  if(!inherits(conn,'twsConnection'))
    stop('requires twsConnection object')

  con <- conn[[1]]
  VERSION <- "26"

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
             "",
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
             Order$nbboPricecap,
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
             "0", #FALSE
             "",  #??????
             Order$whatIf
             )

  writeBin(order, con)  


  waiting <- TRUE

  while(waiting) {
    curChar <- readBin(con,character(),1)
        
    if(length(curChar) > 0) {
      if(curChar==.twsIncomingMSG$ERR_MSG) {
        if(!errorHandler(con, verbose, OK = c(165, 202, 300, 366, 2104,2106,2107))) {
          stop("Unable to complete TWS request")
        }   
      }   

      if(curChar==.twsIncomingMSG$ORDER_STATUS) {
        orderStatus <- readBin(con, character(), 11)
        ### need an orderStatus handler here
        waiting <- FALSE
      }
    }   
  }
  return(orderStatus)
}
