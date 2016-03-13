placeOrder <-
function (twsconn, Contract, Order) 
{
    if (!is.twsConnection(twsconn)) 
        stop("requires twsConnection object")
    if (!is.twsContract(Contract)) 
        stop("requires twsContract object for Contract arg")
    if (!inherits(Order, "twsOrder")) 
        stop("requires twsOrder object for Order arg")
    con <- twsconn[[1]]
    VERSION <- "29"
    if (Order$orderId == "") 
        Order$orderId <- reqIds(twsconn)
    order <- c(.twsOutgoingMSG$PLACE_ORDER, VERSION, as.character(Order$orderId), 
        Contract$symbol, Contract$sectype, Contract$expiry, Contract$strike, 
        Contract$right, Contract$multiplier, Contract$exch, Contract$primary, 
        Contract$currency, Contract$local, Contract$secIdType, 
        Contract$secId, Order$action, Order$totalQuantity, Order$orderType, 
        Order$lmtPrice, Order$auxPrice, Order$tif, Order$ocaGroup, 
        Order$account, Order$openClose, Order$origin, Order$orderRef, 
        Order$transmit, Order$parentId, Order$blockOrder, Order$sweepToFill, 
        Order$displaySize, Order$triggerMethod, Order$outsideRTH, 
        Order$hidden)
    if (Contract$sectype == "BAG") {
        if (is.null(Contract$comboleg)) {
            order <- c(order, 0)
        }
        else {
            comboLeg <- Contract$comboleg
            order <- c(order, length(comboLeg))
            for (i in 1:length(comboLeg)) {
                Leg <- comboLeg[[i]]
                order <- c(order, Leg$conId, Leg$ratio, Leg$action, 
                  Leg$exch, Leg$openClose, Leg$shortSaleSlot, 
                  Leg$designatedLocation)
            }
        }
    }
    order <- c(order, "", Order$discretionaryAmt, Order$goodAfterTime, 
        Order$goodTillDate, Order$faGroup, Order$faMethod, Order$faPercentage, 
        Order$faProfile, Order$shortSaleSlot, Order$designatedLocation, 
        Order$ocaType, Order$rule80A, Order$settlingFirm, Order$allOrNone, 
        Order$minQty, Order$percentOffset, Order$eTradeOnly, 
        Order$firmQuoteOnly, Order$nbboPriceCap, Order$auctionStrategy, 
        Order$startingPrice, Order$stockRefPrice, Order$delta, 
        Order$stockRangeLower, Order$stockRangeUpper, Order$overridePercentageConstraints, 
        Order$volatility, Order$volatilityType, Order$deltaNeutralOrderType, 
        Order$deltaNeutralAuxPrice, Order$continuousUpdate, Order$referencePriceType, 
        Order$trailStopPrice, Order$scaleInitLevelSize, Order$scaleSubsLevelSize, 
        Order$scalePriceIncrement, Order$clearingAccount, Order$clearingIntent, 
        Order$notHeld, "0", "", Order$whatIf)
    writeBin(order, con)
    assign(".Last.orderId", as.integer(Order$orderId), .IBrokersEnv)
    invisible(as.integer(Order$orderId))
}
