`.twsOrderID` <-
structure(list(CUSTOMER=0, FIRM=1,
               OPT_UNKNOWN="?", OPT_BROKER_DEALER="b",
               OPT_CUSTOMER="c", OPT_FIRM="f", OPT_ISEMM="m",
               OPT_FARMM="n", OPT_SPECIALIST="y",
               AUCTION_MATCH=1, AUCTION_IMPROVEMENT=2, AUCTION_TRANSPARENT=3,
               EMPTY_STR=""),
               .Names=c('CUSTOMER', 'FIRM',
               'OPT_UNKNOWN', 'OPT_BROKER_DEALER',
               'OPT_CUSTOMER', 'OPT_FIRM', 'OPT_ISEMM',
               'OPT_FARMM', 'OPT_SPECIALIST',
               'AUCTION_MATCH', 'AUCTION_IMPROVEMENT', 'AUCTION_TRANSPARENT',
               'EMPTY_STR'))


# Enable the hedgeType and hedgeParam  fields and possibly optOutSmartRouting.
twsOrder <- function (
	orderId, action = "BUY", totalQuantity = "0", orderType = "LMT",
    lmtPrice = "0.0", auxPrice = "", tif = "", outsideRTH = "0",
    openClose = "O", origin = .twsOrderID$CUSTOMER, ocaGroup = "",
    account = "", orderRef = "", transmit = TRUE, parentId = "0",
    blockOrder = "0", sweepToFill = "0", displaySize = "0", triggerMethod = "0",
    hidden = "0", discretionaryAmt = "0", goodAfterTime = "",
    goodTillDate = "", faGroup = "", faMethod = "", faPercentage = "",
    faProfile = "", shortSaleSlot = "0", designatedLocation = .twsOrderID$EMPTY_STR,
    ocaType = "0", rule80A = "", settlingFirm = "", clearingAccount = "",
    clearingIntent = "", allOrNone = "0", minQty = "", percentOffset = "",
    eTradeOnly = "1", firmQuoteOnly = "1", nbboPriceCap = "",
    auctionStrategy = "0", startingPrice = "", stockRefPrice = "",
    delta = "", stockRangeLower = "", stockRangeUpper = "", overridePercentageConstraints = "0",
    volatility = "", volatilityType = "", deltaNeutralOrderType = "",
    deltaNeutralAuxPrice = "", 
    continuousUpdate = "0", referencePriceType = "",
    trailStopPrice = "", basisPoints = "", basisPointsType = "",
    scaleInitLevelSize = "", scaleSubsLevelSize = "", scalePriceIncrement = "",
    notHeld = FALSE, 
    algoStrategy = "", 
    algoParams = NULL,  #has count
    whatIf = FALSE,
    clientId = "", permId = "", exemptCode="-1",
	hedgeType = "", hedgeParam = "" ,
	optOutSmartRouting = FALSE, scaleTable="", activeStartTime="", activeStopTime="", trailingPercent="",	
	#NEW
	deltaNeutralConId = "0",
	deltaNeutralSettlingFirm = '',
	deltaNeutralClearingAccount = '',
	deltaNeutralClearingIntent = '',
	deltaNeutralOpenClose = '',
	deltaNeutralShortSale  = "0",
	deltaNeutralShortSaleSlot = "0",
	deltaNeutralDesignatedLocation = '',

	scalePriceAdjustValue = "0",
	scalePriceAdjustInterval = "0",
	scaleProfitOffset = "0",
	scaleAutoReset = "0",
	scaleInitPosition = "0",
	scaleInitFillQty = "0",
	scaleRandomPercent = "0",

	smartComboRoutingParams = NULL,	#has count
	smartComboRoutingParamsCount = '0',	#helper parameter only, used in OPEN_ORDER
	orderComboLegs = NULL,	#has count
	orderComboLegsCount = '0',	#helper parameter only, used in OPEN_ORDER
		
	comboLegs = NULL, #dummy param, used in OPEN_ORDER
	comboLegsCount = '0', #dummy param , used in OPEN_ORDER
	
	orderMiscOptions = NULL	
	)
{
    if (missing(orderId))
        orderId <- ""
    structure(list(orderId = orderId, clientId = clientId, permId = permId,
        action = action, totalQuantity = as.character(as.numeric(totalQuantity)),
        orderType = orderType, lmtPrice = as.character(lmtPrice),
        auxPrice = as.character(auxPrice), tif = tif, ocaGroup = ocaGroup,
        ocaType = ocaType, orderRef = orderRef, transmit = as.character(as.integer(transmit)),
        parentId = parentId, blockOrder = blockOrder, sweepToFill = sweepToFill,
        displaySize = displaySize, triggerMethod = triggerMethod,
        outsideRTH = outsideRTH, hidden = hidden, goodAfterTime = goodAfterTime,
        goodTillDate = goodTillDate, overridePercentageConstraints = overridePercentageConstraints,
        rule80A = rule80A, allOrNone = allOrNone, minQty = minQty,
        percentOffset = percentOffset, trailStopPrice = trailStopPrice,
        faGroup = faGroup, faProfile = faProfile, faMethod = faMethod,
        faPercentage = faPercentage, openClose = openClose, origin = origin,
        shortSaleSlot = shortSaleSlot, designatedLocation = designatedLocation,
        discretionaryAmt = discretionaryAmt, eTradeOnly = eTradeOnly,
        firmQuoteOnly = firmQuoteOnly, nbboPriceCap = nbboPriceCap,
        auctionStrategy = auctionStrategy, startingPrice = startingPrice,
        stockRefPrice = stockRefPrice, delta = delta, stockRangeLower = stockRangeLower,
        stockRangeUpper = stockRangeUpper, volatility = volatility,
        volatilityType = volatilityType, continuousUpdate = continuousUpdate,
        referencePriceType = referencePriceType, deltaNeutralOrderType = deltaNeutralOrderType,
        deltaNeutralAuxPrice = deltaNeutralAuxPrice, basisPoints = basisPoints,
        basisPointsType = basisPointsType, scaleInitLevelSize = scaleInitLevelSize,
        scaleSubsLevelSize = scaleSubsLevelSize, scalePriceIncrement = scalePriceIncrement,
        account = account, settlingFirm = settlingFirm, clearingAccount = clearingAccount,
        clearingIntent = clearingIntent, algoStrategy = algoStrategy,
        algoParams = algoParams, whatIf = as.character(as.integer(whatIf)),
        notHeld = as.character(as.integer(notHeld)),
        exemptCode = as.character(as.integer(exemptCode)),  #NEW
        hedgeType = hedgeType,  #NEW !! using it
        hedgeParam = hedgeParam,  #NEW !! using it
        optOutSmartRouting = as.character(as.integer(optOutSmartRouting)),  #NEW
        scaleTable = scaleTable, #NEW
        activeStartTime = activeStartTime, #NEW
        activeStopTime = activeStopTime , #NEW
        trailingPercent = trailingPercent, #NEW
        
		#NEW all below 
		deltaNeutralConId = deltaNeutralConId,
		deltaNeutralSettlingFirm = deltaNeutralSettlingFirm,
		deltaNeutralClearingAccount = deltaNeutralClearingAccount,
		deltaNeutralClearingIntent = deltaNeutralClearingIntent,
		deltaNeutralOpenClose = deltaNeutralOpenClose,
		deltaNeutralShortSale  = deltaNeutralShortSale,
		deltaNeutralShortSaleSlot = deltaNeutralShortSaleSlot,
		deltaNeutralDesignatedLocation = deltaNeutralDesignatedLocation,

		scalePriceAdjustValue = scalePriceAdjustValue,
		scalePriceAdjustInterval = scalePriceAdjustInterval,
		scaleProfitOffset = scaleProfitOffset,
		scaleAutoReset = scaleAutoReset,
		scaleInitPosition = scaleInitPosition,
		scaleInitFillQty = scaleInitFillQty,
		scaleRandomPercent = scaleRandomPercent,

		smartComboRoutingParams = smartComboRoutingParams,	#has count
		smartComboRoutingParamsCount = smartComboRoutingParamsCount,	
		orderComboLegs = orderComboLegs,	#has count
		orderComboLegsCount = orderComboLegsCount,	
		
		comboLegs = comboLegs, #has count. dummy only
		comboLegsCount = comboLegsCount, # dummy only
		
		orderMiscOptions = orderMiscOptions	        
        
        ), class = "twsOrder")
}



`print.twsOrder` <-
function(x, ...) {
  str(unclass(x))
}
