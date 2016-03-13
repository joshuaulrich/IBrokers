.calculateImpliedVolatility <-
function (twsconn, Contract, optionPrice, underPrice, reqId = 1) 
{
    if (!is.twsConnection(twsconn)) 
        stop("invalid tws connection")
    if (!is.twsContract(Contract)) 
        stop("invalid twsContract")
    VERSION <- "1"
    msg <- c(.twsOutgoingMSG$REQ_CALC_IMPLIED_VOLAT, VERSION, 
        as.character(reqId), Contract$conId, Contract$symbol, 
        Contract$sectype, Contract$expiry, Contract$strike, Contract$right, 
        Contract$multiplier, Contract$exch, Contract$primary, 
        Contract$currency, Contract$local, as.character(optionPrice), 
        as.character(underPrice))
    writeBin(msg, twsconn[[1]])
}
.calculateOptionPrice <-
function (twsconn, Contract, volatility, underPrice, reqId = 1) 
{
    if (!is.twsConnection(twsconn)) 
        stop("invalid tws connection")
    if (!is.twsContract(Contract)) 
        stop("invalid twsContract")
    VERSION <- "1"
    msg <- c(.twsOutgoingMSG$REQ_CALC_OPTION_PRICE, VERSION, 
        as.character(reqId), Contract$conId, Contract$symbol, 
        Contract$sectype, Contract$expiry, Contract$strike, Contract$right, 
        Contract$multiplier, Contract$exch, Contract$primary, 
        Contract$currency, Contract$local, as.character(volatility), 
        as.character(underPrice))
    writeBin(msg, twsconn[[1]])
}
.Depends <-
c("xts", "zoo")
.placeOrder <-
function (twsconn, Contract, Order) 
{
    if (!is.twsConnection(twsconn)) 
        stop("requires twsConnection object")
    if (!is.twsContract(Contract)) 
        stop("requires twsContract object for Contract arg")
    if (!inherits(Order, "twsOrder")) 
        stop("requires twsOrder object for Order arg")
    con <- twsconn[[1]]
    VERSION <- "42"
    if (is.null(Order$hedgeType) | is.null(Order$hedgeParam)) 
        stop(" NEW twsOrder has to be used")
    if (Order$orderId == "") 
        Order$orderId <- reqIds(twsconn)
    order <- c(.twsOutgoingMSG$PLACE_ORDER, VERSION, as.character(Order$orderId), 
        as.character(Contract$conId), Contract$symbol, Contract$sectype, 
        Contract$expiry, Contract$strike, Contract$right, Contract$multiplier, 
        Contract$exch, Contract$primary, Contract$currency, Contract$local, 
        {
            if (is.null(Contract$tradingClass)) "" else Contract$tradingClass
        }, Contract$secIdType, Contract$secId, Order$action, 
        Order$totalQuantity, Order$orderType, Order$lmtPrice, 
        Order$auxPrice, Order$tif, Order$ocaGroup, Order$account, 
        Order$openClose, Order$origin, Order$orderRef, Order$transmit, 
        Order$parentId, Order$blockOrder, Order$sweepToFill, 
        Order$displaySize, Order$triggerMethod, Order$outsideRTH, 
        Order$hidden)
    order <- c(order, "", Order$discretionaryAmt, Order$goodAfterTime, 
        Order$goodTillDate, Order$faGroup, Order$faMethod, Order$faPercentage, 
        Order$faProfile, Order$shortSaleSlot, Order$designatedLocation, 
        Order$exemptCode, Order$ocaType, Order$rule80A, Order$settlingFirm, 
        Order$allOrNone, Order$minQty, Order$percentOffset, Order$eTradeOnly, 
        Order$firmQuoteOnly, Order$nbboPriceCap, Order$auctionStrategy, 
        Order$startingPrice, Order$stockRefPrice, Order$delta, 
        Order$stockRangeLower, Order$stockRangeUpper, Order$overridePercentageConstraints, 
        Order$volatility, Order$volatilityType, Order$deltaNeutralOrderType, 
        Order$deltaNeutralAuxPrice, Order$continuousUpdate, Order$referencePriceType, 
        Order$trailStopPrice, Order$trailingPercent, Order$scaleInitLevelSize, 
        Order$scaleSubsLevelSize, Order$scalePriceIncrement, 
        Order$scaleTable, Order$activeStartTime, Order$activeStopTime)
    if (Order$hedgeType != "") 
        order <- c(order, Order$hedgeType, Order$hedgeParam)
    else order <- c(order, Order$hedgeType)
    order <- c(order, Order$optOutSmartRouting, Order$clearingAccount, 
        Order$clearingIntent, Order$notHeld, "0", Order$algoStrategy, 
        Order$whatIf, "")
    cat("placeOrder VERSION", VERSION, "\n")
    cat(order, "\n", sep = "*")
    writeBin(order, con)
    assign(".Last.orderId", as.integer(Order$orderId), .GlobalEnv)
    invisible(as.integer(Order$orderId))
}
.reqAccountUpdates <-
function (conn, subscribe = TRUE, acctCode = "1") 
{
    if (!is.twsConnection(conn)) 
        stop("requires twsConnection object")
    con <- conn[[1]]
    VERSION <- "2"
    writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, con)
    writeBin(VERSION, con)
    writeBin(as.character(as.numeric(subscribe)), con)
    writeBin(as.character(acctCode), con)
}
.reqAllOpenOrders <-
function (twsconn) 
{
    if (!is.twsConnection(twsconn)) 
        stop("requires twsConnection object")
    con <- twsconn[[1]]
    VERSION <- "1"
    writeBin(c(.twsOutgoingMSG$REQ_ALL_OPEN_ORDERS, VERSION), 
        con)
}
.reqAutoOpenOrders <-
function (twsconn, bAutoBind = TRUE) 
{
    if (!is.twsConnection(twsconn)) 
        stop("requires twsConnection object")
    bAutoBind <- as.character(as.integer(bAutoBind))
    con <- twsconn[[1]]
    VERSION <- "1"
    writeBin(c(.twsOutgoingMSG$REQ_AUTO_OPEN_ORDERS, VERSION, 
        bAutoBind), con)
}
.reqOpenOrders <-
function (twsconn) 
{
    if (!is.twsConnection(twsconn)) 
        stop("requires twsConnection object")
    con <- twsconn[[1]]
    VERSION <- "1"
    writeBin(c(.twsOutgoingMSG$REQ_OPEN_ORDERS, VERSION), con)
}
.twsERR <-
structure(c("Max rate of messages per second has been exceeded.", 
"Max number of tickers has been reached.", "Duplicate ticker ID.", 
"Duplicate order ID.", "Can't modify a filled order.", "Order being modified does not match original order.", 
"Can't transmit order ID.", "Cannot transmit incomplete order.", 
"Price is out of the range defined by the Percent Setting at order defaults frame. The order will not be transmitted.", 
"The price does not conform to the minimum price variation for this contract.", 
"The Time in force (tif type) and the order type are incompatible.", 
"The Tif option should be set to DAY for MOC and LOC orders.", 
"Relative orders are valid for stocks only.", "Relative orders for US stocks can only be submitted to SMART, SMART_ECN, INSTINET, or PRIMEX.", 
"The order cannot be transmitted to a dead exchange.", "The block order size must be at least 50.", 
"VWAP orders must be routed through the VWAP exchange.", "Only VWAP orders may be placed on the VWAP exchange.", 
"It is too late to place a VWAP order for today.", "Invalid BD flag for the order. Check \"Destination\" and \"BD\" flag.", 
"No request tag has been found for order:", "No record is available for conid:", 
"No market rule is available for conid:", "Buy price must be the same as the best asking price.", 
"Sell price must be the same as the best bidding price.", "Linkage orders are not supported on this exchange.", 
"Linkage customers cannot submit non-linkage orders for options.", 
"VWAP orders must be submitted at least three minutes before the start time.", 
"The sweep-to-fill flag and display size are only valid for US stocks routed through SMART, and will be ignored.", 
"This order cannot be transmitted without a clearing account.", 
"Submit new order failed.", "Modify order failed.", "Can't find order with ID =", 
"This order cannot be cancelled.", "VWAP orders can only be cancelled up to three minutes before the start time.", 
"Could not parse ticker request.", "Parsing error:", "The size value should be an integer:", 
"The price value should be a double:", "Institutional customer account does not have account info", 
"Requested ID is not an integer number.", "Order size does not match total share allocation.", 
"Error in validating entry fields.", "Invalid trigger method.", 
"The conditional contract info is incomplete.", "A conditional order can only be submitted when the order type is set to limit or market.", 
"This order cannot be transmitted without a user name.", "The \"hidden\" order attribute may not be specified for this order.", 
"EFPs can only be limit orders.", "Orders cannot be transmitted for a halted security.", 
"A sizeOp order must have a username and account.", "A SizeOp order must go to IBSX", 
"An order can be EITHER Iceberg or Discretionary. Please remove either the Discretionary amount or the Display size.", 
"You must specify an offset amount or a percent offset value.", 
"The percent offset value must be between 0% and 100%.", "The size value cannot be zero.", 
"Cancel attempted when order is not in a cancellable state. Order permId =", 
"Historical market data Service error message.", "Historical market Data Service query message.", 
"HMDS Expired Contract Violation.", "VWAP order time must be in the future.", 
"Discretionary amount does not conform to the minimum price variation for this contract.", 
"No security definition has been found for the request.", "Order rejected - Reason:", 
"Order cancelled - Reason:", "The security (   ) is not available or allowed for this account.", 
"Can't find EId with ticker Id:", "Invalid ticker action:", "Error parsing stop ticker string", 
"Invalid action:", "Invalid acct. value action:", "Request parsing error, the request has been ignored.", 
"Error processing DDE request.", "Invalid request topic.", "Unable to create the 'API' page in TWS as the maximum number of pages already exists.", 
"Max number (3) of market depth requests has been reached.", 
"Can't find the subscribed market depth with tickerId:", "The origin is invalid.", 
"The combo details are invalid.", "The combo details for leg '<leg number>' are invalid.", 
"Security type 'BAG' requires combo leg details.", "Stock combo legs are restricted to SMART order routing.", 
"Market depth data has been HALTED. Please re-subscribe.", "Market depth data has been RESET. Please empty deep book contents before applying any new entries.", 
"Advisors cannot modify partially filled orders.", "Attempt to set the server log level failed as the log level was invalid.", 
"Server error when reading an API client request.", "Server error when validating an API client request.", 
"Server error when processing an API client request.", "Server error: cause - %s", 
"Server error when reading a DDE client request - missing data.", 
"Discretionary orders are not supported for this combination of exchange and order type.", 
"Unable connect as the client id is already in use. Retry with a unique client id.", 
"Only API connections with clientId set to 0 can set the auto bind TWS orders property.", 
"Trailing stop orders can be attached to limit or stop-limit orders only.", 
"Order modify failed. Cannot change to the new order type.", 
"Only FA or STL customers can request managed accounts list.", 
"Internal error. FA or STL does not have any managed accounts.", 
"The account codes for the order profile are invalid.", "Invalid share allocation syntax.", 
"Invalid Good Till Date order", "Invalid delta: The delta must be between 0 and 100.", 
"Invalid Expiration Time.", "Invalid Good After Time.", "Good After Time Disabled.", 
"Futures spread no longer supported.", "Invalid improvement amount.", 
"Invalid delta.", "Invalid Peg.", "Invalid Good Till Date.", 
"The account is not a financial advisor account", "Invalid FA inter-market combo order.", 
"Not an institutional account.", "Invalid short slot value.", 
"Not a short sale.", "Invalid GAT inter-market combo.", "Invalid minimum quantity for inter-market combo.", 
"Invalid RTH only designation.", "Bad short slot location.", 
"Short slot location specified.", "Not subscribed to market data.", 
"Order size incorrect.", "Smart-combo OCA not supported.", "Your client version is out of date.", 
"Smart combo child not supported.", "Native combo OCA is incompatible.", 
"What- if check not supported for this Smart inter-market combo.", 
"Invalid trigger price.", "Invalid adjusted stop price.", "Invalid adjusted stop limit price.", 
"Invalid adjusted trailing amount.", "No scanner subscription found for ticker id:", 
"No historical data query found for ticker id:", "Volatility type if set must be 1 or 2 for VOL orders.", 
"Reference Price Type must be 1 or 2 for dynamic volatility management.", 
"Volatility orders are only valid for US options.", "Dynamic Volatility orders must be SMART routed, or trade on a Price Improvement Exchange.", 
"VOL order requires positive floating point value for volatility.", 
"Cannot set dynamic VOL attribute on non-VOL order.", "Can only set stock range attribute on VOL or PEGGED TO STOCK order.", 
"If both are set, the lower stock range attribute must be less than the upper stock range attribute.", 
"Stock range attributes cannot be negative.", "The order is not eligible for continuous update. The option must trade on a cheap-to-reroute exchange.", 
"Must specify valid delta hedge order aux. price.", "Delta hedge order type requires delta hedge aux. price to be specified.", 
"Delta hedge order type requires that no delta hedge aux. price be specified.", 
"This order type is not allowed for delta hedge orders.", "Your DDE.dll needs to be upgraded.", 
"The price specified violates the number of ticks constraint specified in the default order settings.", 
"The size specified violates the size constraint specified in the default order settings.", 
"Invalid DDE array request.", "Duplicate ticker ID for API scanner subscription.", 
"Duplicate ticker ID for API historical data query.", "Unsupported order type for this exchange and security type.", 
"Order size is smaller than the minimum requirement.", "Supplied routed order ID is not unique.", 
"Supplied routed order ID is invalid.", "Invalid date for GTD.", 
"This order is invalid since is uses an expired contract.", "The short slot order has not delta hedge order type.", 
"Invalid process time.", "Direct routed OCA error.", "Direct routed order type error.", 
"Region order type error.", "Direct routed condition error placeholder.", 
"Order message error", "Algo order error.", "Length restriction.", 
"Conditions are not allowed for this contract.", "Invalid stop price.", 
"Allowed quantity message.", "The child order quantity should be equivalent to the parent order size.", 
"The currency (  ) is not allowed.", "The symbol should contain valid non-unicode characters only.", 
"Invalid scale order increment.", "Invalid scale order. You must specify order component size.", 
"Invalid subsequent component size for scale order.", "Invalid outside RTH.", 
"The contract is not available for trading.", "What-if order should have the transmit flag set to true.", 
"Do not use generic ticks for snapshot market data.", "RFQ already sent for the conid.", 
"RFQ not applicable for the contract. Order ID:", "Invalid initial component size for scale order.", 
"Invalid scale order profit offset.", "Missing initial component size for scale order.", 
"Invalid real-time query.", "Invalid route.", "Invalid OMS component attributes.", 
"Already connected.", "Couldn't connect to TWS. Confirm that API is enabled in TWS via the Configure>API menu command.", 
"Your version of TWS is out of date and must be upgraded.", "Not connected.", 
"Fatal error: Unknown message id.", "Request market data - sending error:", 
"Cancel market data - sending error:", "Order - sending error:", 
"Account update request - sending error:", "Request for executions  - sending error:", 
"Cancel order - sending error:", "Request open order - sending error:", 
"Unknown contract. Verify the contract details supplied.", "Request contract data - sending error:", 
"Request market depth - sending error:", "Cancel market depth - sending error:", 
"Set server log level - sending error:", "FA Information Request - sending error:", 
"FA Information Replace - sending error:", "Request Scanner subscription - sending error:", 
"Cancel Scanner subscription - sending error:", "Request Scanner parameter - sending error:", 
"Request Historical data - sending error:", "Cancel Historical data - sending error:", 
"Request real-time bar data - sending error:", "Cancel real-time bar data - sending error:", 
"Connectivity between IB and TWS has been lost.", "Connectivity between IB and TWS has been restored- data lost.", 
"Connectivity between IB and TWS has been restored- data maintained.", 
"TWS socket port has been reset and this connection is being dropped.", 
"New account data requested from TWS.  API client has been unsubscribed from account data.", 
"Unable to subscribe to account as the following clients are subscribed to a different account.", 
"Unable to modify this order as it is still being processed.", 
"A market data farm is disconnected.", "A market data farm is connected.", 
"A historical data farm is disconnected.", "A historical data farm is connected.", 
"A historical data farm connection has become inactive but should be available upon demand.", 
"A market data farm connection has become inactive but should be available upon demand.", 
"Order Event Warning: Attribute 'Outside Regular Trading Hours' is ignored based on the order type and destination. PlaceOrder is now processed."
), .Dim = c(229L, 1L), .Dimnames = list(c("100", "101", "102", 
"103", "104", "105", "106", "107", "109", "110", "111", "113", 
"114", "115", "116", "117", "118", "119", "120", "121", "122", 
"123", "124", "125", "126", "127", "128", "129", "131", "132", 
"133", "134", "135", "136", "137", "138", "139", "140", "141", 
"142", "143", "144", "145", "146", "147", "148", "151", "152", 
"153", "154", "155", "156", "157", "158", "159", "160", "161", 
"162", "165", "166", "167", "168", "200", "201", "202", "203", 
"300", "301", "302", "303", "304", "305", "306", "307", "308", 
"309", "310", "311", "312", "313", "314", "315", "316", "317", 
"318", "319", "320", "321", "322", "323", "324", "325", "326", 
"327", "328", "329", "330", "331", "332", "333", "334", "335", 
"336", "337", "338", "339", "340", "341", "342", "343", "344", 
"345", "346", "347", "348", "349", "350", "351", "352", "353", 
"354", "355", "356", "357", "358", "359", "360", "361", "362", 
"363", "364", "365", "366", "367", "368", "369", "370", "371", 
"372", "373", "374", "375", "376", "377", "378", "379", "380", 
"381", "382", "383", "384", "385", "386", "387", "388", "389", 
"390", "391", "392", "393", "394", "395", "396", "397", "398", 
"399", "400", "401", "402", "403", "404", "405", "406", "407", 
"408", "409", "410", "411", "412", "413", "414", "415", "416", 
"417", "418", "419", "420", "421", "422", "501", "502", "503", 
"504", "505", "510", "511", "512", "513", "514", "515", "516", 
"517", "518", "519", "520", "521", "522", "523", "524", "525", 
"526", "527", "528", "529", "530", "531", "1101", "1102", "1300", 
"2100", "2101", "2102", "2103", "2104", "2105", "2106", "2107", 
"2108", "2109"), NULL))
.twsIncomingMSG <-
structure(list(TICK_PRICE = "1", TICK_SIZE = "2", ORDER_STATUS = "3", 
    ERR_MSG = "4", OPEN_ORDER = "5", ACCT_VALUE = "6", PORTFOLIO_VALUE = "7", 
    ACCT_UPDATE_TIME = "8", NEXT_VALID_ID = "9", CONTRACT_DATA = "10", 
    EXECUTION_DATA = "11", MARKET_DEPTH = "12", MARKET_DEPTH_L2 = "13", 
    NEWS_BULLETINS = "14", MANAGED_ACCTS = "15", RECEIVE_FA = "16", 
    HISTORICAL_DATA = "17", BOND_CONTRACT_DATA = "18", SCANNER_PARAMETERS = "19", 
    SCANNER_DATA = "20", TICK_OPTION_COMPUTATION = "21", TICK_GENERIC = "45", 
    TICK_STRING = "46", TICK_EFP = "47", CURRENT_TIME = "49", 
    REAL_TIME_BARS = "50", FUNDAMENTAL_DATA = "51", CONTRACT_DATA_END = "52", 
    OPEN_ORDER_END = "53", ACCT_DOWNLOAD_END = "54", EXECUTION_DATA_END = "55", 
    DELTA_NEUTRAL_VALIDATION = "56", TICK_SNAPSHOT_END = "57", 
    MARKET_DATA_TYPE = "58", COMMISSION_REPORT = "59", POSITION_DATA = "61", 
    POSITION_END = "62", ACCOUNT_SUMMARY = "63", ACCOUNT_SUMMARY_END = "64", 
    VERIFY_MESSAGE_API = "65", VERIFY_COMPLETED = "66", DISPLAY_GROUP_LIST = "67", 
    DISPLAY_GROUP_UPDATED = "68"), .Names = c("TICK_PRICE", "TICK_SIZE", 
"ORDER_STATUS", "ERR_MSG", "OPEN_ORDER", "ACCT_VALUE", "PORTFOLIO_VALUE", 
"ACCT_UPDATE_TIME", "NEXT_VALID_ID", "CONTRACT_DATA", "EXECUTION_DATA", 
"MARKET_DEPTH", "MARKET_DEPTH_L2", "NEWS_BULLETINS", "MANAGED_ACCTS", 
"RECEIVE_FA", "HISTORICAL_DATA", "BOND_CONTRACT_DATA", "SCANNER_PARAMETERS", 
"SCANNER_DATA", "TICK_OPTION_COMPUTATION", "TICK_GENERIC", "TICK_STRING", 
"TICK_EFP", "CURRENT_TIME", "REAL_TIME_BARS", "FUNDAMENTAL_DATA", 
"CONTRACT_DATA_END", "OPEN_ORDER_END", "ACCT_DOWNLOAD_END", "EXECUTION_DATA_END", 
"DELTA_NEUTRAL_VALIDATION", "TICK_SNAPSHOT_END", "MARKET_DATA_TYPE", 
"COMMISSION_REPORT", "POSITION_DATA", "POSITION_END", "ACCOUNT_SUMMARY", 
"ACCOUNT_SUMMARY_END", "VERIFY_MESSAGE_API", "VERIFY_COMPLETED", 
"DISPLAY_GROUP_LIST", "DISPLAY_GROUP_UPDATED"))
.twsOrderID <-
structure(list(CUSTOMER = 0, FIRM = 1, OPT_UNKNOWN = "?", OPT_BROKER_DEALER = "b", 
    OPT_CUSTOMER = "c", OPT_FIRM = "f", OPT_ISEMM = "m", OPT_FARMM = "n", 
    OPT_SPECIALIST = "y", AUCTION_MATCH = 1, AUCTION_IMPROVEMENT = 2, 
    AUCTION_TRANSPARENT = 3, EMPTY_STR = ""), .Names = c("CUSTOMER", 
"FIRM", "OPT_UNKNOWN", "OPT_BROKER_DEALER", "OPT_CUSTOMER", "OPT_FIRM", 
"OPT_ISEMM", "OPT_FARMM", "OPT_SPECIALIST", "AUCTION_MATCH", 
"AUCTION_IMPROVEMENT", "AUCTION_TRANSPARENT", "EMPTY_STR"))
.twsOutgoingMSG <-
structure(list(REQ_MKT_DATA = "1", CANCEL_MKT_DATA = "2", PLACE_ORDER = "3", 
    CANCEL_ORDER = "4", REQ_OPEN_ORDERS = "5", REQ_ACCOUNT_DATA = "6", 
    REQ_EXECUTIONS = "7", REQ_IDS = "8", REQ_CONTRACT_DATA = "9", 
    REQ_MKT_DEPTH = "10", CANCEL_MKT_DEPTH = "11", REQ_NEWS_BULLETINS = "12", 
    CANCEL_NEWS_BULLETINS = "13", SET_SERVER_LOGLEVEL = "14", 
    REQ_AUTO_OPEN_ORDERS = "15", REQ_ALL_OPEN_ORDERS = "16", 
    REQ_MANAGED_ACCTS = "17", REQ_FA = "18", REPLACE_FA = "19", 
    REQ_HISTORICAL_DATA = "20", EXERCISE_OPTIONS = "21", REQ_SCANNER_SUBSCRIPTION = "22", 
    CANCEL_SCANNER_SUBSCRIPTION = "23", REQ_SCANNER_PARAMETERS = "24", 
    CANCEL_HISTORICAL_DATA = "25", REQ_CURRENT_TIME = "49", REQ_REAL_TIME_BARS = "50", 
    CANCEL_REAL_TIME_BARS = "51", REQ_FUNDAMENTAL_DATA = "52", 
    CANCEL_FUNDAMENTAL_DATA = "53", REQ_CALC_IMPLIED_VOLAT = "54", 
    REQ_CALC_OPTION_PRICE = "55", CANCEL_CALC_IMPLIED_VOLAT = "56", 
    CANCEL_CALC_OPTION_PRICE = "57"), .Names = c("REQ_MKT_DATA", 
"CANCEL_MKT_DATA", "PLACE_ORDER", "CANCEL_ORDER", "REQ_OPEN_ORDERS", 
"REQ_ACCOUNT_DATA", "REQ_EXECUTIONS", "REQ_IDS", "REQ_CONTRACT_DATA", 
"REQ_MKT_DEPTH", "CANCEL_MKT_DEPTH", "REQ_NEWS_BULLETINS", "CANCEL_NEWS_BULLETINS", 
"SET_SERVER_LOGLEVEL", "REQ_AUTO_OPEN_ORDERS", "REQ_ALL_OPEN_ORDERS", 
"REQ_MANAGED_ACCTS", "REQ_FA", "REPLACE_FA", "REQ_HISTORICAL_DATA", 
"EXERCISE_OPTIONS", "REQ_SCANNER_SUBSCRIPTION", "CANCEL_SCANNER_SUBSCRIPTION", 
"REQ_SCANNER_PARAMETERS", "CANCEL_HISTORICAL_DATA", "REQ_CURRENT_TIME", 
"REQ_REAL_TIME_BARS", "CANCEL_REAL_TIME_BARS", "REQ_FUNDAMENTAL_DATA", 
"CANCEL_FUNDAMENTAL_DATA", "REQ_CALC_IMPLIED_VOLAT", "REQ_CALC_OPTION_PRICE", 
"CANCEL_CALC_IMPLIED_VOLAT", "CANCEL_CALC_OPTION_PRICE"))
.twsTickType <-
structure(list(BID_SIZE = 0, BID = 1, ASK = 2, ASK_SIZE = 3, 
    LAST = 4, LAST_SIZE = 5, HIGH = 6, LOW = 7, VOLUME = 8, CLOSE = 9, 
    BID_OPTION = 10, ASK_OPTION = 11, LAST_OPTION = 12, MODEL_OPTION = 13, 
    OPEN = 14, LOW_13_WEEK = 15, HIGH_13_WEEK = 16, LOW_26_WEEK = 17, 
    HIGH_26_WEEK = 18, LOW_52_WEEK = 19, HIGH_52_WEEK = 20, AVG_VOLUME = 21, 
    OPEN_INTEREST = 22, OPTION_HISTORICAL_VOL = 23, OPTION_IMPLIED_VOL = 24, 
    OPTION_BID_EXCH = 25, OPTION_ASK_EXCH = 26, OPTION_CALL_OPEN_INTEREST = 27, 
    OPTION_PUT_OPEN_INTEREST = 28, OPTION_CALL_VOLUME = 29, OPTION_PUT_VOLUME = 30, 
    INDEX_FUTURE_PREMIUM = 31, BID_EXCH = 32, ASK_EXCH = 33, 
    AUCTION_VOLUME = 34, AUCTION_PRICE = 35, AUCTION_IMBALANCE = 36, 
    MARK_PRICE = 37, BID_EFP_COMPUTATION = 38, ASK_EFP_COMPUTATION = 39, 
    LAST_EFP_COMPUTATION = 40, OPEN_EFP_COMPUTATION = 41, HIGH_EFP_COMPUTATION = 42, 
    LOW_EFP_COMPUTATION = 43, CLOSE_EFP_COMPUTATION = 44, LAST_TIMESTAMP = 45, 
    SHORTABLE = 46, FUNDAMENTAL_RATIOS = 47, RT_VOLUME = 48, 
    HALTED = 49), .Names = c("BID_SIZE", "BID", "ASK", "ASK_SIZE", 
"LAST", "LAST_SIZE", "HIGH", "LOW", "VOLUME", "CLOSE", "BID_OPTION", 
"ASK_OPTION", "LAST_OPTION", "MODEL_OPTION", "OPEN", "LOW_13_WEEK", 
"HIGH_13_WEEK", "LOW_26_WEEK", "HIGH_26_WEEK", "LOW_52_WEEK", 
"HIGH_52_WEEK", "AVG_VOLUME", "OPEN_INTEREST", "OPTION_HISTORICAL_VOL", 
"OPTION_IMPLIED_VOL", "OPTION_BID_EXCH", "OPTION_ASK_EXCH", "OPTION_CALL_OPEN_INTEREST", 
"OPTION_PUT_OPEN_INTEREST", "OPTION_CALL_VOLUME", "OPTION_PUT_VOLUME", 
"INDEX_FUTURE_PREMIUM", "BID_EXCH", "ASK_EXCH", "AUCTION_VOLUME", 
"AUCTION_PRICE", "AUCTION_IMBALANCE", "MARK_PRICE", "BID_EFP_COMPUTATION", 
"ASK_EFP_COMPUTATION", "LAST_EFP_COMPUTATION", "OPEN_EFP_COMPUTATION", 
"HIGH_EFP_COMPUTATION", "LOW_EFP_COMPUTATION", "CLOSE_EFP_COMPUTATION", 
"LAST_TIMESTAMP", "SHORTABLE", "FUNDAMENTAL_RATIOS", "RT_VOLUME", 
"HALTED"))
