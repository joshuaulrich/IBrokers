reqContractDetails <-
function (conn, Contract, reqId = "1", verbose = FALSE, eventWrapper = eWrapper(), 
    CALLBACK = twsCALLBACK, ...) 
{
    .reqContractDetails(conn, Contract, reqId)
    if (is.null(CALLBACK)) 
        invisible(return(NULL))
    eW <- eWrapper(NULL)
    eW$contractDetails <- function(curMsg, msg, timestamp, file, 
        ...) {
        twsContractDetails(version = msg[1], contract = twsContract(conId = msg[12 + 
            1], symbol = msg[3], sectype = msg[4], expiry = msg[5], 
            primary = msg[21], strike = msg[5 + 1], right = msg[6 + 
                1], exch = msg[7 + 1], currency = msg[8 + 1], 
            multiplier = msg[14 + 1], include_expired = Contract$include_expired, 
            combo_legs_desc = "", comboleg = "", local = msg[9 + 
                1]), marketName = msg[10 + 1], tradingClass = msg[11 + 
            1], conId = msg[12 + 1], minTick = msg[13 + 1], orderTypes = unlist(strsplit(msg[15 + 
            1], ",")), validExchanges = unlist(strsplit(msg[16 + 
            1], ",")), priceMagnifier = msg[17 + 1], underConId = msg[18 + 
            1], longName = msg[19 + 1], contractMonth = msg[22], 
            industry = msg[23], category = msg[24], subcategory = msg[25], 
            timeZoneId = msg[26], tradingHours = msg[27], liquidHours = msg[28])
    }
    contracts <- list()
    con <- conn[[1]]
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1)
        if (curMsg != .twsIncomingMSG$CONTRACT_DATA) {
            if (curMsg == .twsIncomingMSG$ERR_MSG) {
                if (!errorHandler(con, verbose, OK = c(165, 300, 
                  366, 2104, 2106, 2107))) {
                  warning("error in contract details")
                  break
                }
            }
            else {
                processMsg(curMsg, con, eW, timestamp, file)
                if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) 
                  break
            }
        }
        if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
            contracts[[length(contracts) + 1]] <- processMsg(curMsg, 
                con, eW, timestamp, file)
        }
    }
    return(contracts)
}
