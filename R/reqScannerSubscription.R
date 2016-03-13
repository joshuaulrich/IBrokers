reqScannerSubscription <-
function (twsconn, subscription, tickerId = 1) 
{
    scanner <- data.frame(reqId = NULL, rank = NULL, symbol = NULL, 
        expiry = NULL, strike = NULL, right = NULL, exch = NULL, 
        currency = NULL, local = NULL, marketName = NULL, tradingClass = NULL, 
        distance = NULL, benchmark = NULL, projection = NULL, 
        legsStr = NULL)
    .reqScannerSubscription(twsconn, subscription, tickerId)
    on.exit(cancelScannerSubscription(twsconn, tickerId))
    con <- twsconn[[1]]
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        if (curMsg == .twsIncomingMSG$SCANNER_DATA) {
            cD <- twsContractDetails()
            version <- readBin(con, character(), 1L)
            reqId <- readBin(con, character(), 1L)
            numberOfElements <- as.integer(readBin(con, character(), 
                1L))
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
                ctr <- cD$contract
                scanner <- rbind(scanner, data.frame(reqId, rank, 
                  symbol = ctr$symbol, expiry = ctr$expiry, strike = ctr$strike, 
                  right = ctr$right, exch = ctr$exch, currency = ctr$currency, 
                  local = ctr$local, marketName = cD$marketName, 
                  tradingClass = cD$tradingClass, distance, benchmark, 
                  projection, legsStr, stringsAsFactors = FALSE))
            }
            return(scanner)
        }
        else processMsg(curMsg, con, eWrapper(NULL))
    }
}
