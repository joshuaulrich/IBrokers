reqMktData <-
function (conn, Contract, tickGenerics = "100,101,104,106,165,221,225,236", 
    snapshot = FALSE, tickerId = "1", timeStamp = "%Y%m%d %H:%M:%OS", 
    playback = 1, file = "", verbose = TRUE, eventWrapper = eWrapper(), 
    CALLBACK = twsCALLBACK, ...) 
{
    if (!is.twsConnection(conn)) 
        stop("tws connection object required")
    if (!is.twsPlayback(conn)) {
        Contract <- as.twsContract(Contract)
        if (is.twsContract(Contract)) 
            Contract <- list(Contract)
        for (n in 1:length(Contract)) {
            if (!is.twsContract(Contract[[n]])) 
                stop("twsContract required")
        }
    }
    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")
    cancelMktData <- function(con, tickerId) {
        if (inherits(con, "sockconn")) {
            for (i in 1:length(tickerId)) {
                writeBin(.twsOutgoingMSG$CANCEL_MKT_DATA, con)
                writeBin("2", con)
                writeBin(tickerId[i], con)
            }
        }
        else {
            seek(con, 0)
        }
    }
    if (is.null(CALLBACK)) 
        CALLBACK <- twsDEBUG
    snapshot <- ifelse(snapshot, "1", "0")
    if (snapshot == "1" && missing(tickGenerics)) 
        tickGenerics <- ""
    VERSION <- "11"
    fullSnapshot <- data.frame()
    symbols. <- NULL
    ticker_id <- as.character(tickerId)
    symbol.or.local <- function(x) {
        symbol <- x$symbol
        local <- x$local
        if (local == "") {
            return(symbol)
        }
        else return(local)
    }
    if (inherits(con, "sockconn")) {
        for (n in 1:length(Contract)) {
            if (Contract[[n]]$sectype == "BAG") 
                stop("BAG contract type in reqMktData not implemented")
            signals <- c(.twsOutgoingMSG$REQ_MKT_DATA, VERSION, 
                ticker_id, Contract[[n]]$conId, Contract[[n]]$symbol, 
                Contract[[n]]$sectype, Contract[[n]]$expiry, 
                Contract[[n]]$strike, Contract[[n]]$right, Contract[[n]]$multiplier, 
                Contract[[n]]$exch, Contract[[n]]$primary, Contract[[n]]$currency, 
                Contract[[n]]$local, {
                  if (is.null(Contract[[n]]$tradingClass)) "" else Contract[[n]]$tradingClass
                }, "0", tickGenerics, snapshot, "")
            writeBin(signals, con)
            if (snapshot == "1") {
                stop("Snapshot not working ? so not supported")
                eventWrapper <- eWrapper.snapshot()
                while (1) {
                  socketSelect(list(con), FALSE, NULL)
                  curMsg <- readBin(con, character(), 1)
                  processMsg(curMsg, con, eventWrapper, NULL, 
                    file, ...)
                  if (curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
                    fullSnapshot <- rbind(fullSnapshot, data.frame(lastTimeStamp = eventWrapper$get.Data("lastTimeStamp"), 
                      symbol = symbol.or.local(Contract[[n]]), 
                      bidSize = eventWrapper$get.Data("bidSize"), 
                      bidPrice = eventWrapper$get.Data("bidPrice"), 
                      askPrice = eventWrapper$get.Data("askPrice"), 
                      askSize = eventWrapper$get.Data("askSize"), 
                      lastPrice = eventWrapper$get.Data("lastPrice"), 
                      Volume = eventWrapper$get.Data("Volume"), 
                      Open = eventWrapper$get.Data("Open"), High = eventWrapper$get.Data("High"), 
                      Low = eventWrapper$get.Data("Low"), Close = eventWrapper$get.Data("Close")))
                    break
                  }
                }
                if (n == length(Contract)) 
                  return(fullSnapshot)
            }
            ticker_id <- as.character(as.numeric(tickerId) + 
                n)
            symbols. <- c(symbols., symbol.or.local(Contract[[n]]))
        }
    }
    eventWrapper$assign.Data("symbols", symbols.)
    if (!missing(CALLBACK) && is.na(list(CALLBACK))) {
        if (is.twsPlayback(conn)) {
            seek(conn[[1]], 0)
            stop("CALLBACK=NA is not available for playback")
        }
        return(as.character(as.numeric(tickerId):length(Contract)))
    }
    if (snapshot == "0") 
        on.exit(cancelMktData(con, as.character(as.numeric(tickerId):length(Contract))))
    if (!is.list(file)) 
        file <- list(file)
    if (length(file) != length(Contract)) 
        file <- rep(file, length(Contract))
    CALLBACK(conn, eWrapper = eventWrapper, timestamp = timeStamp, 
        file = file, playback = playback, timeout = NULL, ...)
}
