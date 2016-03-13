reqRealTimeBars <-
function (conn, Contract, whatToShow = "TRADES", barSize = "5", 
    useRTH = TRUE, playback = 1, tickerId = "1", file = "", verbose = TRUE, 
    eventWrapper = eWrapper(), CALLBACK = twsCALLBACK, ...) 
{
    if (!is.twsPlayback(conn)) {
        tickerId <- .reqRealTimeBars(conn, Contract, whatToShow, 
            barSize, useRTH, tickerId)
    }
    if (is.twsContract(Contract)) 
        Contract <- list(Contract)
    con <- conn[[1]]
    cancelRealTimeBars <- function(con, tickerId) {
        if (inherits(con, "sockconn")) {
            for (i in 1:length(tickerId)) {
                writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS, 
                  con)
                writeBin("1", con)
                writeBin(tickerId[i], con)
            }
        }
        else {
            seek(con, 0)
        }
    }
    if (is.null(CALLBACK)) 
        CALLBACK <- twsDEBUG
    if (!missing(CALLBACK) && is.na(list(CALLBACK))) {
        if (is.twsPlayback(conn)) {
            seek(conn[[1]], 0)
            stop("CALLBACK=NA is not available for playback")
        }
        return(tickerId)
    }
    on.exit(cancelRealTimeBars(con, tickerId))
    symbol.or.local <- function(x) {
        symbol <- x$symbol
        local <- x$local
        if (local == "") {
            return(symbol)
        }
        else return(local)
    }
    eventWrapper$assign.Data("symbols", sapply(Contract, symbol.or.local))
    timeStamp <- NULL
    if (!is.list(file)) 
        file <- list(file)
    if (length(file) != length(Contract)) 
        file <- rep(file, length(Contract))
    CALLBACK(conn, eWrapper = eventWrapper, timestamp = timeStamp, 
        file = file, playback = playback, ...)
}
