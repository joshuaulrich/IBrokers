`reqRealTimeBars` <-
function (conn, Contract, tickerId = "1", 
          whatToShow="TRADES",
          barSize="5",useRTH="1", callback, file, verbose=TRUE)
{
    if (class(conn) != "twsConnection") 
        stop("tws connection object required")
    if (class(Contract) != "twsContract") 
        stop("twsContract required")

    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")

    cancelRealTimeBars <- function(con,tickerId) {
      writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS,con)
      writeBin('1',con)
      writeBin(tickerId,con)
    }

    on.exit(cancelRealTimeBars(con, as.character(tickerId)))

    VERSION <- "1"
 
    signals <- c(.twsOutgoingMSG$REQ_REAL_TIME_BARS, VERSION, as.character(tickerId), 
        Contract$symbol, Contract$sectype, Contract$expiry, Contract$strike, 
        Contract$right, Contract$multiplier, Contract$exch, Contract$primary, 
        Contract$currency, Contract$local,barSize,whatToShow,useRTH)

    for (i in 1:length(signals)) {
        writeBin(signals[i], con)
    }
    waiting <- TRUE
    response <- character(0)

    if (.Platform$OS == "windows") 
        Sys.sleep(0.1)
    while (waiting) {
        curMsg <- suppressWarnings(readBin(con, character(), 
            1))
        if (length(curMsg) > 0) {
            if (curMsg == .twsIncomingMSG$ERR_MSG) {
                if (!errorHandler(con, verbose, OK = c(165, 300, 2104,2106,2107))) {
                  cat("\n")
                  stop("Unable to complete market data request")
                }
            }
            if( curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
              msg <- readBin(con,character(),10)
              columns <- c('Id','time','open','high','low','close','volume','wap','count')
              cat(paste(columns,'=',msg[-1],sep=''),'\n')
            }
        }
    }
#    if (missing(callback)) {
#        cm <- matrix(response, nc = 9, byrow = TRUE)
#        cm[, 8] <- ifelse(cm[, 8] == "false", 0, 1)
#        dts <- gsub("(\\d{4})(\\d{2})(\\d{2})", "\\1-\\2-\\3", 
#            cm[, 1], perl = TRUE)
#        if (!missing(file)) {
#            cm[, 1] <- dts
#            write.table(cm, file = file, quote = FALSE, row.names = FALSE, 
#                col.names = FALSE, sep = ",")
#            invisible(return())
#        }
#        x <- xts(matrix(as.numeric(cm[, -1]), nc = 8), order.by = as.POSIXct(dts))
#        colnames(x) <- c("Open", "High", "Low", "Close", "Volume", 
#            "WAP", "hasGaps", "Count")
#        xtsAttributes(x) <- list(from = req.from, to = req.to, 
#            src = "IB", updated = Sys.time())
#        return(x)
#    }
#    else if (is.null(callback)) {
#        return(c(header, response))
#    }
#    else {
#        FUN <- match.fun(callback)
#        return(FUN(c(header, response)))
#    }
}

