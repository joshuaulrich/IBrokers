`reqMktData` <-
function (conn, Contract, endDateTime, barSize = "1 day", duration = "1 M", 
    useRTH = "1", whatToShow = "TRADES", time.format = "1", verbose = FALSE, 
    tickerId = "1", timeout = 10, callback, file) 
{
    start.time <- Sys.time()
    if (class(conn) != "twsConnection") 
        stop("tws connection object required")
    if (class(Contract) != "twsContract") 
        stop("twsContract required")
    if (!barSize %in% c("1 secs", "5 secs", "15 secs", "30 secs", 
        "1 min", "2 mins", "3 mins", "5 mins", "15 mins", "30 mins", 
        "1 hour", "1 day", "1 week", "1 month", "3 months", "1 year")) 
        stop("unknown barSize")
    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")

    cancelMktData <- function(con,tickerId) {
      writeBin('2',con)
      writeBin('1',con)
      writeBin(tickerId,con)
    }

    on.exit(cancelMktData(con, as.character(tickerId)))

    if (missing(endDateTime)) 
        endDateTime <- strftime(as.POSIXlt(as.POSIXct("1970-01-01") + 
            as.numeric(reqCurrentTime(con))), format = "%Y%m%d %H:%M:%S", 
            use = FALSE)
    signals <- c(.twsOutgoingMSG$REQ_MKT_DATA, "6", as.character(tickerId), 
        Contract$symbol, Contract$sectype, Contract$expiry, Contract$strike, 
        Contract$right, Contract$multiplier, Contract$exch, Contract$primary, 
        Contract$currency, Contract$local,'100')


    for (i in 1:length(signals)) {
        writeBin(signals[i], con)
    }
    waiting <- TRUE
    response <- character(0)
    if (verbose) {
        cat("waiting for TWS reply ...")
        iter <- 1
        flush.console()
    }
    if (.Platform$OS == "windows") 
        Sys.sleep(0.1)
    while (waiting) {
        curMsg <- suppressWarnings(readBin(con, character(), 
            1))
        if (verbose) {
            cat(".")
            if (iter%%30 == 0) 
                cat("\n")
            flush.console()
            iter <- iter + 1
        }
        if (length(curMsg) > 0) {
            if (curMsg == .twsIncomingMSG$ERR_MSG) {
                if (!errorHandler(con, verbose, OK = c(165, 2106))) {
                  cat("\n")
                  stop("Unable to complete historical data request")
                }
            }
            if (curMsg == .twsIncomingMSG$TICK_PRICE) {
                header <- readBin(con, character(), 6)
                cat('<price> ')
                cat(paste(header),'\n')
                #waiting <- FALSE
            }
            if (curMsg == .twsIncomingMSG$TICK_SIZE) {
                header <- readBin(con, character(), 4)
                cat('<size> ')
                cat(paste(header),'\n')
                #waiting <- FALSE
            }
            if (curMsg == .twsIncomingMSG$TICK_OPTION) {
                header <- readBin(con, character(), 5)
                cat('<option> ')
                cat(paste(header),'\n')
                #waiting <- FALSE
            }
            if (curMsg == .twsIncomingMSG$TICK_GENERIC) {
                header <- readBin(con, character(), 4)
                cat('<generic> ')
                cat(paste(header),'\n')
                #waiting <- FALSE
            }
            if (curMsg == .twsIncomingMSG$TICK_STRING) {
                header <- readBin(con, character(), 4)
                cat('<string> ')
                cat(paste(header),'\n')
                #waiting <- FALSE
            }
            if (curMsg == .twsIncomingMSG$TICK_EFP) {
                header <- readBin(con, character(), 13)
                cat('<efp> ')
                cat(paste(header),'\n')
                #waiting <- FALSE
            }
        }
        if (Sys.time() - start.time > timeout) {
        }
        Sys.sleep(0.001)
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
