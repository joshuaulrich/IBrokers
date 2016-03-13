reqHistoricalData <-
function (conn, Contract, endDateTime, barSize = "1 day", duration = "1 M", 
    useRTH = "1", whatToShow = "TRADES", timeFormat = "1", tzone = "", 
    verbose = TRUE, tickerId = "1", eventHistoricalData, file) 
{
    if (!missing(endDateTime) && length(endDateTime) > 1) {
        if (!timeBased(endDateTime)) 
            stop("endDateTime length greater than 2 needs to be timeBased")
        sleep <- 0
        rHDargs <- list(conn = conn, Contract = Contract, barSize = barSize, 
            duration = duration, useRTH = useRTH, whatToShow = whatToShow, 
            timeFormat = timeFormat, tzone = tzone, verbose = verbose, 
            tickerId = tickerId)
        if (!missing(eventHistoricalData)) 
            rHDargs$eventHistoricalData <- eventHistoricalData
        if (!missing(file)) 
            rHDargs$file <- file
        x <- lapply(format(endDateTime, "%Y%m%d 23:59:59"), function(eDT) {
            rHDargs$endDateTime <- eDT
            xx <- try(do.call("reqHistoricalData", rHDargs), 
                silent = TRUE)
            Sys.sleep(10)
            if (inherits(xx, "try-error")) 
                return(NULL)
            return(xx)
        })
        x <- do.call("rbind.xts", x)
        return(x[-which(duplicated(.index(x)))])
    }
    con <- conn[[1]]
    cancelHistoricalData <- function(con, tickerId) {
        if (!isOpen(con)) 
            stop("invalid TWS connection")
        writeBin(.twsOutgoingMSG$CANCEL_HISTORICAL_DATA, con)
        writeBin("1", con)
        writeBin(as.character(tickerId), con)
    }
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")
    on.exit(cancelHistoricalData(con, as.character(tickerId)))
    if (!is.twsConnection(conn)) 
        stop("tws connection object required")
    if (!is.twsContract(Contract)) 
        stop("twsContract required")
    validBarSize <- c("1 secs", "5 secs", "15 secs", "30 secs", 
        "1 min", "2 mins", "3 mins", "5 mins", "15 mins", "30 mins", 
        "1 hour", "1 day", "1 week", "1 month", "3 months", "1 year")
    if (!barSize %in% validBarSize) 
        stop(paste("unknown barSize try: ", paste(validBarSize, 
            sep = ";")))
    if (missing(endDateTime) || is.null(endDateTime)) 
        endDateTime <- strftime(as.POSIXlt(as.POSIXct("1970-01-01") + 
            as.numeric(reqCurrentTime(conn))), format = "%Y%m%d %H:%M:%S", 
            usetz = FALSE)
    VERSION <- "4"
    signals <- c(.twsOutgoingMSG$REQ_HISTORICAL_DATA, VERSION, 
        as.character(tickerId), Contract$symbol, Contract$sectype, 
        Contract$expiry, Contract$strike, Contract$right, Contract$multiplier, 
        Contract$exch, Contract$primary, Contract$currency, Contract$local, 
        Contract$include_expired, endDateTime, barSize, duration, 
        useRTH, whatToShow, timeFormat)
    writeBin(signals, con)
    waiting <- TRUE
    response <- character(0)
    if (verbose) {
        cat("waiting for TWS reply on", Contract$symbol, "...")
        iter <- 1
        flush.console()
    }
    while (waiting) {
        if (!socketSelect(list(con), FALSE, 0.25)) 
            next
        curMsg <- readBin(con, character(), 1)
        if (verbose) {
            cat(".")
            if (iter%%30 == 0) 
                cat("\n")
            flush.console()
            iter <- iter + 1
        }
        if (length(curMsg) > 0) {
            if (curMsg == .twsIncomingMSG$ERR_MSG) {
                if (!errorHandler(con, verbose, OK = c(165, 300, 
                  366, 2104, 2106, 2107))) {
                  cat("failed.\n")
                  on.exit()
                  invisible(return())
                }
            }
            if (curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
                header <- readBin(con, character(), 5)
                nbin <- as.numeric(header[5]) * 9
                req.from <- header[3]
                req.to <- header[4]
                Sys.sleep(2)
                response <- readBin(con, character(), nbin)
                waiting <- FALSE
                if (verbose) {
                  cat(" done.\n")
                  flush.console()
                }
                on.exit()
            }
        }
    }
    if (missing(eventHistoricalData)) {
        cm <- matrix(response, ncol = 9, byrow = TRUE)
        cm[, 8] <- ifelse(cm[, 8] == "false", 0, 1)
        if (timeFormat == 2 && !nchar(cm[1, 1]) > 8) {
            dts <- structure(as.numeric(cm[, 1]), class = c("POSIXct", 
                "POSIXt"), tzone = tzone)
        }
        else {
            dts <- structure(as.numeric(as.POSIXlt(gsub("(\\d{4})(\\d{2})(\\d{2})", 
                "\\1-\\2-\\3", cm[, 1], perl = TRUE))), class = c("POSIXct", 
                "POSIXt"), tzone = tzone)
        }
        if (!missing(file)) {
            cm[, 1] <- dts
            write.table(cm, file = file, quote = FALSE, row.names = FALSE, 
                col.names = FALSE, sep = ",")
            invisible(return())
        }
        x <- xts(matrix(as.numeric(cm[, -1]), ncol = 8), order.by = dts, 
            tzone = tzone)
        localsymbol <- reqContractDetails(conn, Contract)[[1]]$contract$local
        colnames(x) <- paste(localsymbol, c("Open", "High", "Low", 
            "Close", "Volume", "WAP", "hasGaps", "Count"), sep = ".")
        xtsAttributes(x) <- list(from = req.from, to = req.to, 
            src = "IB", updated = Sys.time())
        return(x)
    }
    else if (is.null(eventHistoricalData)) {
        return(c(header, response))
    }
    else {
        FUN <- match.fun(eventHistoricalData)
        return(FUN(c(header, response)))
    }
}
