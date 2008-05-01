`reqMktData` <-
function (conn, Contract, tickGenerics='100,101,104,106,162,165,221,225,236',
          snapshot = FALSE, tickerId = "1", timeStamp=TRUE, callback, file, verbose=TRUE,
          eventTickPrice,eventTickSize,eventTickOption,eventTickGeneric,
          eventTickString,eventTickEFP) 
{
    if (class(conn) != "twsConnection") 
        stop("tws connection object required")

    if(class(Contract) == "twsContract") Contract <- list(Contract)

    for(n in 1:length(Contract)) {
      if (class(Contract[[n]]) != "twsContract") 
          stop("twsContract required")
    }

    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")

    cancelMktData <- function(con,tickerId) {
      for(i in 1:length(tickerId)) {
        writeBin(.twsOutgoingMSG$CANCEL_MKT_DATA,con)
        writeBin('1',con)
        writeBin(tickerId[i],con)
      }
    }

    # set up default event handlers
    if(missing(eventTickPrice))
      eventTickPrice <- e_tick_price
    if(missing(eventTickSize))
      eventTickSize  <- e_tick_size
    if(missing(eventTickOption)) 
      eventTickOption <- e_tick_option
    if(missing(eventTickGeneric)) 
      eventTickGeneric <- e_tick_generic
    if(missing(eventTickString)) 
      eventTickString <- e_tick_string
    if(missing(eventTickEFP))
      eventTickEFP <- e_tick_EFP

    snapshot <- ifelse(snapshot,"1","0")

    if(snapshot == '1' && missing(tickGenerics)) tickGenerics <- ''
  
    VERSION <- "7"
 
    ticker_id <- as.character(tickerId)

    for(n in 1:length(Contract)) {
      signals <- c(.twsOutgoingMSG$REQ_MKT_DATA, VERSION, ticker_id,
          Contract[[n]]$symbol, Contract[[n]]$sectype, Contract[[n]]$expiry, Contract[[n]]$strike, 
          Contract[[n]]$right, Contract[[n]]$multiplier, Contract[[n]]$exch, Contract[[n]]$primary, 
          Contract[[n]]$currency, Contract[[n]]$local,tickGenerics,snapshot)
  
  
      for (i in 1:length(signals)) {
          writeBin(signals[i], con)
      }
      ticker_id <- as.character(as.numeric(tickerId)+n)
    }

    on.exit(cancelMktData(con, as.character(as.numeric(tickerId):length(Contract))))
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
            if (curMsg == .twsIncomingMSG$TICK_PRICE) {
                contents <- readBin(con, character(), 6)
                eventTickPrice(curMsg,contents)
            }
            if (curMsg == .twsIncomingMSG$TICK_SIZE) {
                contents <- readBin(con, character(), 4)
                eventTickSize(curMsg,contents)
            }
            if (curMsg == .twsIncomingMSG$TICK_OPTION) {
                contents <- readBin(con, character(), 5)
                eventTickOption(curMsg,contents)
            }
            if (curMsg == .twsIncomingMSG$TICK_GENERIC) {
                contents <- readBin(con, character(), 4)
                eventTickGeneric(curMsg,contents)
            }
            if (curMsg == .twsIncomingMSG$TICK_STRING) {
                contents <- readBin(con, character(), 4)
                eventTickString(curMsg,contents)
                if(snapshot == '1') 
                  waiting <- FALSE
            }
            if (curMsg == .twsIncomingMSG$TICK_EFP) {
                contents <- readBin(con, character(), 13)
                cat('<efp> ')
                cat(curMsg,paste(contents),'\n')
            }
            flush.console()
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

