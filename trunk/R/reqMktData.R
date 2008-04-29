`reqMktData` <-
function (conn, Contract, tickGenerics='100,101,104,106,162,165,221,225,236',
          snapshot = FALSE, tickerId = "1", timeStamp=TRUE, callback, file, verbose=TRUE,
          eventTickPrice,eventTickSize,eventTickOption,eventTickGeneric,
          eventTickString,eventTickEFP) 
{
    if (class(conn) != "twsConnection") 
        stop("tws connection object required")
    if (class(Contract) != "twsContract") 
        stop("twsContract required")

    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")

    cancelMktData <- function(con,tickerId,snapshot) {
      if(snapshot == "0") {
        writeBin(.twsOutgoingMSG$CANCEL_MKT_DATA,con)
        writeBin('1',con)
        writeBin(tickerId,con)
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
 
    signals <- c(.twsOutgoingMSG$REQ_MKT_DATA, VERSION, as.character(tickerId), 
        Contract$symbol, Contract$sectype, Contract$expiry, Contract$strike, 
        Contract$right, Contract$multiplier, Contract$exch, Contract$primary, 
        Contract$currency, Contract$local,tickGenerics,snapshot)

    on.exit(cancelMktData(con, as.character(tickerId),snapshot))

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
            if (curMsg == .twsIncomingMSG$TICK_PRICE) {
                header <- readBin(con, character(), 6)
                #cat('<price> ')
                #cat(curMsg,paste(header),'\n')
                eventTickPrice(curMsg,header)
            }
            if (curMsg == .twsIncomingMSG$TICK_SIZE) {
                header <- readBin(con, character(), 4)
                cat('<size> ')
                cat(curMsg,paste(header),'\n')
            }
            if (curMsg == .twsIncomingMSG$TICK_OPTION) {
                header <- readBin(con, character(), 5)
                cat('<option> ')
                cat(curMsg,paste(header),'\n')
            }
            if (curMsg == .twsIncomingMSG$TICK_GENERIC) {
                header <- readBin(con, character(), 4)
                cat('<generic> ')
                cat(curMsg,paste(header),'\n')
            }
            if (curMsg == .twsIncomingMSG$TICK_STRING) {
                header <- readBin(con, character(), 4)
                cat('<string> ')
                cat(curMsg,paste(header),'\n')
                if(snapshot == '1') 
                  waiting <- FALSE
            }
            if (curMsg == .twsIncomingMSG$TICK_EFP) {
                header <- readBin(con, character(), 13)
                cat('<efp> ')
                cat(curMsg,paste(header),'\n')
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

