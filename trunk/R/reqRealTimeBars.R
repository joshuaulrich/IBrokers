`reqRealTimeBars` <-
function (conn, Contract,
          whatToShow="TRADES",
          barSize="5",useRTH="1",
          tickerId = "1",
          file = "",
          verbose=TRUE,
          eventRealTimeBars,
          CALLBACK,
          ...)
{
    if (class(conn) != "twsConnection") 
        stop("tws connection object required")

    if (class(Contract) == "twsContract") 
        Contract <- list(Contract)

    for (n in 1:length(Contract)) {
        if (class(Contract[[n]]) != "twsContract") 
            stop("twsContract required")
    }

    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")

    cancelRealTimeBars <- function(con,tickerId) {
      for(i in 1:length(tickerId)) {
        writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS,con)
        writeBin('1',con)
        writeBin(tickerId[i],con)
      }
    }

    if( missing(CALLBACK)) {
      if( missing(eventRealTimeBars))
        eventRealTimeBars <- event_real_time_bars
    } else if( is.null(CALLBACK)) {
        eventRealTimeBars <- NULL
    }


    VERSION <- "1"
    ticker_id <- as.character(tickerId)
 
    for(n in 1:length(Contract)) {
      signals <- c(.twsOutgoingMSG$REQ_REAL_TIME_BARS, VERSION, ticker_id, 
          Contract[[n]]$symbol,
          Contract[[n]]$sectype,
          Contract[[n]]$expiry,
          Contract[[n]]$strike, 
          Contract[[n]]$right,
          Contract[[n]]$multiplier,
          Contract[[n]]$exch, 
          Contract[[n]]$primary, 
          Contract[[n]]$currency,
          Contract[[n]]$local,barSize,whatToShow,useRTH)
  
      for (i in 1:length(signals)) {
          writeBin(signals[i], con)
      }
      ticker_id <- as.character(as.numeric(tickerId) + n)
    }

    on.exit(cancelRealTimeBars(con, as.character(as.numeric(tickerId):length(Contract))))
    waiting <- TRUE

    if( missing(CALLBACK) || is.null(CALLBACK)) {
      while (waiting) {
          curMsg <- suppressWarnings(readBin(con, character(), 
              1))
          if (length(curMsg) > 0) {
              if (curMsg == .twsIncomingMSG$ERR_MSG) {
                  if (!errorHandler(con, verbose, OK = c(165, 300, 366, 2104,2106,2107))) {
                    cat("\n")
                    stop("Unable to complete market data request")
                  }
              }
              if( curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
                contents <- readBin(con,character(),10)
                if(is.null(eventRealTimeBars)) {
                  cat(paste(contents),'\n',file=file, append=TRUE)
                } else eventRealTimeBars(curMsg, contents, file=file)
              }
          flush.console()
          }
      }
    } else CALLBACK(con,...)
}

