`reqRealTimeBars` <-
function (conn, Contract,
          whatToShow="TRADES",
          barSize="5",useRTH=TRUE,
          playback = 1,
          tickerId = "1",
          file = "",
          verbose=TRUE,
          eventWrapper=eWrapper(),
          CALLBACK=twsCALLBACK,
          ...)
{
    if (!inherits(conn,"twsConnection") )
        stop("tws connection object required")

    if(!inherits(conn, "twsPlayback")) {
      if (inherits(Contract,"twsContract")) 
          Contract <- list(Contract)
  
      for (n in 1:length(Contract)) {
          if (!inherits(Contract[[n]],"twsContract") )
              stop("twsContract required")
      }
    }

    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")


    cancelRealTimeBars <- function(con,tickerId) {
      if(inherits(con,'sockconn')) {
        for(i in 1:length(tickerId)) {
          writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS,con)
          writeBin('1',con)
          writeBin(tickerId[i],con)
        }
      } else {
        seek(con, 0)
      }
    }
  
    if(is.null(CALLBACK))
      CALLBACK <- twsDEBUG # function to simply return raw data
 
    VERSION <- "1"
    ticker_id <- as.character(tickerId)

    if(inherits(con, 'sockconn')) {
      #write to TWS connection 
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
            Contract[[n]]$local,barSize,whatToShow,
            as.character(as.numeric(useRTH)))
    
        writeBin(signals, con)
        ticker_id <- as.character(as.numeric(tickerId) + n)
      }
      msg_expected_length <- NA
    } else {
      msg_expected_length <- as.numeric(readBin(con,character(), 1))
    }

    if(!missing(CALLBACK) && is.na(list(CALLBACK))) {
      if(inherits(conn, 'twsPlayback')) {
        seek(conn[[1]], 0)
        stop("CALLBACK=NA is not available for playback")
      }
      return(as.character(as.numeric(tickerId):length(Contract)))
    }
    on.exit(cancelRealTimeBars(con, as.character(as.numeric(tickerId):length(Contract))))

    timeStamp <- NULL
    CALLBACK(conn, eWrapper=eventWrapper, timestamp=timeStamp, file=file,
             playback=playback, ...)
}

`cancelRealTimeBars` <- function(conn,tickerId) {
      if(!inherits(conn,"twsConnection"))
        stop("twsConnection object required")

      con <- conn[[1]]

      for(i in 1:length(tickerId)) {
        writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS,con)
        writeBin('1',con)
        writeBin(tickerId[i],con)
      }
    }
