`reqRealTimeBars` <-
function (conn, Contract,
          whatToShow="TRADES",
          barSize="5",useRTH="1",
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
            Contract[[n]]$local,barSize,whatToShow,useRTH)
    
        for (i in 1:length(signals)) {
            writeBin(signals[i], con)
        }
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

    #waiting <- TRUE

    #msg_position <- 0 # where we are in the message - only relevant for playback
    #PLAYBACK <- ifelse(inherits(conn,'twsPlayback'), TRUE, FALSE)
    timeStamp <- NULL

    CALLBACK(conn, eWrapper=eventWrapper, timestamp=timeStamp, file=file,
             playback=playback, ...)

#    if( missing(CALLBACK) || is.null(CALLBACK)) {
#      while (waiting) {
#
#          curMsg <- readBin(con, character(), 1)
#          msg_position <- msg_position + 1
#
#
#        #  if (length(curMsg) > 0) {
#              if (curMsg == .twsIncomingMSG$ERR_MSG) {
#                if (!errorHandler(con, verbose, OK = c(165, 300, 366, 2104,2106,2107))) {
#                  cat("\n")
#                  stop("Unable to complete market data request")
#                }
#                msg_position <- msg_position + 4
#              }
#              if( curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
#                contents <- readBin(con,character(),10)
#                if(is.null(eventRealTimeBars)) {
#                  cat(curMsg,paste(contents),'\n',file=file, append=TRUE)
#                } else eventRealTimeBars(curMsg, contents, file=file)
#                msg_position <- msg_position + 10
#              }
#          flush.console()
#          if(PLAYBACK)
#            Sys.sleep(as.numeric(barSize) * playback)
#          if(!is.na(msg_expected_length) && msg_position == msg_expected_length) 
#            waiting <- FALSE
#        # }
#      }
#    } else CALLBACK(con,...)
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
