.reqRealTimeBars <- function(conn, Contract,
          whatToShow="TRADES",
          barSize="5",useRTH=TRUE,
          tickerId = "1")
{
  if(!inherits(conn,"twsConnection") )
    stop("tws connection object required")
  if(inherits(Contract,"twsContract")) 
    Contract <- list(Contract)

  for(n in 1:length(Contract)) {
    if(!inherits(Contract[[n]],"twsContract") )
       stop("twsContract required")
  }
  con <- conn[[1]]
  if(!isOpen(con)) 
    stop("connection to TWS has been closed")

 
  VERSION <- "1"
  if(length(tickerId) != length(Contract))
    tickerId <- seq(as.numeric(tickerId), length.out=length(Contract))

  ticker_id <- as.character(tickerId)

  for(n in 1:length(Contract)) {
    request <- c(.twsOutgoingMSG$REQ_REAL_TIME_BARS, VERSION, ticker_id[n], 
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
    writeBin(request, con)
#    ticker_id <- as.character(as.numeric(tickerId))
#    ticker_ids[n] <- ticker_id
  }
  ticker_id
}

reqRealTimeBars <-
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
    if(!inherits(conn, "twsPlayback")) {
  
    }

    tickerId <- .reqRealTimeBars(conn, Contract, whatToShow, barSize, useRTH, tickerId)

    if(inherits(Contract,"twsContract")) 
      Contract <- list(Contract)

    con <- conn[[1]]
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

    if(!missing(CALLBACK) && is.na(list(CALLBACK))) {
      if(inherits(conn, 'twsPlayback')) {
        seek(conn[[1]], 0)
        stop("CALLBACK=NA is not available for playback")
      }
      return(tickerId)
    }
    on.exit(cancelRealTimeBars(con, tickerId))
    
    #if(missing(eventWrapper)) {
      eventWrapper$assign.Data("symbols", sapply(Contract, `[[`, "symbol"))
    #}

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
