`reqMktData` <-
function (conn, Contract, tickGenerics='100,101,104,106,162,165,221,225,236',
          snapshot = FALSE, tickerId = "1", timeStamp=TRUE,
          file='', verbose=TRUE,
          eventTickPrice,eventTickSize,
          eventTickOption,eventTickGeneric,
          eventTickString,eventTickEFP,CALLBACK,...) 
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

    if(!is.character(timeStamp) & timeStamp) {
      timeStamp <- "%Y%m%d %H:%M:%OS"
    } else {
      timeStamp <- NULL
    }
    # set up default event handlers, if
    # callback is not set
    if(missing(CALLBACK)) {
      if(missing(eventTickPrice))
        eventTickPrice   <- e_tick_price
      if(missing(eventTickSize))
        eventTickSize    <- e_tick_size
      if(missing(eventTickOption)) 
        eventTickOption  <- e_tick_option
      if(missing(eventTickGeneric)) 
        eventTickGeneric <- e_tick_generic
      if(missing(eventTickString)) 
        eventTickString  <- e_tick_string
      if(missing(eventTickEFP))
        eventTickEFP     <- e_tick_EFP
    } 
    else if(is.null(CALLBACK)) {
        eventTickPrice   <- NULL
        eventTickSize    <- NULL
        eventTickOption  <- NULL
        eventTickGeneric <- NULL
        eventTickString  <- NULL
        eventTickEFP     <- NULL
    }


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
    if(missing(CALLBACK) || is.null(CALLBACK)) {
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
          if (curMsg == .twsIncomingMSG$TICK_PRICE) {
              contents <- readBin(con, character(), 6)
              if(is.null(eventTickPrice)) {
                if (!is.null(timeStamp)) cat(format(Sys.time(), timeStamp),' ',file=file,append=TRUE)
                cat(paste(contents),'\n',file=file, append=TRUE)
              } else eventTickPrice(curMsg,contents,timeStamp,file)
          }
          if (curMsg == .twsIncomingMSG$TICK_SIZE) {
              contents <- readBin(con, character(), 4)
              if(is.null(eventTickSize)) {
                if (!is.null(timeStamp)) cat(format(Sys.time(), timeStamp),' ',file=file,append=TRUE)
                cat(paste(contents),'\n',file=file, append=TRUE)
              } else eventTickSize(curMsg,contents,timeStamp,file)
          }
          if (curMsg == .twsIncomingMSG$TICK_OPTION) {
              contents <- readBin(con, character(), 5)
              if(is.null(eventTickOption)) {
                if (!is.null(timeStamp)) cat(format(Sys.time(), timeStamp),' ',file=file,append=TRUE)
                cat(paste(contents),'\n',file=file, append=TRUE)
              } else eventTickOption(curMsg,contents,timeStamp,file)
          }
          if (curMsg == .twsIncomingMSG$TICK_GENERIC) {
              contents <- readBin(con, character(), 4)
              if(is.null(eventTickGeneric)) {
                if (!is.null(timeStamp)) cat(format(Sys.time(), timeStamp),' ',file=file,append=TRUE)
                cat(paste(contents),'\n',file=file, append=TRUE)
              } else eventTickGeneric(curMsg,contents,timeStamp,file)
          }
          if (curMsg == .twsIncomingMSG$TICK_STRING) {
              contents <- readBin(con, character(), 4)
              if(is.null(eventTickString)) {
                if (!is.null(timeStamp)) cat(format(Sys.time(), timeStamp),' ',file=file,append=TRUE)
                cat(paste(contents),'\n',file=file, append=TRUE)
              } else eventTickString(curMsg,contents,timeStamp,file)
              if(snapshot == '1') 
                waiting <- FALSE
          }
          if (curMsg == .twsIncomingMSG$TICK_EFP) {
              contents <- readBin(con, character(), 13)
              if(is.null(eventTickEFP)) {
                if (!is.null(timeStamp)) cat(format(Sys.time(), timeStamp),' ',file=file,append=TRUE)
                cat(paste(contents),'\n',file=file, append=TRUE)
              } else {
                cat('<efp> ')
                cat(curMsg,paste(contents),'\n')
              }
          }
          flush.console()
        }
      }
    } else CALLBACK(con,...)
}
