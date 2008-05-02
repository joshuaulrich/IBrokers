`reqMktDepth` <-
function (conn, Contract, tickerId = "1", numRows="20",
          timeStamp="%Y%m%d %H:%M:%OS",
          file='', verbose=TRUE,
          eventUpdateMktDepth, eventUpdateMktDepthL2,
          CALLBACK,...) 
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

    cancelMktDepth <- function(con,tickerId) {
      for(i in 1:length(tickerId)) {
        writeBin(.twsOutgoingMSG$CANCEL_MKT_DEPTH,con)
        writeBin('1',con)
        writeBin(tickerId[i],con)
      }
    }


    # set up default event handlers, if
    # callback is not set
    if(missing(CALLBACK)) {
      if(missing(eventUpdateMktDepth))
        eventUpdateMktDepth <- e_update_mkt_depth
      if(missing(eventUpdateMktDepthL2))
        eventUpdateMktDepthL2 <- e_update_mkt_depthL2
    }

    VERSION <- "3"
 
    ticker_id <- as.character(tickerId)

    for(n in 1:length(Contract)) {
      signals <- c(.twsOutgoingMSG$REQ_MKT_DEPTH, VERSION, ticker_id,
          Contract[[n]]$symbol, Contract[[n]]$sectype, Contract[[n]]$expiry, Contract[[n]]$strike, 
          Contract[[n]]$right, Contract[[n]]$multiplier, Contract[[n]]$exch,
          Contract[[n]]$currency, Contract[[n]]$local,numRows)
  
  
      for (i in 1:length(signals)) {
          writeBin(signals[i], con)
      }
      ticker_id <- as.character(as.numeric(tickerId)+n)
    }

    on.exit(cancelMktDepth(con, as.character(as.numeric(tickerId):length(Contract))))
    waiting <- TRUE
    response <- character(0)

    if (.Platform$OS == "windows") 
        Sys.sleep(0.1)
    if(missing(CALLBACK)) {
      while (waiting) {
        curMsg <- suppressWarnings(readBin(con, character(), 
            1))
        if (length(curMsg) > 0) {
          if (curMsg == .twsIncomingMSG$ERR_MSG) {
              if (!errorHandler(con, verbose, OK = c(165, 300, 366, 2104,2106,2107))) {
                cat("\n")
                stop("Unable to complete market depth request")
              }
          }
          if (curMsg == .twsIncomingMSG$MARKET_DEPTH) {
              contents <- readBin(con, character(), 7)
              if(is.null(eventUpdateMktDepth)) {
                cat(paste(contents),'\n',file=file)
              } else eventUpdateMktDepth(curMsg,contents,timeStamp,file)
          }
          if (curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
              contents <- readBin(con, character(), 8)
              eventUpdateMktDepthL2(curMsg,contents,timeStamp,file)
          }
          flush.console()
        }
      }
    } else CALLBACK(con,...)
}
