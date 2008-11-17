`reqMktDepth` <-
function (conn, Contract, tickerId = "1", numRows="20",
          timeStamp=TRUE, playback=1,
          file='', verbose=TRUE,
          eventUpdateMktDepth, eventUpdateMktDepthL2,
          CALLBACK,...) 
{
    if (!inherits(conn, "twsConnection"))
        stop("tws connection object required")

    if(!inherits(conn, 'twsPlayback')) {
      # if playback from a file, don't test or require contract
      if(class(Contract) == "twsContract") Contract <- list(Contract)
  
      for(n in 1:length(Contract)) {
        if (class(Contract[[n]]) != "twsContract") 
            stop("twsContract required")
      }
    }

    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")

    cancelMktDepth <- function(con,tickerId) {
      if(inherits(con, 'sockconn')) {
        for(i in 1:length(tickerId)) {
          writeBin(.twsOutgoingMSG$CANCEL_MKT_DEPTH,con)
          writeBin('1',con)
          writeBin(tickerId[i],con)
        }
      } else {
        # reset to beginning of file
        seek(con,0)
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
      if(missing(eventUpdateMktDepth))
        eventUpdateMktDepth <- e_update_mkt_depth
      if(missing(eventUpdateMktDepthL2))
        eventUpdateMktDepthL2 <- e_update_mkt_depthL2
    } else if(is.null(CALLBACK)) {
        # return raw data
        eventUpdateMktDepth   <- NULL
        eventUpdateMktDepthL2 <- NULL
    }

    VERSION <- "3"
 
    ticker_id <- as.character(tickerId)

    if(inherits(con, 'sockconn')) {
      # write to live TWS Connection
      for(n in 1:length(Contract)) {
        signals <- c(.twsOutgoingMSG$REQ_MKT_DEPTH,
                     VERSION, 
                     ticker_id,
                     Contract[[n]]$symbol,
                     Contract[[n]]$sectype,
                     Contract[[n]]$expiry,
                     Contract[[n]]$strike, 
                     Contract[[n]]$right,
                     Contract[[n]]$multiplier,
                     Contract[[n]]$exch,
                     Contract[[n]]$currency,
                     Contract[[n]]$local,
                     numRows)
    
    
        for (i in 1:length(signals)) {
            writeBin(signals[i], con)
        }
        ticker_id <- as.character(as.numeric(tickerId)+n)
      }
      msg_expected_length <- NA
    } else {
      msg_expected_length <- as.numeric(readBin(con, character(), 1))
    } 

    if(!missing(CALLBACK) && is.na(list(CALLBACK))) {
      if(inherits(conn, 'twsPlayback')) {
        seek(conn[[1]], 0)
        stop("CALLBACK=NA is not available for playback")
      }
      return(as.character(as.numeric(tickerId):length(Contract)))
    }
    on.exit(cancelMktDepth(con, as.character(as.numeric(tickerId):length(Contract))))

    waiting <- TRUE
#    response <- character(0)

    msg_length <- ifelse(inherits(conn, 'twsPlayback'), 3, 1)
    msg_position <- 0  # only relevant for playback???? why???
    sys.time <- NULL   # timeStamp interpretation

#    if (.Platform$OS == "windows") 
#        Sys.sleep(0.1)
    if(missing(CALLBACK) || is.null(CALLBACK)) {
      while (waiting) {
        curMsg <- readBin(con, character(), msg_length)

        if(!is.null(timeStamp)) {
          if(msg_length > 1) {
            last.time <- sys.time
            sys.time <- as.POSIXct(paste(curMsg[1:2],collapse=' '))
            if(!is.null(last.time)) {
              Sys.sleep((sys.time-last.time)*playback)
            }   
          } else sys.time <- Sys.time()
        } else sys.time <- NULL

        curMsg <- curMsg[msg_length] 

        msg_position <- msg_position + msg_length 


        if (length(curMsg) > 0) {
          if (curMsg == .twsIncomingMSG$ERR_MSG) {
              if (!errorHandler(con, verbose, OK = c(165, 300, 366, 2104,2106,2107))) {
                cat("\n")
                stop("Unable to complete market depth request")
              }
              msg_position <- msg_position + 4
          }
          if (curMsg == .twsIncomingMSG$MARKET_DEPTH) {
              contents <- readBin(con, character(), 7)
              if(is.null(eventUpdateMktDepth)) {
                if(!is.null(timeStamp)) cat(as.character(sys.time),' ',file=file,append=TRUE)
                cat(curMsg,paste(contents),'\n',file=file,append=TRUE)
              } else eventUpdateMktDepth(curMsg,contents,sys.time,file)
              msg_position <- msg_position + 7
          }
          if (curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
              contents <- readBin(con, character(), 8)
              if(is.null(eventUpdateMktDepthL2)) {
                if(!is.null(timeStamp)) cat(as.character(sys.time),' ',file=file,append=TRUE)
                cat(curMsg,paste(contents),'\n',file=file, append=TRUE)
              } else eventUpdateMktDepthL2(curMsg,contents,sys.time,file)
              msg_position <- msg_position + 8
          }
          flush.console()
          if(!is.na(msg_expected_length) && msg_position == msg_expected_length)
            waiting <- FALSE
        }
      }
    } else CALLBACK(con,...)
}

`cancelMktDepth` <- function(conn,tickerId) {
  if(!inherits(conn, "twsConnection"))
    stop("twsConnection object required")

  con <- conn[[1]]

  for(i in 1:length(tickerId)) {
    writeBin(.twsOutgoingMSG$CANCEL_MKT_DEPTH,con)
    writeBin('1',con)
    writeBin(tickerId[i],con)
  }
}

