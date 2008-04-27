`reqHistoricalData` <-
function(conn,Contract,endDateTime,
         barSize='1 day',duration='1 M',
         useRTH='1',whatToShow='TRADES',time.format='1',
         verbose=TRUE, tickerId='1', timeout=10)
{
  start.time <- Sys.time()

  if(class(conn) != 'twsConnection') stop('tws connection object required')
  if(class(Contract) != 'twsContract') stop('twsContract required')

  if(!barSize %in% c('1 secs','5 secs','15 secs','30 secs',
                     '1 min', '2 mins','3 mins','5 mins','15 mins',
                     '30 mins','1 hour','1 day','1 week','1 month',
                     '3 months','1 year'))
    stop('unknown barSize')

  con <- conn[[1]]

  if(!isOpen(con)) stop("connection to TWS has been closed")

  on.exit(cancelHistoricalData(con,as.character(tickerId)))

  if(missing(endDateTime)) 
    endDateTime <- strftime(
                     as.POSIXlt(as.POSIXct('1970-01-01')+
                     as.numeric(reqCurrentTime(con))),
                     format='%Y%m%d %H:%M:%S',use=FALSE)

  signals <- c(.twsOutgoingMSG$REQ_HISTORICAL_DATA, # '20'
               '4', # version
               as.character(tickerId),
               Contract$symbol, Contract$sectype,
               Contract$expiry, Contract$strike,
               Contract$right,  Contract$multiplier,
               Contract$exch,   Contract$primary,
               Contract$currency, Contract$local,
               Contract$include_expired,
               endDateTime, barSize, duration, useRTH,
               whatToShow, time.format)
#  signals <- c('20','4','1',
#               'QQQQ','STK','',
#               '0.0','','',
#               'SMART','ISLAND','USD',
#               '','0','20080219 21:11:41 GMT',
#               '1 day','1 M','1',
#               'TRADES','1')
  readBin(con,character(),100) # flush the input stream

  for(i in 1:length(signals)) {
    writeBin(signals[i],con)
  }

  waiting <- TRUE           # waiting for valid response?
  response <- character(0)  # currently read response

  if(verbose) {
    cat('waiting for TWS reply ...')
    iter <- 1
    flush.console()
  }

  while(waiting) {
    curMsg <- readBin(con,character(),1)
    if(verbose) {
      cat('.')
      if(iter %% 30 == 0) cat('\n')
      flush.console()
      iter <- iter + 1
    }

    if(length(curMsg) > 0) {
      if(curMsg == .twsIncomingMSG$ERR_MSG) {
        if(!errorHandler(con,verbose,OK=c(165,2106))) {
          cat('\n')
          stop('Unable to complete historical data request')
        }
      }
      if(curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
        header <- readBin(con,character(),5)
        nbin <- as.numeric(header[5])*9
        req.from <- header[3]
        req.to   <- header[4]
        response <- readBin(con,character(),nbin)
        waiting <- FALSE
        if(verbose) {
          cat(' done.\n')
          flush.console()
        }
      }
    }
    # need to add waiting test condition
    if(Sys.time() - start.time > timeout) {
      cancelHistoricalData(con,as.character(tickerId))
      cat('\n')
      stop("historical data request timed-out")
    }

    Sys.sleep(.5)
  }

  
  cm <- matrix(response,nc=9,byrow=TRUE)
  cm[,8] <- ifelse(cm[,8]=='false',0,1)
  dts <- gsub('(\\d{4})(\\d{2})(\\d{2})','\\1-\\2-\\3',cm[,1],perl=TRUE)
  x <- xts(matrix(as.numeric(cm[,-1]),nc=8),order.by=as.POSIXct(dts))
  colnames(x) <- c('Open','High','Low','Close','Volume',
                   'WAP','hasGaps','Count')
  xtsAttributes(x) <- list(from=req.from,to=req.to)
  x
}

