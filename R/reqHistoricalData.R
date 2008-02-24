`reqHistoricalData` <-
function(conn,Contract,endDateTime,
         barSize='1 day',duration='1 M',
         useRTH='1',whatToShow='TRADES',time.format='1')
{
  if(class(conn) != 'twsConnection') stop('tws connection object required')
  if(class(Contract) != 'twsContract') stop('twsContract required')

  if(!barSize %in% c('1 secs','5 secs','15 secs','30 secs',
                     '1 min', '2 mins','3 mins','5 mins','15 mins',
                     '30 mins','1 hour','1 day','1 week','1 month',
                     '3 months','1 year'))
    stop('unknown barSize')

  con <- conn[[1]]

  if(missing(endDateTime)) 
    endDateTime <- strftime(
                     as.POSIXlt(as.POSIXct('1970-01-01')+reqCurrentTime(con)),
                     format='%Y%m%d %H:%M:%S',use=TRUE)
  # signals server version = 37
  #
  # REQ_HISTORICAL_DATA, VERSION, TICKER_ID,
  # C$symbol, C$sectype, C$expiry, C$strike,
  # C$right, C$multiplier, C$exch, C$primary,
  # C$currency, C$local, C$include_expired,
  #
  # end_date_time, bar_size, duration,
  # use_rth, what_to_show, format_date

  signals <- c(.twsMSG$REQ_HISTORICAL_DATA, # '20'
               '4', # version
               '1', # tick id
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
  readChar(con,10000)

  for(i in 1:length(signals)) {
    writeChar(signals[i],con)
  }

  waiting <- TRUE           # waiting for valid response?
  response <- character(0)  # currently read response
  datafarmmsg <- FALSE      # data farm msg(s)?

  while(1) {
    curChar <- readChar(con,1)
    # loop until first character is had
    if(waiting & identical(curChar,character(0))) next
    waiting <- FALSE        # if here - a valid char has been read

    if(!is.na(response[1]) & response[1]=='4')
      datafarmmsg <- TRUE   # if first char is 4 - ignore this response

    if(identical(curChar,character(0))) {  # end of message
      if(datafarmmsg) {     # if data message - reset loop
        response <- character(0)
        datafarmmsg <- FALSE
        waiting <- TRUE
        next
      } else break          # end of valid message
    }

    if(charToRaw(curChar)=='00') {
      # replace \0 in reply with `|` bars
      response[length(response)+1] <- '|'
    } else response[length(response)+1] <- curChar
  }
#  rawToChar(charToRaw(readChar(con,10000))[-c(1:5,16)])
  cv <- strsplit(paste(response,collapse=''),'\\|')[[1]]
  cm <- matrix(cv[-(1:6)],nc=9,byrow=TRUE)
  cm[,8] <- ifelse(cm[,8]=='false',0,1)
  dts <- gsub('(\\d{4})(\\d{2})(\\d{2})','\\1-\\2-\\3',cm[,1],perl=TRUE)
  x <- xts(matrix(as.numeric(cm[,-1]),nc=8),order.by=as.POSIXct(dts))
  colnames(x) <- c('Open','High','Low','Close','Volume',
                   'WAP','hasGaps','Count')
  xtsAttributes(x) <- list(start=cv[4],end=cv[5],misc=cv[1:3])
  x
}

