`twsConnect` <-
function (clientId=1, host='localhost', port = 7496, verbose=TRUE,
          timeout=5)
 {
     if(is.null(getOption('digits.secs'))) 
       options(digits.secs=6)
     start.time <- Sys.time()
     s <- socketConnection(host = host, port = port,
                           open='ab', blocking=TRUE)

     if(!isOpen(s)) { 
       close(s)
       stop(paste("couldn't connect to TWS on port",port))
     }

     CLIENT_VERSION <- "38"

     writeBin(CLIENT_VERSION, s)
     writeBin(as.character(clientId), s)

     waiting <- TRUE
     while(waiting) {
       curMsg <- readBin(s, character(), 1)
       if(length(curMsg) > 0) {
         if(curMsg %in% c('39','40')) {
           SERVER_VERSION <- curMsg
           CONNECTION_TIME <- readBin(s,character(),1)
         }
         if(curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
           NEXT_VALID_ID <- readBin(s,character(),2)[2]
           waiting <- FALSE
         }
         if(curMsg == .twsIncomingMSG$ERR_MSG) {
           if(!errorHandler(s,verbose)) stop()
         }
       }
       if(Sys.time()-start.time > timeout) {
         close(s)
         stop('tws connection timed-out')
       }
     }

     structure(list(s,
                    clientId=clientId,port=port,
                    server.version=SERVER_VERSION,
                    connected.at=CONNECTION_TIME), 
                    class = "twsConnection")
 }
