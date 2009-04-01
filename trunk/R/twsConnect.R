twsConnect <-
function (clientId=1, host='localhost', port = 7496, verbose=TRUE,
          timeout=5, filename=NULL, blocking=FALSE)
 {
   if(is.null(getOption('digits.secs'))) 
     options(digits.secs=6)

   if(is.character(clientId))
     filename <- clientId

   if(is.null(filename)) {
     start.time <- Sys.time()
     s <- socketConnection(host = host, port = port,
                           open='ab', blocking=blocking)

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
         if(curMsg %in% as.character(39:44)) {
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
  } else { 
    #file is defined - not a real connection
    fh <- file(filename,open='r')
    dat <- scan(fh, what=character(), quiet=TRUE)
    close(fh)

    tmp <- tempfile()
    fh <- file(tmp, open='ab')

    #writeBin(c(as.character(length(dat)),dat), fh)
    writeBin(dat, fh)
    #for(i in dat) writeBin(i, fh)

    close(fh)
    s <- file(tmp, open='rb')

    structure(list(s,
                   clientId=NULL,port=NULL,
                   server.version=NULL,
                   connected.at=filename), 
                   class = c("twsPlayback","twsConnection"))

  }
}

is.twsConnection <- function(x)
{
  inherits(x, "twsConnection")
}

is.twsPlayback <- function(x)
{
  inherits(x, "twsPlayback")
}

isConnected <- function(x)
{
  if(is.twsConnection(x)) {
    if(inherits(try(isOpen(x[[1]]), silent=TRUE), 'try-error')) {
      FALSE
    } else TRUE 
  } else isOpen(x)
} 
