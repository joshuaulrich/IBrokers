`twsConnect` <-
function (clientId=1, host='localhost', port = 7496)
 {
     s <- socketConnection(host = host, port = port, open='ab')

     if(!isOpen(s)) { 
       close(s)
       stop(paste("couldn't connect to TWS on port",port))
     }

     writeBin("37", s)
     waiting <- TRUE
     while(waiting) {
       response <- readBin(s,character(),1)
       if(length(response) > 0 && response %in% c('39','40')) {
         SERVER_VERSION <- response
         while(waiting) {
           response <- readBin(s,character(),1)
           if(length(response) > 0) {
             CONNECTION_TIME <- response
             waiting <- FALSE
           }
         }
       }
     }

     writeBin(as.character(clientId), s)

     structure(list(s,
                    clientId=clientId,port=port,
                    server.version=SERVER_VERSION,
                    connected.at=CONNECTION_TIME), 
                    class = "twsConnection")
 }

`twsConnect2` <-
function (port = 7496, clientID = 1)
 {
     s <- socketConnection(port = port)
     writeChar("37", s)
     writeChar(as.character(clientID), s)
     waiting <- TRUE
     while (1) {
         curChars <- readChar(s,1000)
         #curChar <- readChar(s, 1)
         if (waiting & identical(curChars, character(0)))
             next
         waiting <- FALSE
 
         # process what is captured thus far
         pc <- parseTWS(curChars)
         SERVERINFO <- FALSE
 
         if(!SERVERINFO & length(pc) > 0) {
           SERVER_VERSION <- pc[1]
           CONNECTION_TIME <- pc[2]
           SERVERINFO <- TRUE
         }
 
         if (identical(curChars, character(0)))
             break
     }
     structure(list(s,SERVER_VERSION,CONNECTION_TIME), class = "twsConnection")
 }

`twsConnect0` <-
function(port=7496,clientID=1) {
  s <- socketConnection(port=port)
  writeChar('37',s)
  writeChar(as.character(clientID),s)
#  writeChar('37',s,eos=NULL)
#  writeChar('',s)
#  writeChar('1',s,eos=NULL)
#  writeChar('',s)

  waiting <- TRUE

  while(1) {
    curChar <- readChar(s,1)
    if(waiting & identical(curChar,character(0))) next
    waiting <- FALSE

    if(identical(curChar,character(0))) break

#    if(charToRaw(curChar)=='00') {
#      # replace \0 in reply with blanks
#      response[length(response)+1] <- ' '
#    } else response[length(response)+1] <- curChar
  }
#  readChar(s,255)
  structure(list(s),class='twsConnection')
}

