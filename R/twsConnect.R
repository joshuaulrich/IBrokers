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

`twsConnect` <-
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

