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

