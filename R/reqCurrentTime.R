`reqCurrentTime` <-
function(con) {
  con <- con[[1]]
  # TWS expects nul endings
  # this requires extra calls
  # from R
  readChar(con,10000) # flush any residual TWS messages

#  writeChar(.twsMSG['REQ_CURRENT_TIME'],con,eos=NULL)
#  writeChar('',con)
#  writeChar('1',con,eos=NULL)
#  writeChar('',con)

  writeChar(.twsMSG$REQ_CURRENT_TIME,con)
  writeChar('1',con)

  # removes the need to Sys.sleep
  waiting <- TRUE
  response <- character(0)

  while(1) {
    curChar <- readChar(con,1)
    if(waiting & identical(curChar,character(0))) next
    waiting <- FALSE

    if(identical(curChar,character(0))) break

    if(charToRaw(curChar)=='00') {
      # replace \0 in reply with blanks
      response[length(response)+1] <- ' '
    } else response[length(response)+1] <- curChar
  }
  as.numeric(strsplit(paste(response,collapse=''),' ')[[1]])[3]
#
#function(con) {
#  writeChar('49',con,eos=NULL)
#  writeChar('',con)
#  writeChar('1',con,eos=NULL)
#  writeChar('',con)
#  Sys.sleep(1)
#  rawToChar(charToRaw(readChar(con,100))[-c(1:5,16)])
}

