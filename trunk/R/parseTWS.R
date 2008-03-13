`parseTWS` <-
function(char) {
  if(length(char) == 1) {
    charv <- sapply(charToRaw(char),function(x) ifelse(x=='00','|',rawToChar(x)))
    strsplit(paste(charv,collapse=''),'\\|')[[1]]
  }
}

