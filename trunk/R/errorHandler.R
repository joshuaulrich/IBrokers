`errorHandler` <-
function(con, verbose, OK=NULL) {
  err <- readBin(con,character(),4)

  if(as.numeric(err[3]) %in% OK || as.numeric(err[3]) > 1000) {
    if(verbose > 1) {
      warning(paste(.twsERR[err[3],]))
      return(TRUE)
    } else return(TRUE)
  } else {
    if(verbose > 0) warning(paste(.twsERR[err[3],]))
    return(FALSE)
  }
}
