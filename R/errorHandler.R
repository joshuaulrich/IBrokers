`errorHandler` <-
function(con, verbose, OK=NULL) {
  err <- readBin(con,character(),4)

  if(as.numeric(err[3]) %in% OK || as.numeric(err[3]) > 1000) {
    if(as.numeric(err[3]) == 1100) {

      #close(con)  # instead of closing connection, we'll 
      warning(err[4])
    }
    if(verbose > 1) {
      warning(err[4])
      return(TRUE)
    } else return(TRUE)
  } else {
    if(verbose > 0) #warning(paste(.twsERR[err[3],]))
      warning(err[4])
    return(FALSE)
  }
}
