`errorHandler` <-
function(con, verbose, OK=NULL) {
  err <- readBin(con,character(),4)

  if(as.numeric(err[3]) %in% OK) {
    if(verbose > 1) {
      message(paste(err[4]))
      return(TRUE)
    } else return(TRUE)
  } else {
    if(verbose > 0) warning(paste(err[4]))
    return(FALSE)
  }
}
