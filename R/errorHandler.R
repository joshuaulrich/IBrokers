`errorHandler` <-
function(con, verbose) {
  err <- readBin(con,character(),4)
  if(verbose) {
    cat(err[4],'\n')
  }
  if(as.numeric(err[3]) %in% c(165,2106)) {
    return(TRUE)
  } else return(FALSE)
}
