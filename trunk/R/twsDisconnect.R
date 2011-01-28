twsDisconnect <- function(twsconn) {
  if(!is.twsConnection(twsconn))
    stop("not a 'tws' connection")
  close(twsconn)
}

close.twsConnection <- function(con, ...)
{
  close(con[[1]])
} 

close.twsconn <- function(con, ...)
{
  con$connected <- FALSE
  close(con$conn)
} 
