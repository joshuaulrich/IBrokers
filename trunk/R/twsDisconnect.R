twsDisconnect <- function(con) {
  if(!is.twsConnection(con))
    stop("not a 'tws' connection")
  close(con[[1]])
}

close.twsConnection <- function(con, ...)
{
  close(con[[1]])
} 

close.twsconn <- function(con, ...)
{
  close(con$conn)
} 
