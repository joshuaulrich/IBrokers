`twsDisconnect` <-
function(con) {
  if(!inherits(con,'twsConnection')) stop("not a tws connection")
  close(con[[1]])
}

close.twsConnection <- function(con, ...)
{
  close(con[[1]])
} 
