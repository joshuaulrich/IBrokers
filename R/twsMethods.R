`print.twsConnection` <-
function(x,...) {
  cat('<twsConnection,',x$clientId,' @ ',
      as.character(x$connected.at),'>\n', sep="")
}

print.twsconn <- function(x,...) {
  cat('<twsConnection,',x$clientId,' @ ',
      as.character(x$connected.at),', nextId=',x$nextValidId,'>\n', sep="")
}

`[[.twsconn` <- function(x, i, ...) {
  if(i==1)
    return(x$conn)
  NULL
}
