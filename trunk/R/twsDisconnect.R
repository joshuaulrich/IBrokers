`twsDisconnect` <-
function(con) {
  if(!inherits(con,'twsConnection')) stop("not a tws connection")
  close(con[[1]])
}

