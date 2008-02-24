`twsDisconnect` <-
function(con) {
  if(class(con)!='twsConnection') stop("not a tws connection")
  close(con[[1]])
}

