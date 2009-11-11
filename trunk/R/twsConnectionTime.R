`twsConnectionTime` <-
function(con) {
  if(!is.twsConnection(con))
    stop('con must be a twsConnection object')

  return(con$connected.at)
}
