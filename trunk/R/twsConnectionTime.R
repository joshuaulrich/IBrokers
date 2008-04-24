`twsConnectionTime` <-
function(con) {
  if(!inherits(con,'twsConnection'))
    stop('con must be a twsConnection object')

  return(con$connected.at)
}
