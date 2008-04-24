`serverVersion` <-
function(con) {
  if(!inherits(con,'twsConnection'))
    stop('con must be a twsConnection object')

  con$server.version
}
