serverVersion <- function(con) {
  if(!is.twsConnection(con))
    stop('con must be a twsConnection object')

  con$server.version
}
