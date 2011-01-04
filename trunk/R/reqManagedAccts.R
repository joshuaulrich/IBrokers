reqManagedAccts <- function(twsconn) {
  if( !is.twsConnection(twsconn))
    stop('invalid twsConnection')

  VERSION <- "1"

  writeBin(c(.twsOutgoingMSG$REQ_MANAGED_ACCTS,
             VERSION), 
           twsconn[[1]])
}

requestFA <- function(twsconn, faDataType) {
  if( !is.twsConnection(twsconn))
    stop('invalid twsConnection')

  VERSION <- "1"

  writeBin(c(.twsOutgoingMSG$REQ_FA,
             VERSION, 
             as.character(faDataType)), 
           twsconn[[1]])
}

replaceFA <- function(twsconn, faDataType, xml) {
  if( !is.twsConnection(twsconn))
    stop('invalid twsConnection')

  VERSION <- "1"

  writeBin( c(.twsOutgoingMSG$REPLACE_FA,
              VERSION,
              as.character(faDataType),
              as.character(xml)),
           twsconn[[1]]) 
}
