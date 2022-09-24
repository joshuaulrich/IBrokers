.reqManagedAccts <-
    function(conn) {
        if(!isConnected(conn))
            stop('peer has gone away. check your IB connection',call.=FALSE)
        if(!is.twsConnection(conn))
            stop('requires twsConnection object')

        con <- conn[[1]]

        VERSION <- "1"
        outgoing <- c(.twsOutgoingMSG$REQ_MANAGED_ACCTS, VERSION)

        writeBin(outgoing, con)
}

reqManagedAccts <- function(twsconn) {
  .reqManagedAccts(twsconn)
  con <- twsconn[[1]]
  e_managed_acc <- eWrapper()
  while (isConnected(twsconn)) {
      socketSelect(list(con), FALSE, NULL)
      curMsg <- readBin(con, character(), 1)
      accounts_raw <- processMsg(curMsg,
                                con,
                                eWrapper=e_managed_acc,
                                twsconn=twsconn,
                                timestamp=NULL, file="")
      if(curMsg == .twsIncomingMSG$MANAGED_ACCTS)
          break
  }
  # Clean up ending "," when multiple accounts are reported.
  accounts <- if(endsWith(accounts_raw[3], ",")) substring(accounts_raw[3],1, nchar(accounts_raw[3]) - 1) else accounts_raw[3]

  return(accounts)
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
