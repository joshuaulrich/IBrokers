cancelAccountUpdates <- function(conn, acctCode="1")
{
  if(!is.twsConnection(conn))
    stop("requires twsConnection object")
  .reqAccountUpdates(conn, "0", acctCode)
}

.reqAccountUpdates <- function(conn, subscribe=TRUE, acctCode="1")
{
  if (!is.twsConnection(conn))
      stop("requires twsConnection object")

  con <- conn[[1]]
  VERSION <- "2"

  # send messages to TWS
  writeBin(.twsOutgoingMSG$REQ_ACCOUNT_DATA, con)
  writeBin(VERSION, con)
  writeBin(as.character(as.numeric(subscribe)), con) 
  writeBin(as.character(acctCode), con)
}

reqAccountUpdates <- 
function (conn,
          subscribe=TRUE,
          acctCode="1",
          eventWrapper=eWrapper(),
          CALLBACK=twsCALLBACK, ...)
{
  if (!is.twsConnection(conn))
      stop("requires twsConnection object")

  .reqAccountUpdates(conn, subscribe, acctCode)

  on.exit(.reqAccountUpdates(conn, "0", acctCode))

  CALLBACK(conn, eventWrapper, NULL, file)
}
  

