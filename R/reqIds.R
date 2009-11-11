.reqIds <- function(conn, numIds=1)
{
  if(inherits(conn, "twsconn"))
    return(conn$nextValidId)
  if (!is.twsConnection(conn))
    stop("requires twsConnection object")
  con <- conn[[1]]

  VERSION <- "1"
  writeBin(.twsOutgoingMSG$REQ_IDS, con)
  writeBin(VERSION, con)
  writeBin(as.character(numIds), con)
}

reqIds <- function(conn, numIds=1)
{
  if(inherits(conn, "twsconn"))
    return(conn$nextValidId)
  .reqIds(conn, numIds)

  con <- conn[[1]]
  e_next_id <- eWrapper()
  e_next_id$nextValidId <- function(curMsg, msg, timestamp, file, ...) { msg[2] }

  while(TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- readBin(con, character(), 1)

    nextValidID <- processMsg(curMsg,
                              con,
                              eWrapper=e_next_id,
                              timestamp=NULL,file="")
    if(curMsg == .twsIncomingMSG$NEXT_VALID_ID)
      break
  }
  return(nextValidID)
}

