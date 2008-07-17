`reqIds` <-
function (conn, numIds=1) 
{
  if (!inherits(conn, "twsConnection")) 
    stop("requires twsConnection object")
  con <- conn[[1]]

  VERSION <- "1"
  writeBin(.twsOutgoingMSG$REQ_IDS, con)
  writeBin(VERSION, con)
  writeBin(as.character(numIds), con)

  waiting <- TRUE

  while (waiting) {
    curChar <- readBin(con, character(), 1)
    if (curChar == .twsIncomingMSG$NEXT_VALID_ID) {
        nextId <- readBin(con, character(), 2)[2]
        waiting <- FALSE
    }
  }
  return(nextId)
}

