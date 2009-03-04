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

  e_next_id <- eWrapper()
  e_next_id$nextValidId <- function(msg, timestamp, file, ...) { msg[2] }


  while(TRUE) {
    curMsg <- readBin(con, character(), 1)
    if(length(curMsg) < 1)
      next
    nextValidID <- processMsg(curMsg,
                              con,
                              eWrapper=e_next_id,
                              timestamp=NULL,file="")
    if(curMsg == .twsIncomingMSG$NEXT_VALID_ID)
      break
  }

  return(nextValidID)
#  while (waiting) {
#    curChar <- readBin(con, character(), 1)
#    if (curChar == .twsIncomingMSG$NEXT_VALID_ID) {
#        nextId <- readBin(con, character(), 2)[2]
#        waiting <- FALSE
#    }
#  }
#  return(nextId)
}

