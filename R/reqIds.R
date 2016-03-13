reqIds <-
function (conn, numIds = 1) 
{
    if (inherits(conn, "twsconn")) {
        id <- conn$nextValidId
        conn$nextValidId <- as.character(as.numeric(id) + 1L)
        return(id)
    }
    .reqIds(conn, numIds)
    con <- conn[[1]]
    e_next_id <- eWrapper()
    e_next_id$nextValidId <- function(curMsg, msg, timestamp, 
        file, ...) {
        msg[2]
    }
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1)
        nextValidID <- processMsg(curMsg, con, eWrapper = e_next_id, 
            timestamp = NULL, file = "")
        if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) 
            break
    }
    return(nextValidID)
}
