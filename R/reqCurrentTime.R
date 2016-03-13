reqCurrentTime <-
function (twsconn) 
{
    .reqCurrentTime(twsconn)
    con <- twsconn[[1]]
    e_current_time <- eWrapper()
    e_current_time$currentTime <- function(curMsg, msg, timestamp, 
        file, ...) {
        msg[2]
    }
    while (isConnected(twsconn)) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1)
        currentTime <- processMsg(curMsg, con, eWrapper = e_current_time, 
            twsconn = twsconn, timestamp = NULL, file = "")
        if (curMsg == .twsIncomingMSG$CURRENT_TIME) 
            break
    }
    structure(as.numeric(currentTime), class = c("POSIXt", "POSIXct"))
}
