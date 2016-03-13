reqScannerParameters <-
function (twsconn) 
{
    .reqScannerParameters(twsconn)
    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        msg <- processMsg(curMsg, con, eW)
        if (curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) 
            return(invisible(msg[2]))
    }
}
