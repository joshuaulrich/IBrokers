reqOpenOrders <-
function (twsconn) 
{
    .reqOpenOrders(twsconn)
    con <- twsconn[[1]]
    eW <- eWrapper()
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        processMsg(curMsg, con, eW)
    }
}
