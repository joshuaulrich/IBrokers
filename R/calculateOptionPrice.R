calculateOptionPrice <-
function (twsconn, Contract, volatility, underPrice, reqId = 1) 
{
    .calculateOptionPrice(twsconn, Contract, volatility, underPrice, 
        reqId)
    eW <- eWrapper(NULL)
    eW$tickOptionComputation <- function(curMsg, msg, ...) {
        as.numeric(msg[6])
    }
    con <- twsconn[[1]]
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, "character", 1)
        msg <- processMsg(curMsg, con, eW, NULL, "")
        if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
            return(msg)
        }
    }
}
