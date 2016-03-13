calculateImpliedVolatility <-
function (twsconn, Contract, optionPrice, underPrice, reqId = 1) 
{
    .calculateImpliedVolatility(twsconn, Contract, optionPrice, 
        underPrice, reqId)
    eW <- eWrapper(NULL)
    eW$tickOptionComputation <- function(msg, string, ...) {
        as.numeric(string[4])
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
