cancelHistoricalData <-
function (conn, tickerId) 
{
    if (!inherits(conn, "twsConnection")) 
        if (!is.twsConnection(conn)) 
            stop("twsConnection object required")
    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("invalid TWS connection")
    writeBin(.twsOutgoingMSG$CANCEL_HISTORICAL_DATA, con)
    writeBin("1", con)
    writeBin(as.character(tickerId), con)
}
