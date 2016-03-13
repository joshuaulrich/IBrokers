cancelMktData <-
function (conn, tickerId) 
{
    if (!is.twsConnection(conn)) 
        stop("twsConnection object required")
    con <- conn[[1]]
    for (i in 1:length(tickerId)) {
        writeBin(.twsOutgoingMSG$CANCEL_MKT_DATA, con)
        writeBin("1", con)
        writeBin(tickerId[i], con)
    }
}
