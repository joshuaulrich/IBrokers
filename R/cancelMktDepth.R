cancelMktDepth <-
function (conn, tickerId) 
{
    if (!inherits(conn, "twsConnection")) 
        stop("twsConnection object required")
    con <- conn[[1]]
    for (i in 1:length(tickerId)) {
        writeBin(.twsOutgoingMSG$CANCEL_MKT_DEPTH, con)
        writeBin("1", con)
        writeBin(tickerId[i], con)
    }
}
