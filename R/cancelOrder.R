cancelOrder <-
function (twsconn, orderId) 
{
    if (!is.twsConnection(twsconn)) 
        stop("requires twsConnection object")
    if (missing(orderId)) 
        stop("valid \"orderId\" required")
    con <- twsconn[[1]]
    VERSION <- "1"
    writeBin(.twsOutgoingMSG$CANCEL_ORDER, con)
    writeBin(VERSION, con)
    writeBin(as.character(orderId), con)
}
