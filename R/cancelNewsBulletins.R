cancelNewsBulletins <-
function (twsconn) 
{
    if (!is.twsConnection(twsconn)) 
        stop("requires twsConnection object")
    VERSION <- "1"
    writeBin(c(.twsOutgoingMSG$CANCEL_NEWS_BULLETINS, VERSION), 
        twsconn[[1]])
}
