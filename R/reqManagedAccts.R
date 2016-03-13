reqManagedAccts <-
function (twsconn) 
{
    if (!is.twsConnection(twsconn)) 
        stop("invalid twsConnection")
    VERSION <- "1"
    writeBin(c(.twsOutgoingMSG$REQ_MANAGED_ACCTS, VERSION), twsconn[[1]])
}
