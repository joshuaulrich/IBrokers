requestFA <-
function (twsconn, faDataType) 
{
    if (!is.twsConnection(twsconn)) 
        stop("invalid twsConnection")
    VERSION <- "1"
    writeBin(c(.twsOutgoingMSG$REQ_FA, VERSION, as.character(faDataType)), 
        twsconn[[1]])
}
