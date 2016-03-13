replaceFA <-
function (twsconn, faDataType, xml) 
{
    if (!is.twsConnection(twsconn)) 
        stop("invalid twsConnection")
    VERSION <- "1"
    writeBin(c(.twsOutgoingMSG$REPLACE_FA, VERSION, as.character(faDataType), 
        as.character(xml)), twsconn[[1]])
}
