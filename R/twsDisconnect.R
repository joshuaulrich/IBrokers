twsDisconnect <-
function (twsconn) 
{
    if (!is.twsConnection(twsconn)) 
        stop("not a 'tws' connection")
    close(twsconn)
}
