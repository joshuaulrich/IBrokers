isConnected <-
function (twsconn) 
{
    is_open <- function(con) {
        if (inherits(try(isOpen(con), silent = TRUE), "try-error")) {
            FALSE
        }
        else TRUE
    }
    if (!is.twsConnection(twsconn)) {
        warning("isConnected requires a twsconn object")
        return(FALSE)
    }
    if (!is.null(twsconn$connected)) {
        return(is_open(twsconn[[1]]) && twsconn$connected)
    }
    else {
        is_open(twsconn[[1]])
    }
}
