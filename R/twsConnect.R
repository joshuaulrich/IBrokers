twsConnect <-
function (clientId = 1, host = "localhost", port = 7496, verbose = TRUE, 
    timeout = 5, filename = NULL, blocking = .Platform$OS.type == 
        "windows") 
{
    startApi <- function(conn, clientId) {
        if (!is.twsConnection(conn)) 
            stop("requires twsConnection object")
        con <- conn[[1]]
        VERSION <- "1"
        START_API <- "71"
        writeBin(START_API, con)
        writeBin(VERSION, con)
        writeBin(as.character(clientId), con)
    }
    if (is.null(getOption("digits.secs"))) 
        options(digits.secs = 6)
    if (is.character(clientId)) 
        filename <- clientId
    if (is.null(filename)) {
        start.time <- Sys.time()
        s <- socketConnection(host = host, port = port, open = "ab", 
            blocking = blocking)
        on.exit(close(s))
        if (!isOpen(s)) {
            close(s)
            stop(paste("couldn't connect to TWS on port", port))
        }
        CLIENT_VERSION <- "63"
        writeBin(c(CLIENT_VERSION), s)
        eW <- eWrapper(NULL)
        eW$.Data <- environment()
        SERVER_VERSION <- NEXT_VALID_ID <- CONNECTION_TIME <- NULL
        while (TRUE) {
            if (!is.null(CONNECTION_TIME)) 
                break
            if (!socketSelect(list(s), FALSE, 0.1)) 
                next
            curMsg <- readBin(s, character(), 1)
            if (is.null(SERVER_VERSION)) {
                SERVER_VERSION <- curMsg[1]
                CONNECTION_TIME <- readBin(s, character(), 1)
                next
            }
        }
        on.exit()
        twsconn <- new.env()
        twsconn$conn <- s
        twsconn$clientId <- clientId
        twsconn$port <- port
        twsconn$server.version <- SERVER_VERSION
        twsconn$connected.at <- CONNECTION_TIME
        twsconn$connected <- NULL
        class(twsconn) <- c("twsconn", "environment")
        startApi(twsconn, clientId)
        twsconn$nextValidId <- NEXT_VALID_ID <- as.integer(reqIds(twsconn, 
            1))
        assign(".NEXT_VALID_ID", NEXT_VALID_ID, .GlobalEnv)
        return(twsconn)
    }
    else {
        fh <- file(filename, open = "r")
        dat <- scan(fh, what = character(), quiet = TRUE)
        close(fh)
        tmp <- tempfile()
        fh <- file(tmp, open = "ab")
        writeBin(dat, fh)
        close(fh)
        s <- file(tmp, open = "rb")
        twsconn <- new.env()
        twsconn$conn <- s
        twsconn$clientId <- NULL
        twsconn$nextValidId <- NULL
        twsconn$port <- NULL
        twsconn$server.version <- NULL
        twsconn$connected.at <- filename
        class(twsconn) <- c("twsplay", "twsconn", "environment")
        return(twsconn)
    }
}
