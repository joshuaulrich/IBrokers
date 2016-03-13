\name{twsConnect}
\alias{twsConnect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twsConnect(clientId = 1, host = "localhost", port = 7496, verbose = TRUE, timeout = 5, filename = NULL, blocking = .Platform$OS.type == "windows")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clientId}{
%%     ~~Describe \code{clientId} here~~
}
  \item{host}{
%%     ~~Describe \code{host} here~~
}
  \item{port}{
%%     ~~Describe \code{port} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{timeout}{
%%     ~~Describe \code{timeout} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{blocking}{
%%     ~~Describe \code{blocking} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
