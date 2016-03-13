\name{reqMktDepth}
\alias{reqMktDepth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqMktDepth(conn, Contract, tickerId = "1", numRows = "20", timeStamp = TRUE, playback = 1, file = "", verbose = TRUE, eventWrapper = eWrapper(), CALLBACK = twsCALLBACK, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{Contract}{
%%     ~~Describe \code{Contract} here~~
}
  \item{tickerId}{
%%     ~~Describe \code{tickerId} here~~
}
  \item{numRows}{
%%     ~~Describe \code{numRows} here~~
}
  \item{timeStamp}{
%%     ~~Describe \code{timeStamp} here~~
}
  \item{playback}{
%%     ~~Describe \code{playback} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{eventWrapper}{
%%     ~~Describe \code{eventWrapper} here~~
}
  \item{CALLBACK}{
%%     ~~Describe \code{CALLBACK} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (conn, Contract, tickerId = "1", numRows = "20", timeStamp = TRUE, 
    playback = 1, file = "", verbose = TRUE, eventWrapper = eWrapper(), 
    CALLBACK = twsCALLBACK, ...) 
{
    if (!is.twsConnection(conn)) 
        stop("tws connection object required")
    if (!is.twsPlayback(conn)) {
        if (class(Contract) == "twsContract") 
            Contract <- list(Contract)
        for (n in 1:length(Contract)) {
            if (class(Contract[[n]]) != "twsContract") 
                stop("twsContract required")
        }
    }
    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")
    cancelMktDepth <- function(con, tickerId) {
        if (inherits(con, "sockconn")) {
            for (i in 1:length(tickerId)) {
                writeBin(.twsOutgoingMSG$CANCEL_MKT_DEPTH, con)
                writeBin("1", con)
                writeBin(tickerId[i], con)
            }
        }
        else {
            seek(con, 0)
        }
    }
    if (!is.character(timeStamp) & timeStamp) {
        timeStamp <- "\%Y\%m\%d \%H:\%M:\%OS"
    }
    else {
        timeStamp <- NULL
    }
    if (is.null(CALLBACK)) 
        CALLBACK <- twsDEBUG
    VERSION <- "3"
    ticker_id <- as.character(tickerId)
    if (inherits(con, "sockconn")) {
        for (n in 1:length(Contract)) {
            signals <- c(.twsOutgoingMSG$REQ_MKT_DEPTH, VERSION, 
                ticker_id, Contract[[n]]$symbol, Contract[[n]]$sectype, 
                Contract[[n]]$expiry, Contract[[n]]$strike, Contract[[n]]$right, 
                Contract[[n]]$multiplier, Contract[[n]]$exch, 
                Contract[[n]]$currency, Contract[[n]]$local, 
                numRows)
            for (i in 1:length(signals)) {
                writeBin(signals[i], con)
            }
            ticker_id <- as.character(as.numeric(tickerId) + 
                n)
        }
        msg_expected_length <- NA
    }
    else {
        msg_expected_length <- as.numeric(readBin(con, character(), 
            1))
    }
    if (!missing(CALLBACK) && is.na(list(CALLBACK))) {
        if (inherits(conn, "twsPlayback")) {
            seek(conn[[1]], 0)
            stop("CALLBACK=NA is not available for playback")
        }
        return(as.character(as.numeric(tickerId):length(Contract)))
    }
    on.exit(cancelMktDepth(con, as.character(as.numeric(tickerId):length(Contract))))
    CALLBACK(conn, eWrapper = eventWrapper, timestamp = timeStamp, 
        file = file, playback = playback, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
