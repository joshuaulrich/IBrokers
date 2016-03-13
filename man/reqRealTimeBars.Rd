\name{reqRealTimeBars}
\alias{reqRealTimeBars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqRealTimeBars(conn, Contract, whatToShow = "TRADES", barSize = "5", useRTH = TRUE, playback = 1, tickerId = "1", file = "", verbose = TRUE, eventWrapper = eWrapper(), CALLBACK = twsCALLBACK, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{Contract}{
%%     ~~Describe \code{Contract} here~~
}
  \item{whatToShow}{
%%     ~~Describe \code{whatToShow} here~~
}
  \item{barSize}{
%%     ~~Describe \code{barSize} here~~
}
  \item{useRTH}{
%%     ~~Describe \code{useRTH} here~~
}
  \item{playback}{
%%     ~~Describe \code{playback} here~~
}
  \item{tickerId}{
%%     ~~Describe \code{tickerId} here~~
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
function (conn, Contract, whatToShow = "TRADES", barSize = "5", 
    useRTH = TRUE, playback = 1, tickerId = "1", file = "", verbose = TRUE, 
    eventWrapper = eWrapper(), CALLBACK = twsCALLBACK, ...) 
{
    if (!is.twsPlayback(conn)) {
        tickerId <- .reqRealTimeBars(conn, Contract, whatToShow, 
            barSize, useRTH, tickerId)
    }
    if (is.twsContract(Contract)) 
        Contract <- list(Contract)
    con <- conn[[1]]
    cancelRealTimeBars <- function(con, tickerId) {
        if (inherits(con, "sockconn")) {
            for (i in 1:length(tickerId)) {
                writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS, 
                  con)
                writeBin("1", con)
                writeBin(tickerId[i], con)
            }
        }
        else {
            seek(con, 0)
        }
    }
    if (is.null(CALLBACK)) 
        CALLBACK <- twsDEBUG
    if (!missing(CALLBACK) && is.na(list(CALLBACK))) {
        if (is.twsPlayback(conn)) {
            seek(conn[[1]], 0)
            stop("CALLBACK=NA is not available for playback")
        }
        return(tickerId)
    }
    on.exit(cancelRealTimeBars(con, tickerId))
    symbol.or.local <- function(x) {
        symbol <- x$symbol
        local <- x$local
        if (local == "") {
            return(symbol)
        }
        else return(local)
    }
    eventWrapper$assign.Data("symbols", sapply(Contract, symbol.or.local))
    timeStamp <- NULL
    if (!is.list(file)) 
        file <- list(file)
    if (length(file) != length(Contract)) 
        file <- rep(file, length(Contract))
    CALLBACK(conn, eWrapper = eventWrapper, timestamp = timeStamp, 
        file = file, playback = playback, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
