\name{reqMktData}
\alias{reqMktData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqMktData(conn, Contract, tickGenerics = "100,101,104,106,165,221,225,236", snapshot = FALSE, tickerId = "1", timeStamp = "\%Y\%m\%d \%H:\%M:\%OS", playback = 1, file = "", verbose = TRUE, eventWrapper = eWrapper(), CALLBACK = twsCALLBACK, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{Contract}{
%%     ~~Describe \code{Contract} here~~
}
  \item{tickGenerics}{
%%     ~~Describe \code{tickGenerics} here~~
}
  \item{snapshot}{
%%     ~~Describe \code{snapshot} here~~
}
  \item{tickerId}{
%%     ~~Describe \code{tickerId} here~~
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
function (conn, Contract, tickGenerics = "100,101,104,106,165,221,225,236", 
    snapshot = FALSE, tickerId = "1", timeStamp = "\%Y\%m\%d \%H:\%M:\%OS", 
    playback = 1, file = "", verbose = TRUE, eventWrapper = eWrapper(), 
    CALLBACK = twsCALLBACK, ...) 
{
    if (!is.twsConnection(conn)) 
        stop("tws connection object required")
    if (!is.twsPlayback(conn)) {
        Contract <- as.twsContract(Contract)
        if (is.twsContract(Contract)) 
            Contract <- list(Contract)
        for (n in 1:length(Contract)) {
            if (!is.twsContract(Contract[[n]])) 
                stop("twsContract required")
        }
    }
    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")
    cancelMktData <- function(con, tickerId) {
        if (inherits(con, "sockconn")) {
            for (i in 1:length(tickerId)) {
                writeBin(.twsOutgoingMSG$CANCEL_MKT_DATA, con)
                writeBin("2", con)
                writeBin(tickerId[i], con)
            }
        }
        else {
            seek(con, 0)
        }
    }
    if (is.null(CALLBACK)) 
        CALLBACK <- twsDEBUG
    snapshot <- ifelse(snapshot, "1", "0")
    if (snapshot == "1" && missing(tickGenerics)) 
        tickGenerics <- ""
    VERSION <- "11"
    fullSnapshot <- data.frame()
    symbols. <- NULL
    ticker_id <- as.character(tickerId)
    symbol.or.local <- function(x) {
        symbol <- x$symbol
        local <- x$local
        if (local == "") {
            return(symbol)
        }
        else return(local)
    }
    if (inherits(con, "sockconn")) {
        for (n in 1:length(Contract)) {
            if (Contract[[n]]$sectype == "BAG") 
                stop("BAG contract type in reqMktData not implemented")
            signals <- c(.twsOutgoingMSG$REQ_MKT_DATA, VERSION, 
                ticker_id, Contract[[n]]$conId, Contract[[n]]$symbol, 
                Contract[[n]]$sectype, Contract[[n]]$expiry, 
                Contract[[n]]$strike, Contract[[n]]$right, Contract[[n]]$multiplier, 
                Contract[[n]]$exch, Contract[[n]]$primary, Contract[[n]]$currency, 
                Contract[[n]]$local, {
                  if (is.null(Contract[[n]]$tradingClass)) "" else Contract[[n]]$tradingClass
                }, "0", tickGenerics, snapshot, "")
            writeBin(signals, con)
            if (snapshot == "1") {
                stop("Snapshot not working ? so not supported")
                eventWrapper <- eWrapper.snapshot()
                while (1) {
                  socketSelect(list(con), FALSE, NULL)
                  curMsg <- readBin(con, character(), 1)
                  processMsg(curMsg, con, eventWrapper, NULL, 
                    file, ...)
                  if (curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
                    fullSnapshot <- rbind(fullSnapshot, data.frame(lastTimeStamp = eventWrapper$get.Data("lastTimeStamp"), 
                      symbol = symbol.or.local(Contract[[n]]), 
                      bidSize = eventWrapper$get.Data("bidSize"), 
                      bidPrice = eventWrapper$get.Data("bidPrice"), 
                      askPrice = eventWrapper$get.Data("askPrice"), 
                      askSize = eventWrapper$get.Data("askSize"), 
                      lastPrice = eventWrapper$get.Data("lastPrice"), 
                      Volume = eventWrapper$get.Data("Volume"), 
                      Open = eventWrapper$get.Data("Open"), High = eventWrapper$get.Data("High"), 
                      Low = eventWrapper$get.Data("Low"), Close = eventWrapper$get.Data("Close")))
                    break
                  }
                }
                if (n == length(Contract)) 
                  return(fullSnapshot)
            }
            ticker_id <- as.character(as.numeric(tickerId) + 
                n)
            symbols. <- c(symbols., symbol.or.local(Contract[[n]]))
        }
    }
    eventWrapper$assign.Data("symbols", symbols.)
    if (!missing(CALLBACK) && is.na(list(CALLBACK))) {
        if (is.twsPlayback(conn)) {
            seek(conn[[1]], 0)
            stop("CALLBACK=NA is not available for playback")
        }
        return(as.character(as.numeric(tickerId):length(Contract)))
    }
    if (snapshot == "0") 
        on.exit(cancelMktData(con, as.character(as.numeric(tickerId):length(Contract))))
    if (!is.list(file)) 
        file <- list(file)
    if (length(file) != length(Contract)) 
        file <- rep(file, length(Contract))
    CALLBACK(conn, eWrapper = eventWrapper, timestamp = timeStamp, 
        file = file, playback = playback, timeout = NULL, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
