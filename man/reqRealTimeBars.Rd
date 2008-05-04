\name{reqRealTimeBars}
\alias{reqRealTimeBars}
\title{ Request Real Time Bars from TWS }
\description{
Allows for streaming real-time bars to be handled in \R
}
\usage{
reqRealTimeBars(conn,
                Contract,
                tickerId = "1",
                whatToShow = "TRADES", 
                barSize = "5",
                useRTH = "1",
                callback,
                file,
                verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{ ~~Describe \code{conn} here~~ }
  \item{Contract}{ ~~Describe \code{Contract} here~~ }
  \item{tickerId}{ ~~Describe \code{tickerId} here~~ }
  \item{whatToShow}{ ~~Describe \code{whatToShow} here~~ }
  \item{barSize}{ ~~Describe \code{barSize} here~~ }
  \item{useRTH}{ ~~Describe \code{useRTH} here~~ }
  \item{callback}{ ~~Describe \code{callback} here~~ }
  \item{file}{ ~~Describe \code{file} here~~ }
  \item{verbose}{ ~~Describe \code{verbose} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (conn, Contract, tickerId = "1", whatToShow = "TRADES", 
    barSize = "5", useRTH = "1", callback, file, verbose = TRUE) 
{
    if (class(conn) != "twsConnection") 
        stop("tws connection object required")
    if (class(Contract) != "twsContract") 
        stop("twsContract required")
    con <- conn[[1]]
    if (!isOpen(con)) 
        stop("connection to TWS has been closed")
    cancelRealTimeBars <- function(con, tickerId) {
        writeBin(.twsOutgoingMSG$CANCEL_REAL_TIME_BARS, con)
        writeBin("1", con)
        writeBin(tickerId, con)
    }
    on.exit(cancelRealTimeBars(con, as.character(tickerId)))
    VERSION <- "1"
    signals <- c(.twsOutgoingMSG$REQ_REAL_TIME_BARS, VERSION, 
        as.character(tickerId), Contract$symbol, Contract$sectype, 
        Contract$expiry, Contract$strike, Contract$right, Contract$multiplier, 
        Contract$exch, Contract$primary, Contract$currency, Contract$local, 
        barSize, whatToShow, useRTH)
    for (i in 1:length(signals)) {
        writeBin(signals[i], con)
    }
    waiting <- TRUE
    response <- character(0)
    if (.Platform$OS == "windows") 
        Sys.sleep(0.1)
    while (waiting) {
        curMsg <- suppressWarnings(readBin(con, character(), 
            1))
        if (length(curMsg) > 0) {
            if (curMsg == .twsIncomingMSG$ERR_MSG) {
                if (!errorHandler(con, verbose, OK = c(165, 300, 
                  2104, 2106, 2107))) {
                  cat("\n")
                  stop("Unable to complete market data request")
                }
            }
            if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
                msg <- readBin(con, character(), 10)
                columns <- c("Id", "time", "open", "high", "low", 
                  "close", "volume", "wap", "count")
                cat(paste(columns, "=", msg[-1], sep = ""), "\n")
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
