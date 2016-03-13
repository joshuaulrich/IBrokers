\name{reqScannerSubscription}
\alias{reqScannerSubscription}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqScannerSubscription(twsconn, subscription, tickerId = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsconn}{
%%     ~~Describe \code{twsconn} here~~
}
  \item{subscription}{
%%     ~~Describe \code{subscription} here~~
}
  \item{tickerId}{
%%     ~~Describe \code{tickerId} here~~
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
function (twsconn, subscription, tickerId = 1) 
{
    scanner <- data.frame(reqId = NULL, rank = NULL, symbol = NULL, 
        expiry = NULL, strike = NULL, right = NULL, exch = NULL, 
        currency = NULL, local = NULL, marketName = NULL, tradingClass = NULL, 
        distance = NULL, benchmark = NULL, projection = NULL, 
        legsStr = NULL)
    .reqScannerSubscription(twsconn, subscription, tickerId)
    on.exit(cancelScannerSubscription(twsconn, tickerId))
    con <- twsconn[[1]]
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1L)
        if (curMsg == .twsIncomingMSG$SCANNER_DATA) {
            cD <- twsContractDetails()
            version <- readBin(con, character(), 1L)
            reqId <- readBin(con, character(), 1L)
            numberOfElements <- as.integer(readBin(con, character(), 
                1L))
            for (i in 1:numberOfElements) {
                msg <- readBin(con, character(), 16L)
                rank <- msg[1]
                cD$contract$conId <- msg[2]
                cD$contract$symbol <- msg[3]
                cD$contract$sectype <- msg[4]
                cD$contract$expiry <- msg[5]
                cD$contract$strike <- msg[6]
                cD$contract$right <- msg[7]
                cD$contract$exch <- msg[8]
                cD$contract$currency <- msg[9]
                cD$contract$local <- msg[10]
                cD$marketName <- msg[11]
                cD$tradingClass <- msg[12]
                distance <- msg[13]
                benchmark <- msg[14]
                projection <- msg[15]
                legsStr <- msg[16]
                ctr <- cD$contract
                scanner <- rbind(scanner, data.frame(reqId, rank, 
                  symbol = ctr$symbol, expiry = ctr$expiry, strike = ctr$strike, 
                  right = ctr$right, exch = ctr$exch, currency = ctr$currency, 
                  local = ctr$local, marketName = cD$marketName, 
                  tradingClass = cD$tradingClass, distance, benchmark, 
                  projection, legsStr, stringsAsFactors = FALSE))
            }
            return(scanner)
        }
        else processMsg(curMsg, con, eWrapper(NULL))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
