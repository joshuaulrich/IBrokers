\name{reqAccountUpdates}
\alias{reqAccountUpdates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqAccountUpdates(conn, subscribe = TRUE, acctCode = "1", eventWrapper = eWrapper(), CALLBACK = twsCALLBACK, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{subscribe}{
%%     ~~Describe \code{subscribe} here~~
}
  \item{acctCode}{
%%     ~~Describe \code{acctCode} here~~
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
function (conn, subscribe = TRUE, acctCode = "1", eventWrapper = eWrapper(), 
    CALLBACK = twsCALLBACK, ...) 
{
    if (!is.twsConnection(conn)) 
        stop("requires twsConnection object")
    .reqAccountUpdates(conn, subscribe, acctCode)
    on.exit(.reqAccountUpdates(conn, "0", acctCode))
    verbose <- FALSE
    acct <- list()
    con <- conn[[1]]
    eW <- eWrapper(NULL)
    eW$assign.Data("data", structure(list(), class = "eventAccountValue"))
    eW$updatePortfolio <- function(curMsg, msg, ...) {
        version <- as.numeric(msg[1])
        contract <- twsContract(conId = msg[2], symbol = msg[3], 
            sectype = msg[4], exch = msg[9], primary = msg[9], 
            expiry = msg[5], strike = msg[6], currency = msg[10], 
            right = msg[7], local = msg[11], multiplier = msg[8], 
            combo_legs_desc = "", comboleg = "", include_expired = "")
        portfolioValue <- list()
        portfolioValue$position <- as.numeric(msg[12])
        portfolioValue$marketPrice <- as.numeric(msg[13])
        portfolioValue$marketValue <- as.numeric(msg[14])
        portfolioValue$averageCost <- as.numeric(msg[15])
        portfolioValue$unrealizedPNL <- as.numeric(msg[16])
        portfolioValue$realizedPNL <- as.numeric(msg[17])
        portfolioValue$accountName <- msg[18]
        p <- structure(list(contract = contract, portfolioValue = portfolioValue), 
            class = "eventPortfolioValue")
        p
    }
    eW$updateAccountValue <- function(curMsg, msg, ...) {
        data <- eW$get.Data("data")
        data[[msg[2]]] <- c(value = msg[3], currency = msg[4])
        eW$assign.Data("data", data)
    }
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1)
        if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
            acct[[length(acct) + 1]] <- processMsg(curMsg, con, 
                eW, timestamp, file)
        }
        else {
            processMsg(curMsg, con, eW, timestamp, file)
        }
        if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) 
            break
    }
    return(structure(list(eW$get.Data("data"), acct), class = "AccountUpdate"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
