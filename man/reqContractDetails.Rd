\name{reqContractDetails}
\alias{reqContractDetails}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqContractDetails(conn, Contract, reqId = "1", verbose = FALSE, eventWrapper = eWrapper(), CALLBACK = twsCALLBACK, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{Contract}{
%%     ~~Describe \code{Contract} here~~
}
  \item{reqId}{
%%     ~~Describe \code{reqId} here~~
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
function (conn, Contract, reqId = "1", verbose = FALSE, eventWrapper = eWrapper(), 
    CALLBACK = twsCALLBACK, ...) 
{
    .reqContractDetails(conn, Contract, reqId)
    if (is.null(CALLBACK)) 
        invisible(return(NULL))
    eW <- eWrapper(NULL)
    eW$contractDetails <- function(curMsg, msg, timestamp, file, 
        ...) {
        twsContractDetails(version = msg[1], contract = twsContract(conId = msg[12 + 
            1], symbol = msg[3], sectype = msg[4], expiry = msg[5], 
            primary = msg[21], strike = msg[5 + 1], right = msg[6 + 
                1], exch = msg[7 + 1], currency = msg[8 + 1], 
            multiplier = msg[14 + 1], include_expired = Contract$include_expired, 
            combo_legs_desc = "", comboleg = "", local = msg[9 + 
                1]), marketName = msg[10 + 1], tradingClass = msg[11 + 
            1], conId = msg[12 + 1], minTick = msg[13 + 1], orderTypes = unlist(strsplit(msg[15 + 
            1], ",")), validExchanges = unlist(strsplit(msg[16 + 
            1], ",")), priceMagnifier = msg[17 + 1], underConId = msg[18 + 
            1], longName = msg[19 + 1], contractMonth = msg[22], 
            industry = msg[23], category = msg[24], subcategory = msg[25], 
            timeZoneId = msg[26], tradingHours = msg[27], liquidHours = msg[28])
    }
    contracts <- list()
    con <- conn[[1]]
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1)
        if (curMsg != .twsIncomingMSG$CONTRACT_DATA) {
            if (curMsg == .twsIncomingMSG$ERR_MSG) {
                if (!errorHandler(con, verbose, OK = c(165, 300, 
                  366, 2104, 2106, 2107))) {
                  warning("error in contract details")
                  break
                }
            }
            else {
                processMsg(curMsg, con, eW, timestamp, file)
                if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) 
                  break
            }
        }
        if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
            contracts[[length(contracts) + 1]] <- processMsg(curMsg, 
                con, eW, timestamp, file)
        }
    }
    return(contracts)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
