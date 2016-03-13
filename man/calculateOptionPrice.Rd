\name{calculateOptionPrice}
\alias{calculateOptionPrice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calculateOptionPrice(twsconn, Contract, volatility, underPrice, reqId = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsconn}{
%%     ~~Describe \code{twsconn} here~~
}
  \item{Contract}{
%%     ~~Describe \code{Contract} here~~
}
  \item{volatility}{
%%     ~~Describe \code{volatility} here~~
}
  \item{underPrice}{
%%     ~~Describe \code{underPrice} here~~
}
  \item{reqId}{
%%     ~~Describe \code{reqId} here~~
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
function (twsconn, Contract, volatility, underPrice, reqId = 1) 
{
    .calculateOptionPrice(twsconn, Contract, volatility, underPrice, 
        reqId)
    eW <- eWrapper(NULL)
    eW$tickOptionComputation <- function(curMsg, msg, ...) {
        as.numeric(msg[6])
    }
    con <- twsconn[[1]]
    while (TRUE) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, "character", 1)
        msg <- processMsg(curMsg, con, eW, NULL, "")
        if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
            return(msg)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
