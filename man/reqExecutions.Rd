\name{reqExecutions}
\alias{reqExecutions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqExecutions(twsconn, reqId = "0", ExecutionFilter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsconn}{
%%     ~~Describe \code{twsconn} here~~
}
  \item{reqId}{
%%     ~~Describe \code{reqId} here~~
}
  \item{ExecutionFilter}{
%%     ~~Describe \code{ExecutionFilter} here~~
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
function (twsconn, reqId = "0", ExecutionFilter) 
{
    if (!is.twsConnection(twsconn)) 
        stop("invalid 'twsConnection' object")
    con <- twsconn[[1]]
    VERSION <- "3"
    outgoing <- c(.twsOutgoingMSG$REQ_EXECUTIONS, VERSION, as.character(reqId), 
        ExecutionFilter$clientId, ExecutionFilter$acctCode, ExecutionFilter$time, 
        ExecutionFilter$symbol, ExecutionFilter$secType, ExecutionFilter$exchange, 
        ExecutionFilter$side)
    writeBin(outgoing, con)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
