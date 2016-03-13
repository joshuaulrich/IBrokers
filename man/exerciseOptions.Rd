\name{exerciseOptions}
\alias{exerciseOptions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
exerciseOptions(twsconn, contract, exerciseAction = 1, exerciseQuantity = 1, account = "", override = 0, tickerId = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsconn}{
%%     ~~Describe \code{twsconn} here~~
}
  \item{contract}{
%%     ~~Describe \code{contract} here~~
}
  \item{exerciseAction}{
%%     ~~Describe \code{exerciseAction} here~~
}
  \item{exerciseQuantity}{
%%     ~~Describe \code{exerciseQuantity} here~~
}
  \item{account}{
%%     ~~Describe \code{account} here~~
}
  \item{override}{
%%     ~~Describe \code{override} here~~
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
function (twsconn, contract, exerciseAction = 1, exerciseQuantity = 1, 
    account = "", override = 0, tickerId = 1) 
{
    if (!is.twsConnection(twsconn)) 
        stop("invalid twsconn")
    if (!is.twsContract(contract)) 
        stop("invalid twsContract")
    if (!as.numeric(exerciseAction) \%in\% 1:2) 
        stop("valid exerciseAction is 1 or 2")
    if (!as.numeric(override) \%in\% 0:1) 
        stop("valid overrride is 0 or 1")
    VERSION <- "1"
    msg <- c(.twsOutgoingMSG$EXERCISE_OPTION, VERSION, tickerId, 
        contract$symbol, contract$sectype, contract$expiry, contract$strike, 
        contract$multiplier, contract$exch, contract$currency, 
        contract$local, exerciseAction, exerciseQuantity, account, 
        override)
    writeBin(as.character(msg), twsconn[[1]])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
