\name{twsOption}
\alias{twsOption}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twsOption(local, expiry = "", strike = "", right = "", exch = "SMART", primary = "", currency = "USD", symbol = "", multiplier = "100", include_expired = "0", conId = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{local}{
%%     ~~Describe \code{local} here~~
}
  \item{expiry}{
%%     ~~Describe \code{expiry} here~~
}
  \item{strike}{
%%     ~~Describe \code{strike} here~~
}
  \item{right}{
%%     ~~Describe \code{right} here~~
}
  \item{exch}{
%%     ~~Describe \code{exch} here~~
}
  \item{primary}{
%%     ~~Describe \code{primary} here~~
}
  \item{currency}{
%%     ~~Describe \code{currency} here~~
}
  \item{symbol}{
%%     ~~Describe \code{symbol} here~~
}
  \item{multiplier}{
%%     ~~Describe \code{multiplier} here~~
}
  \item{include_expired}{
%%     ~~Describe \code{include_expired} here~~
}
  \item{conId}{
%%     ~~Describe \code{conId} here~~
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
function (local, expiry = "", strike = "", right = "", exch = "SMART", 
    primary = "", currency = "USD", symbol = "", multiplier = "100", 
    include_expired = "0", conId = 0) 
{
    twsContract(conId, symbol, "OPT", exch, primary, expiry, 
        strike, currency, right, local, multiplier, NULL, NULL, 
        include_expired)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
