\name{twsContract}
\alias{twsContract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twsContract(conId = "0", symbol, sectype, exch, primary = "", expiry = "", strike = "0", currency, right, local, multiplier, combo_legs_desc, comboleg, include_expired, secIdType = "", secId = "", tradingClass = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conId}{
%%     ~~Describe \code{conId} here~~
}
  \item{symbol}{
%%     ~~Describe \code{symbol} here~~
}
  \item{sectype}{
%%     ~~Describe \code{sectype} here~~
}
  \item{exch}{
%%     ~~Describe \code{exch} here~~
}
  \item{primary}{
%%     ~~Describe \code{primary} here~~
}
  \item{expiry}{
%%     ~~Describe \code{expiry} here~~
}
  \item{strike}{
%%     ~~Describe \code{strike} here~~
}
  \item{currency}{
%%     ~~Describe \code{currency} here~~
}
  \item{right}{
%%     ~~Describe \code{right} here~~
}
  \item{local}{
%%     ~~Describe \code{local} here~~
}
  \item{multiplier}{
%%     ~~Describe \code{multiplier} here~~
}
  \item{combo_legs_desc}{
%%     ~~Describe \code{combo_legs_desc} here~~
}
  \item{comboleg}{
%%     ~~Describe \code{comboleg} here~~
}
  \item{include_expired}{
%%     ~~Describe \code{include_expired} here~~
}
  \item{secIdType}{
%%     ~~Describe \code{secIdType} here~~
}
  \item{secId}{
%%     ~~Describe \code{secId} here~~
}
  \item{tradingClass}{
%%     ~~Describe \code{tradingClass} here~~
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
function (conId = "0", symbol, sectype, exch, primary = "", expiry = "", 
    strike = "0", currency, right, local, multiplier, combo_legs_desc, 
    comboleg, include_expired, secIdType = "", secId = "", tradingClass = "") 
{
    if (is.null(names(match.call()[-1]))) 
        return(do.call("twsContract", rep(list(""), 14)))
    structure(list(conId = conId, symbol = symbol, sectype = sectype, 
        exch = exch, primary = primary, expiry = expiry, strike = strike, 
        currency = currency, right = right, local = local, multiplier = multiplier, 
        combo_legs_desc = combo_legs_desc, comboleg = comboleg, 
        include_expired = include_expired, secIdType = secIdType, 
        secId = secId, tradingClass = tradingClass), class = "twsContract")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
