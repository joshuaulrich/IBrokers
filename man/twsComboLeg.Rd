\name{twsComboLeg}
\alias{twsComboLeg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twsComboLeg(conId = 0, ratio = 0, action = c("BUY", "SELL", "SSHORT"), exchange = NULL, openClose = 0, shortSaleSlot = 0, designatedLocation = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conId}{
%%     ~~Describe \code{conId} here~~
}
  \item{ratio}{
%%     ~~Describe \code{ratio} here~~
}
  \item{action}{
%%     ~~Describe \code{action} here~~
}
  \item{exchange}{
%%     ~~Describe \code{exchange} here~~
}
  \item{openClose}{
%%     ~~Describe \code{openClose} here~~
}
  \item{shortSaleSlot}{
%%     ~~Describe \code{shortSaleSlot} here~~
}
  \item{designatedLocation}{
%%     ~~Describe \code{designatedLocation} here~~
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
function (conId = 0, ratio = 0, action = c("BUY", "SELL", "SSHORT"), 
    exchange = NULL, openClose = 0, shortSaleSlot = 0, designatedLocation = "") 
{
    structure(list(conId = conId, ratio = ratio, action = action[1], 
        exchange = exchange, openClose = openClose, shortSaleSlot = shortSaleSlot, 
        designatedLocation = designatedLocation, SAME = 0, OPEN = 1, 
        CLOSE = 2, UNKNOWN = 3), class = "twsComboLeg")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
