\name{reqHistory}
\alias{reqHistory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqHistory(conn, Contract, barSize = "1 min", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{Contract}{
%%     ~~Describe \code{Contract} here~~
}
  \item{barSize}{
%%     ~~Describe \code{barSize} here~~
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
function (conn, Contract, barSize = "1 min", ...) 
{
    if (barSize == "1 min") {
        endDateTime <- Sys.Date() - seq(360, 0, -5)
        duration <- "5 D"
    }
    else if (barSize == "15 mins") {
        endDateTime <- Sys.Date() - seq(360, 0, -10)
        duration <- "10 D"
    }
    reqHistoricalData(conn, Contract, barSize = barSize, duration = duration, 
        endDateTime = endDateTime, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
