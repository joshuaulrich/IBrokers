\name{ibgConnect}
\alias{ibgConnect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ibgConnect(clientId = 1, host = "localhost", port = 4001, verbose = TRUE, timeout = 5, filename = NULL, blocking = .Platform$OS.type == "windows")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clientId}{
%%     ~~Describe \code{clientId} here~~
}
  \item{host}{
%%     ~~Describe \code{host} here~~
}
  \item{port}{
%%     ~~Describe \code{port} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{timeout}{
%%     ~~Describe \code{timeout} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{blocking}{
%%     ~~Describe \code{blocking} here~~
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
function (clientId = 1, host = "localhost", port = 4001, verbose = TRUE, 
    timeout = 5, filename = NULL, blocking = .Platform$OS.type == 
        "windows") 
{
    twsConnect(clientId, host, port, verbose, timeout, filename)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
