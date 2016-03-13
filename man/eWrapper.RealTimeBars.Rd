\name{eWrapper.RealTimeBars}
\alias{eWrapper.RealTimeBars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eWrapper.RealTimeBars(nbars = 1, nsymbols = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nbars}{
%%     ~~Describe \code{nbars} here~~
}
  \item{nsymbols}{
%%     ~~Describe \code{nsymbols} here~~
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
function (nbars = 1, nsymbols = 1) 
{
    eW <- eWrapper(NULL)
    eW$realtimeBars <- function(curMsg, msg, timestamp, file, 
        ...) {
        id <- as.numeric(msg[2])
        data <- eW$get.Data("data")
        data[[id]][1] <- as.numeric(msg[3])
        data[[id]][2:8] <- as.numeric(msg[4:10])
        eW$assign.Data("data", data)
        c(curMsg, msg)
    }
    return(eW)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
