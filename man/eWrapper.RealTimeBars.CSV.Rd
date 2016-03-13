\name{eWrapper.RealTimeBars.CSV}
\alias{eWrapper.RealTimeBars.CSV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eWrapper.RealTimeBars.CSV(n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (n = 1) 
{
    eW <- eWrapper(NULL)
    eW$assign.Data("data", rep(list(structure(.xts(matrix(rep(NA_real_, 
        7), ncol = 7), 0), .Dimnames = list(NULL, c("Open", "High", 
        "Low", "Close", "Volume", "WAP", "Count")))), n))
    eW$realtimeBars <- function(curMsg, msg, timestamp, file, 
        ...) {
        id <- as.numeric(msg[2])
        file <- file[[id]]
        data <- eW$get.Data("data")
        attr(data[[id]], "index") <- as.numeric(msg[3])
        nr.data <- NROW(data[[id]])
        cat(paste(msg[3], msg[4], msg[5], msg[6], msg[7], msg[8], 
            msg[9], msg[10], sep = ","), "\n", file = file, append = TRUE)
        data[[id]][nr.data, 1:7] <- as.numeric(msg[4:10])
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
