\name{reqCurrentTime}
\alias{reqCurrentTime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqCurrentTime(twsconn)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsconn}{
%%     ~~Describe \code{twsconn} here~~
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
function (twsconn) 
{
    .reqCurrentTime(twsconn)
    con <- twsconn[[1]]
    e_current_time <- eWrapper()
    e_current_time$currentTime <- function(curMsg, msg, timestamp, 
        file, ...) {
        msg[2]
    }
    while (isConnected(twsconn)) {
        socketSelect(list(con), FALSE, NULL)
        curMsg <- readBin(con, character(), 1)
        currentTime <- processMsg(curMsg, con, eWrapper = e_current_time, 
            twsconn = twsconn, timestamp = NULL, file = "")
        if (curMsg == .twsIncomingMSG$CURRENT_TIME) 
            break
    }
    structure(as.numeric(currentTime), class = c("POSIXt", "POSIXct"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
