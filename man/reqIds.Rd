\name{reqIds}
\alias{reqIds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reqIds(conn, numIds = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{numIds}{
%%     ~~Describe \code{numIds} here~~
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
function (conn, numIds = 1) 
{
    if (!is.twsConnection(conn)) 
        stop("requires twsConnection object")
    con <- conn[[1]]
    VERSION <- "1"
    writeBin(.twsOutgoingMSG$REQ_IDS, con)
    writeBin(VERSION, con)
    writeBin(as.character(numIds), con)
    con <- conn[[1]]
    e_next_id <- eWrapper(NULL)
    e_next_id$nextValidId <- function(curMsg, msg, timestamp, 
        file, ...) {
        msg[2]
    }
    while (TRUE) {
        socketSelect(list(con), FALSE, 0.1)
        curMsg <- readBin(con, character(), 1)
        nextValidID <- processMsg(curMsg, con, eWrapper = e_next_id, 
            timestamp = NULL, file = "")
        if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) 
            break
    }
    return(nextValidID)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
