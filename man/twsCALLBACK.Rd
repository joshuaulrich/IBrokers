\name{twsCALLBACK}
\alias{twsCALLBACK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twsCALLBACK(twsCon, eWrapper, timestamp, file, playback = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsCon}{
%%     ~~Describe \code{twsCon} here~~
}
  \item{eWrapper}{
%%     ~~Describe \code{eWrapper} here~~
}
  \item{timestamp}{
%%     ~~Describe \code{timestamp} here~~
}
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{playback}{
%%     ~~Describe \code{playback} here~~
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
function (twsCon, eWrapper, timestamp, file, playback = 1, ...) 
{
    if (missing(eWrapper)) 
        eWrapper <- eWrapper()
    con <- twsCon[[1]]
    if (inherits(twsCon, "twsPlayback")) {
        sys.time <- NULL
        while (TRUE) {
            if (!is.null(timestamp)) {
                last.time <- sys.time
                sys.time <- as.POSIXct(strptime(paste(readBin(con, 
                  character(), 2), collapse = " "), timestamp))
                if (!is.null(last.time)) {
                  Sys.sleep((sys.time - last.time) * playback)
                }
                curMsg <- readBin(con, character(), 1)
                if (length(curMsg) < 1) 
                  next
                processMsg(curMsg, con, eWrapper, format(sys.time, 
                  timestamp), file, ...)
            }
            else {
                curMsg <- readBin(con, character(), 1)
                if (length(curMsg) < 1) 
                  next
                processMsg(curMsg, con, eWrapper, timestamp, 
                  file, ...)
                if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) 
                  Sys.sleep(5 * playback)
            }
        }
    }
    else {
        tryCatch(while (isConnected(twsCon)) {
            if (!socketSelect(list(con), FALSE, 0.25)) 
                next
            curMsg <- readBin(con, "character", 1L)
            if (!is.null(timestamp)) {
                processMsg(curMsg, con, eWrapper, format(Sys.time(), 
                  timestamp), file, twsCon, ...)
            }
            else {
                processMsg(curMsg, con, eWrapper, timestamp, 
                  file, twsCon, ...)
            }
        }, error = function(e) {
            close(twsCon)
            stop("IB connection error. Connection closed", call. = FALSE)
        })
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
