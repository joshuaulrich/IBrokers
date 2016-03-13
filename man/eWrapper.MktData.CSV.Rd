\name{eWrapper.MktData.CSV}
\alias{eWrapper.MktData.CSV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eWrapper.MktData.CSV(n = 1)
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
        7), ncol = 7), 0), .Dimnames = list(NULL, c("BidSize", 
        "BidPrice", "AskPrice", "AskSize", "Last", "LastSize", 
        "Volume")))), n))
    eW$tickPrice <- function(curMsg, msg, timestamp, file, ...) {
        tickType = msg[3]
        msg <- as.numeric(msg)
        id <- as.numeric(msg[2])
        file <- file[[id]]
        data <- eW$get.Data("data")
        attr(data[[id]], "index") <- as.numeric(Sys.time())
        nr.data <- NROW(data[[id]])
        if (tickType == .twsTickType$BID) {
            cat(paste(timestamp, msg[5], msg[4], "", "", "", 
                "", "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 1:2] <- msg[5:4]
        }
        else if (tickType == .twsTickType$ASK) {
            cat(paste(timestamp, "", "", msg[4], msg[5], "", 
                "", "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 3:4] <- msg[4:5]
        }
        else if (tickType == .twsTickType$LAST) {
            cat(paste(timestamp, "", "", "", "", msg[4], "", 
                "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 5] <- msg[4]
        }
        eW$assign.Data("data", data)
        c(curMsg, msg)
    }
    eW$tickSize <- function(curMsg, msg, timestamp, file, ...) {
        data <- eW$get.Data("data")
        tickType = msg[3]
        msg <- as.numeric(msg)
        id <- as.numeric(msg[2])
        file <- file[[id]]
        attr(data[[id]], "index") <- as.numeric(Sys.time())
        nr.data <- NROW(data[[id]])
        if (tickType == .twsTickType$BID_SIZE) {
            cat(paste(timestamp, msg[4], "", "", "", "", "", 
                "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 1] <- msg[4]
        }
        else if (tickType == .twsTickType$ASK_SIZE) {
            cat(paste(timestamp, "", "", "", msg[4], "", "", 
                "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 4] <- msg[4]
        }
        else if (tickType == .twsTickType$LAST_SIZE) {
            cat(paste(timestamp, "", "", "", "", "", msg[4], 
                "", sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 6] <- msg[4]
        }
        else if (tickType == .twsTickType$VOLUME) {
            cat(paste(timestamp, "", "", "", "", "", "", msg[4], 
                sep = ","), "\n", file = file, append = TRUE)
            data[[id]][nr.data, 7] <- msg[4]
        }
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
