\name{IBrokers-package}
\alias{IBrokers-package}
\alias{IBrokers}
\docType{package}
\title{
R API to the Interactive Brokers Trader Workstation (TWS).
}
\description{
A very limited R implementation of the TWS API.  At present
it is only able to access historic data from the Interactive
Brokers servers. Future additions will include more API
access and possible real-time charting via the \pkg{quantmod}
package.

Recent changes have made this API implementation much more
robust on *nix platforms, and \emph{almost} usable on
Windows.  Many new error-checking calls have been
incorporated, as well as a more reliable even-loop
to capture the data from the TWS.

The underlying socket connections are pure \R. This was
a design decision to maximize cross-platform availability,
as well as a recognition that historical data
requests, or any requests while in a single threaded \R
session, must be non-threaded.

The above decision may change, though the only currently 
potentially portable implementation that \emph{might}
be made to work is hand-coded C --- as the official
API tools from Interactive Brokers are either Java-based
or Windows-centric.

Near term additions include \code{reqMktData} and
\code{reqRealTimeBars}, both with callback handlers
to allow for R code to interact with the API.

This version is mostly a proof-of-concept
and will be updated regularly.  That said, please report any
and all bugs/experiences to the maintainer so they can be
incorporated into development versions.

Additionally, beta testers are needed to make this
a viable alternative for IB-API interaction.  Don't be shy.
}
\details{
\tabular{ll}{
Package: \tab IBrokers\cr
Type: \tab Package\cr
Version: \tab 0.0-6\cr
Date: \tab 2008-04-26\cr
License: \tab GPL-3\cr
}
The current API methods supported are:

reqCurrentTime: The TWS server time in seconds since the epoch
reqHistoricalData: Fetch historical data
}
\author{
Jeffrey A. Ryan

Maintainer: Jeffrey A. Ryan <jeff.a.ryan@gmail.com>

}
\references{
Interactive Brokers: \url{www.interactivebrokers.com}
}
\keyword{ package }
\examples{
\dontrun{
tws <- twsConnect() # make a new connection to the TWS
reqCurrentTime(tws) # check the server's timestamp

contract <- twsEquity('IBKR','SMART','ISLAND') # equity specification

reqHistoricalData(tws,contract) # request historical data
twsDisconnect(tws)  # disconnect from the TWS
}
}
