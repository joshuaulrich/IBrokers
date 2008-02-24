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

A word of warning.  I wrote this in 3 days. 
 Much of the current event loop is being
rewritten to better handle errors, as well as speed up
large data requests.  This version is mostly a proof-of-concept
and will be updated regularly.  That said, please report any
and all bugs/experiences to the maintainer so they can be
incorporated into development versions.
}
\details{
\tabular{ll}{
Package: \tab IBrokers\cr
Type: \tab Package\cr
Version: \tab 0.0-1\cr
Date: \tab 2008-02-21\cr
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
