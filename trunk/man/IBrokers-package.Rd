\name{IBrokers-package}
\alias{IBrokers-package}
\alias{IBrokers}
\alias{IBrokersRef}
\docType{package}
\title{
R API to the Interactive Brokers Trader Workstation (TWS).
}
\description{
\emph{This software is in no way affiliated, endorsed, or
approved by Interactive Brokers or any of its affiliates.
It comes with absolutely no warranty and should
not be used in actual trading unless the user can
read and understand the source.}

\pkg{IBrokers} is a pure R implementation of the TWS API.  At present
it is only able pull data from the Interactive
Brokers servers via the TWS.  Future additions will include more API
access, including live
order handling, and better management across R sessions.

Possible real-time charting via the \pkg{quantmod}
package may be incorporated into future
releases.

Changes to move to version 0.1-0
have made this API implementation much more
robust on all platforms.
Many new error-checking calls have been
incorporated, as well as a more reliable event-loop
to capture the data from the TWS.

The underlying socket connections are pure \R. This was
a design decision to maximize cross-platform availability,
as well as a recognition that historical data
requests, or any requests while in a single threaded \R
session, must be non-threaded.

Recent additions include \code{reqMktData} to
handle live market data from one or more
symbols, \code{reqMktDepth} to
capture market depth for one or more
symbols, and
\code{reqRealTimeBars} to recieve 5 second
real time bars. Each of these functions
have been implemented with optional user
defined callback handlers
to allow for R code to interact with the API
while receiving data from the TWS.

Please report any
and all bugs/experiences to the maintainer so they can be
corrected or incorporated into future versions.

Additionally, beta testers are needed to make this
a viable alternative for IB-API interaction.  Don't be shy.
}
\details{
\tabular{ll}{
Package: \tab IBrokers\cr
Type: \tab Package\cr
Version: \tab 0.9-1\cr
Date: \tab 2011-02-26\cr
License: \tab GPL-3\cr
}
The current API methods supported are:

\code{twsConnect}: Establish TWS connection\cr
\code{twsDisconnect}: Close TWS connection\cr
\code{isConnected}: Check connection\cr
\code{setServerLogLevel}: Set logging level\cr

\code{twsAccountUpdates}: Get Account Details\cr
\code{reqIds}: Request next available ID\cr
\code{reqCurrentTime}: The TWS server time in seconds since the epoch\cr
\code{reqHistoricalData}: Fetch historical data\cr
\code{reqMktData}: Receive real-time market data\cr
\code{reqMktDepth}: Receive real-time order book depth\cr
\code{reqRealTimeBars}: Receive 5 second OHLCVWC bar data\cr

\emph{Experimental support:}\cr
\code{placeOrder}: Place a live order to the TWS\cr
\code{cancelOrder}: Cancel a pending order on the TWS\cr
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
IBrokersRef()       # IBrokers Reference Card in PDF viewer

tws <- twsConnect() # make a new connection to the TWS
reqCurrentTime(tws) # check the server's timestamp

contract <- twsEquity('IBKR','SMART','ISLAND') # equity specification

reqHistoricalData(tws,contract) # request historical data
twsDisconnect(tws)  # disconnect from the TWS
}
}
