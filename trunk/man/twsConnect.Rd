\name{twsConnect}
\alias{twsConnect}
\alias{twsConnect2}
\alias{is.twsConnection}
\alias{is.twsPlayback}
\alias{isConnected}
\alias{twsDisconnect}
\title{ Establish, Check or Terminate a Connection to TWS }
\description{
Functions to initiate, check, or disconnect from the Trader
Workstation (TWS).
}
\usage{
twsConnect(clientId = 1, host = 'localhost', 
           port = 7496, verbose = TRUE, timeout = 5,
           filename = NULL)

twsDisconnect(con)

isConnected(x)
is.twsConnection(x)
is.twsPlayback(x)
}
\arguments{
  \item{clientId}{ the unique client id to associate with }
  \item{host}{ the host server }
  \item{port}{ the port that the TWS is listening on }
  \item{verbose}{ should the connection attempt be verbose }
  \item{timeout}{ length in seconds before aborting attempt }
  \item{filename}{ file containing recorded TWS data }
  \item{con}{ a valid \code{twsConnection} object }
  \item{x}{ a connection to be checked }
}
\details{
Returns a \code{twsConnection} object for use in
subsequent TWS API calls. Attempting to create another
connection to the server with the same clientId will
result in an error.

If \code{filename} is set to a file containing
data recorded in the standard TWS format - calls
using this connection will playback the recorded
data.
}
\value{
A \code{twsconn} object.
}
\references{ Interactive Brokers: \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\note{
While it is not strictly required to disconnect via \code{twsDisconnect}
it is probably advisable.

If not set options(digits.secs=6) will be called
internally to properly represent on screen the
R based timestamps.
}
\examples{
\dontrun{
tws <- twsConnect()
twsDisconnect(tws)
}
}
\keyword{ utilities }
