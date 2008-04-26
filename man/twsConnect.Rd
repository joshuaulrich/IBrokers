\name{twsConnect}
\alias{twsConnect}
\alias{twsDisconnect}
\title{ Establish or Terminate a Connection to TWS }
\description{
Connect or disconnect to the Trader Workstation (TWS).
}
\usage{
twsConnect(clientId = 1, host = 'localhost', 
           port = 7496, verbose = TRUE, timeout = 10)
twsDisconnect(con)
}
\arguments{
  \item{clientId}{ the unique client id to associate with }
  \item{host}{ the host server }
  \item{port}{ the port that the TWS is listening on }
  \item{verbose}{ should the connection attempt be verbose }
  \item{timeout}{ length in seconds before aborting attempt }
  \item{con}{ a valid \code{twsConnection} object }
}
\details{
Returns a \code{twsConnection} object for use in
subsequent TWS API calls. Attempting to create another
connection to the server with the same clientId will
result in an error.
}
\value{
A \code{twsConnection} object.
}
\references{ Interactive Brokers: \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\note{
While it is not strictly required to disconnect via \code{twsDisconnect}
it is probably advisable.

This may not work the first time on a Windows machine. The best
course of action would be to upgrade your OS 
to something more robust like *nix.
}
\examples{
\dontrun{
tws <- twsConnect()
twsDisconnect(tws)
}
}
\keyword{ utilities }
