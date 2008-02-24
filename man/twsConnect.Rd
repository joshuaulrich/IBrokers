\name{twsConnect}
\alias{twsConnect}
\alias{twsDisconnect}
\title{ Establish or Terminate a Connection to TWS }
\description{
Connect or disconnect to the Trader Workstation (TWS).
}
\usage{
twsConnect(port=7496,clientID=1)
twsDisconnect(con)
}
\arguments{
  \item{port}{ the port that the TWS is listening on }
  \item{clientID}{ the unique client id to associate with }
  \item{con}{ a valid \code{twsConnection} object }
}
\details{
Returns a \code{twsConnection} object for use in
subsequent TWS API calls.
}
\value{
A \code{twsConnection} object.
}
\references{ Interactive Brokers: \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\note{
While it is not strictly required to disconnect via \code{twsDisconnect}
it is probably advisable.
}
\examples{
\dontrun{
tws <- twsConnect()
twsDisconnect(tws)
}
}
\keyword{ utilities }
