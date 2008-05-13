\name{reqCurrentTime}
\alias{reqCurrentTime}
\title{ Request The Current TWS Time }
\description{
Returns the current time from the TWS
server, expressed as seconds since
1970-01-01 GMT.
}
\usage{
reqCurrentTime(conn)
}
\arguments{
  \item{conn}{ a valid tws connection object }
}
\value{
Seconds since 1970-01-01 GMT
}
\references{ Interactive Brokers \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\examples{
\dontrun{
tws <- twsConnect()
reqCurrentTime(tws)
}
}
\keyword{ utilities }
