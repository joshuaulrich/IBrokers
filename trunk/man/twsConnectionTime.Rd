\name{twsConnectionTime}
\alias{twsConnectionTime}
\alias{serverVersion}
\title{ TWS API Utility Functions }
\description{
General API utility functions.
}
\usage{
twsConnectionTime(con)

serverVersion(con)
}
\arguments{
  \item{con}{ a twsConnection object }
}
\details{
This is simply extracted from the \code{twsConnection} object.
No API request is made.
}
\value{
The requested value.
}
\references{ Interactive Brokers LLC \url{http://www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{twsConnect}} }
\examples{
\dontrun{
twsConnectionTime(con)
serverVersion(con)
}
}
\keyword{ utilities }% __ONLY ONE__ keyword per line
