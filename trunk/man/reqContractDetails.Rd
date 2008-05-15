\name{reqContractDetails}
\alias{reqContractDetails}
\title{ Request Contract Details From TWS }
\description{
Returns an object (list) of details relating
to a particular IB tradeable product.
}
\usage{
reqContractDetails(conn, Contract, reqId = "1", conId = "", verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{ a valid \code{twsConnection} }
  \item{Contract}{ a valid \code{twsContract} }
  \item{reqId}{ a unique ID }
  \item{conId}{ a contract ID }
  \item{verbose}{ be verbose? }
}
\details{
Returns a list of details for the product specified.  See the TWS
API for specifics at this point.
}
\value{
A \code{twsContractDetails} object.
}
\references{ Interactive Brokers \url{http://www.interactivebrokers.com } }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{twsContract}} }
\examples{
\dontrun{
tws <- twsConnect()
reqContractDetails(tws, twsEquity("QQQQ"))
}
}
\keyword{ utilities }
