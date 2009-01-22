\name{reqContractDetails}
\alias{reqContractDetails}
\title{ Request Contract Details From TWS }
\description{
Returns an object (a list of class \code{twsContractDetails} objects)
 of IB contract details relating
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
A \code{twsContractDetails} object, or list of the same.
}
\references{ Interactive Brokers \url{http://www.interactivebrokers.com } }
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{twsContract}} }
\examples{
\dontrun{
tws <- twsConnect()
reqContractDetails(tws, twsEquity("QQQQ"))

# retrieve all QQQQ contracts as a list
reqContractDetails(tws, twsOption(local="", right="", symbol="QQQQ"))
# retrieve only calls
reqContractDetails(tws, twsOption(local="", right="C", symbol="QQQQ"))
# retrieve only puts
reqContractDetails(tws, twsOption(local="", right="P", symbol="QQQQ"))

opt.details <- lapply(c("MSFT","AAPL"),
                      function(x) {
                                    reqContractDetails(tws,
                                      twsOption(local="", right="",
                                                symbol=x))
                                  } )

length(opt.details) #number of symbols passed e.g. 2
sapply(opt.details, length) # contracts per symbol
}
}
\keyword{ utilities }
