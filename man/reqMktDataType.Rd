\name{reqMktDataType}
\alias{reqMktDataType}
\title{ Request Market Data Type from TWS }
\description{
Set the market data type with TWS
}
\usage{
reqMktDataType(conn, mktDataType = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{ a valid \code{twsConnection} or \code{twsPlayback} connection }
  \item{mktDataType}{ market data type code }
}
\details{
This function sets the market data type that will be returned by
TWS when \code{reqMktData} is called.

\describe{
  \item{1}{Real-time: Live data is streamed back in real time. Market data
           subscriptions are required to receive live market data.}
  \item{2}{Frozen: Market data is the last data recorded at market close.
           Frozen data requires TWS/IBG v.962 or higher and the same market
           data subscriptions necessary for real time streaming data.}
  \item{3}{Delayed: Market data 15-20 minutes behind real-time (depending on
           the exchange). Automatically use delayed data if user does not
           have a real-time subscription. Ignored if real-time data is
           available.}
  \item{4}{Delayed-frozen: Requests delayed "frozen" data for users without
           market data subscriptions.}
}
}
\value{
\code{NULL} (invisibly)
}
\references{ Interactive Brokers API:
              \url{https://interactivebrokers.github.io/tws-api/index.html} }
\author{ Joshua M. Ulrich }
\seealso{ \code{\link{twsConnect}},
          \code{\link{reqMktData}} }
\examples{
\dontrun{
tws <- twsConnect()
contract <- twsEquity("QQQQ","SMART","ISLAND")
# set market data type to 'delayed'
reqMktDataType(tws, 3)
reqMktData(tws, contract)
}
}
\keyword{ utilities }
