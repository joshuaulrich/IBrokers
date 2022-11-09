\name{reqMatchingSymbols}
\alias{reqMatchingSymbols}
\title{ Stock Contract Search }
\description{
Starting in API v973.02 and TWS v964, a function reqMatchingSymbols is available to search for stock contracts. The input can be either the first few letters of the ticker symbol, or for longer strings, a character sequence matching a word in the security name. For instance to search for the stock symbol 'IBKR', the input 'I' or 'IB' can be used, as well as the word 'Interactive'. Up to 16 matching results are returned.
}
\usage{
reqMatchingSymbols(twsconn, pattern)
}
\arguments{
  \item{twsconn}{ a valid tws connection object }
  \item{pattern}{ either start of ticker symbol or (for larger strings) company name }
}
\value{
dataframe: conId, symbol, secType, primaryExchange, currency, derivateSecTypes
}
\references{ Interactive Brokers \url{https://www.interactivebrokers.com}}
\author{ J.W. de Roode }
\examples{
\dontrun{
tws <- twsConnect()
reqMatchingSymbols(tws, pattern="IB")
}
}
\keyword{ utilities }
