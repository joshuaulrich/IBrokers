\name{twsEquity}
\alias{twsEquity}
\title{ Create a twsEquity }
\description{
Create a twsEquity for use in API calls.
}
\usage{
twsEquity(symbol,
          exch,
          primary,
          strike='0.0',
          currency='USD',
          right='',
          local='',
          multiplier='',
          include_expired='0')
}
\arguments{
  \item{symbol}{ the IB symbol requested }
  \item{exch}{ the requested exchange }
  \item{primary}{ the primary exchange of the security }
  \item{strike}{ the strike price }
  \item{currency}{ the requested currency }
  \item{right}{ the requested right }
  \item{local}{ the local security name }
  \item{multiplier}{ the contract multiplier }
  \item{include_expired}{ should expired contracts be included }
}
\details{
A wrapper to \code{twsContract} to make \sQuote{equity}
contracts easier to specify.
}
\value{
A \code{twsContract} object.
}
\references{ Interactive Brokers: \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{reqHistoricalData}}, \code{\link{twsContract}} }
\examples{
equity <- twsEquity("AAPL","SMART","ISLAND")
}
\keyword{ utilities }
