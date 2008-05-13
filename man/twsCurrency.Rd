\name{twsCurrency}
\alias{twsCurrency}
\title{ Create a twsCurrency }
\description{
Create a twsCurrency for use in API calls.
}
\usage{
twsCurrency(symbol,
            currency='USD',
            exch='IDEAL',
            primary='',
            strike='0.0',
            right='',
            local='',
            multiplier='',
            include_expired='0')
}
\arguments{
  \item{symbol}{ the IB symbol requested }
  \item{currency}{ the requested currency }
  \item{exch}{ the requested exchange }
  \item{primary}{ the primary exchange of the security }
  \item{strike}{ the strike price }
  \item{right}{ the requested right }
  \item{local}{ the local security name }
  \item{multiplier}{ the contract multiplier }
  \item{include_expired}{ should expired contracts be included }
}
\details{
A wrapper to \code{twsContract} to make \sQuote{currency/FX}
contracts easier to specify.
}
\value{
A \code{twsContract} object.
}
\references{ Interactive Brokers: \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{reqHistoricalData}}, \code{\link{twsContract}} }
\examples{
currency <- twsCurrency("EUR")
}
\keyword{ utilities }
