\name{twsFuture}
\alias{twsFuture}
\alias{twsFUT}
\alias{twsFutureOpt}
\alias{twsFOP}
\title{ Create a twsFuture Contract }
\description{
Create a twsFuture contract for use in API calls.
}
\usage{
twsFuture(symbol,
          exch,
          expiry,
          primary='',
          currency='USD',
          right='',
          local='',
          multiplier='',
          include_expired='0',
          conId=0)
}
\arguments{
  \item{symbol}{ the IB symbol requested }
  \item{exch}{ the requested exchange }
  \item{expiry}{ the requested contract expiration }
  \item{primary}{ the primary exchange of the security }
  \item{currency}{ the requested currency }
  \item{right}{ the requested right }
  \item{local}{ the local security name }
  \item{multiplier}{ the contract multiplier }
  \item{include_expired}{ should expired contracts be included }
  \item{conId}{ contract ID }
}
\details{
A wrapper to \code{twsContract} to make \sQuote{futures}
contracts easier to specify.

\code{twsFUT} is an alias.
}
\value{
A \code{twsContract} object.
}
\references{ Interactive Brokers: \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{reqHistoricalData}}, \code{\link{twsContract}} }
\examples{
future <- twsFuture("NQ","GLOBEX","200803")
}
\keyword{ utilities }
