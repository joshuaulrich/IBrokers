\name{twsOption}
\alias{twsOption}
\alias{twsOPT}
\title{ Create a twsContract for Options }
\description{
Create a twsContract for use in API calls.
}
\usage{
twsOption(local,
          expiry,
          strike,
          right="C",
          exch="SMART",
          primary="",
          currency='USD',
          symbol='',
          include_expired='0')
}
\arguments{
  \item{local}{ the IB symbol requested }
  \item{expiry}{ option expiration CCYYMM - default to front month}
  \item{strike}{ the strike price }
  \item{right}{ the requested right - \sQuote{C},\sQuote{CALL},
                \sQuote{P}, or \sQuote{PUT} }
  \item{exch}{ the requested exchange }
  \item{primary}{ the primary exchange of the security }
  \item{currency}{ the requested currency }
  \item{symbol}{ the security name }
  \item{include_expired}{ should expired contracts be included }
}
\details{
A wrapper to \code{twsContract} to make \sQuote{option}
contracts easier to specify.
}
\value{
A \code{twsContract} object.
}
\references{ Interactive Brokers: \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{reqMktData}}, \code{\link{twsContract}} }
\examples{
opt <- twsOption("AEOQW",strike="17.5", right="PUT")
}
\keyword{ utilities }
