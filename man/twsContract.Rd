\name{twsContract}
\alias{twsContract}
\alias{twsIND}
\alias{twsIndex}
\alias{is.twsContract}
\alias{as.twsContract}
\title{ Create a twsContract }
\description{
Create, test, and coerce a twsContract for use in API calls.
}
\usage{
twsContract(conId,
            symbol,
            sectype,
            exch,
            primary,
            expiry,
            strike,
            currency,
            right,
            local,
            multiplier,
            combo_legs_desc,
            comboleg,
            include_expired,
            secIdType = "",
            secId = ""
            )

is.twsContract(x)

as.twsContract(x, ...)
}

\arguments{
  \item{conId}{ the IB contract ID }
  \item{symbol}{ the IB symbol requested }
  \item{sectype}{ the security type }
  \item{exch}{ the requested exchange }
  \item{primary}{ the primary exchange of the security }
  \item{expiry}{ the expiration date }
  \item{strike}{ the strike price }
  \item{currency}{ the requested currency }
  \item{right}{ the requested right }
  \item{local}{ the local security name }
  \item{multiplier}{ the contract multiplier }
  \item{combo_legs_desc}{ not implemented yet }
  \item{comboleg}{ not implemented yet }
  \item{include_expired}{ should expired contracts be included }
  \item{secIdType}{ unique identifier for secIdType }
  \item{secId}{ security identifier: ISIN, CUSIP, SEDOL, RIC }
  \item{x}{ object to test or coerce }
  \item{\dots}{ additional arguments }
}
\details{
These are directly from the TWS API.  See that help until I can
find time to fill in this one.

More useful for specific requests are \code{twsEquity},
\code{twsOption}, \code{twsBond}, \code{twsFuture}, 
and \code{twsCurrency}.
}
\value{
A \code{twsContract} object.
}
\references{ Interactive Brokers: \url{www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{reqHistoricalData}} }
\examples{
contract <- twsContract(0,"AAPL","STK","SMART","ISLAND",
                        "","0.0","USD","","","",NULL,NULL,"0")
}
\keyword{ utilities }
