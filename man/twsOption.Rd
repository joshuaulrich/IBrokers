\name{twsOption}
\alias{twsOption}
\alias{twsOPT}
\title{ Create a twsContract for Options }
\description{
Create a twsContract for use in API calls.
}
\usage{
twsOption(local,
          expiry="",
          strike="",
          right="",
          exch="SMART",
          primary="",
          currency='USD',
          symbol='',
          multiplier="100",
          include_expired='0',
          conId=0)
}
\arguments{
  \item{local}{ the IB symbol requested }
  \item{expiry}{ option expiration CCYYMM [optional]}
  \item{strike}{ the strike price [optional] }
  \item{right}{ the requested right - \sQuote{C},\sQuote{CALL},
                \sQuote{P}, or \sQuote{PUT} [optional]}
  \item{exch}{ the requested exchange [optional, defaults to SMART]}
  \item{primary}{ the primary exchange of the security [optional]}
  \item{currency}{ the requested currency [defaults to USD]}
  \item{symbol}{ the security name [optional]}
  \item{multiplier}{ the contract multiplier }
  \item{include_expired}{ should expired contracts be included [defaults to \dQuote{0} (false)] }
  \item{conId}{ contract ID }
}
\details{
A wrapper to \code{twsContract} to make \sQuote{option}
contracts easier to specify.

Some of the optionable parameters are contingent on the
request being made.  Refer to the \emph{TWS} documentation
for details.

\code{twsOPT} is an alias.
}
\value{
A \code{twsContract} object.
}
\note{
Option contracts on the TWS have certain rules which are different than
standard data requests.  

The \code{local} symbol is required. This can be found on the main TWS
screen under contract details, or via the web at \url{https://www.interactivebrokers.com}

Since the local symbol is required, all other values are redundant.  It is best to simply
specify the local name and let the TWS manage the lookup.

The \code{expiry} needs to be either of class \code{Date} to be coerced to
a string of format \sQuote{CCYYMM}, or provided in that format.

Historical requests cannot be for a barSize=\sQuote{1 D}
or less frequent.  

\code{barSize} must be "1 min" per Interactive Brokers API.
}
\references{ Interactive Brokers: \url{https://www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{reqMktData}}, \code{\link{twsContract}} }
\examples{
opt <- twsOption("QQQAS",expiry="200901", strike="45.0", right="C")
}
\keyword{ utilities }
