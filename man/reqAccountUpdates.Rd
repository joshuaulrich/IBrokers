\name{reqAccountUpdates}
\alias{reqAccountUpdates}
\alias{twsPortfolioValue}
\alias{.reqAccountUpdates}
\title{ Request Account Updates }
\description{
Request and view account details from Interactive Brokers
}
\usage{
reqAccountUpdates(conn, 
                  subscribe = TRUE,
                  acctCode = "1",
                  eventWrapper = eWrapper(), 
                  CALLBACK=twsCALLBACK, 
                  ...)

.reqAccountUpdates(conn, subscribe = TRUE, acctCode = "1")

twsPortfolioValue(x, zero.pos=TRUE, ...)
}
\arguments{
  \item{conn}{ A twsConnection object }
  \item{subscribe}{ subscribe (TRUE) or unsubscribe (FALSE) }
  \item{acctCode}{ an account description - not used for most accounts }
  \item{eventWrapper}{ message-level callback closure }
  \item{CALLBACK}{ main receiver loop, if any }
  \item{x}{ object to extract PortfolioValue from. See details. }
  \item{zero.pos}{ should PortfolioValue include zero positions? }
  \item{\dots}{ additional args }
}
\details{
By default, for non-FA accounts, this returns the current login's
account information.

This main version returns a list of objects as
returned by the TWS.  \code{.reqAccountUpdates} sends
the request to subscribe or cancel, but returns immediately.
This is designed to be used within a larger
custom callback routine, where the \code{eventWrapper} object
passed to processMsg (see also) keeps trace of the portfolio
updates in a consistent manner.

\code{twsPortfolioValue} extracts into a data.frame commonly
used fields from all positions held. There are currently methods
for the the default returned object of \code{reqAccountUpdates}.
}
\references{ Interactive Brokers API: \url{http://www.interactivebrokers.com} }
\author{ Jeffrey A. Ryan }

\examples{
\dontrun{
  tws <- twsConnect()

  reqAccountUpdates(tws)        # this will return a AccountUpdate object
  .reqAccountUpdates(tws)       # this will return immediately

  .reqAccountUpdates(tws, FALSE)  # cancel the request
  cancelAccountUpdates(tws)     # the same

  twsDisconnect(tes)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
\keyword{ utilities }
