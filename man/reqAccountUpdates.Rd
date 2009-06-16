\name{reqAccountUpdates}
\alias{reqAccountUpdates}
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
                  CALLBACK=twsCALLBACK, ...)

.reqAccountUpdates(conn, subscribe = TRUE, acctCode = "1")
}
\arguments{
  \item{conn}{ A twsConnection object }
  \item{subscribe}{ subscribe (TRUE) or unsubscribe (FALSE) }
  \item{acctCode}{ an account description - not used for most accounts }
  \item{eventWrapper}{ message-level callback closure }
  \item{CALLBACK}{ main receiver loop, if any }
  \item{\dots}{ additional args }
}
\details{
By default, for non-FA accounts, this returns the current login's
account information.

This main version simply prints to screen the results
returned by the TWS.  \code{.reqAccountUpdates} sends
the request to subscribe or cancel, but returns immediately.
This is designed to be used within a larger
custom callback routine, where the \code{eventWrapper} object
passed to processMsg (see also) keeps trace of the portfolio
updates in a consistent manner.
}
\references{ Interactive Brokers API: \url{http://www.interactivebrokers.com} }
\author{ Jeffrey A. Ryan }

\examples{
\dontrun{
  tws <- twsConnect()

  reqAccountUpdates(tws)        # this will require a user interrupt
  .reqAccountUpdates(tws)       # this will return immediately

  .reqAccountUpdates(tws, "0")  # cancel the request
  cancelAccountUpdates(tws)     # the same

  twsDisconnect(tes)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
\keyword{ utilities }
