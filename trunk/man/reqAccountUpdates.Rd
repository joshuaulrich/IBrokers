\name{reqAccountUpdates}
\alias{reqAccountUpdates}
\title{ Request Account Updates }
\description{
Request and view account details from Interactive Brokers
}
\usage{
reqAccountUpdates(conn, account = "1")
}
\arguments{
  \item{conn}{ A twsConnection object }
  \item{account}{ an account description - not used for most accounts }
}
\details{
By default, for non-FA accounts, this returns the current login's
account information.

This returns a data.frame with 4 columns. The first column is the
name of the account value being returned. The second is the value
associated with the specific name in column 1.  The third column
contains the currency, if applicable.  The fourth contains the
account the information applies to.
}
\value{
A data.frame with key account information.
}
\references{ Interactive Brokers API: \url{http://www.interactivebrokers.com} }
\author{ Jeffrey A. Ryan }

\examples{
\dontrun{
  tws <- twsConnect()
  reqAccountUpdates(tws)
  twsDisconnect(tes)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
\keyword{ utilities }
