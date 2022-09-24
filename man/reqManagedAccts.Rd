\name{reqManagedAccts}
\alias{reqManagedAccts}
\title{ Managed Accounts }
\description{
A single username can handle more than one account. As mentioned in the Connectivity section, the TWS will automatically send a list of managed accounts once the connection is established. The list can also be fetched via the IBApi.EClient.reqManagedAccts method. For an individual account, this call works as well and returns a single account.
}
\usage{
reqManagedAccts(twsconn)
}
\arguments{
  \item{twsconn}{ a valid tws connection object }
}
\value{
Individual account: a string containing a single account number. For a FamilyAccount  it returns a string with a ',' separated list of available accounts.
}
\references{ Interactive Brokers \url{https://www.interactivebrokers.com}}
\author{ Jeffrey A. Ryan }
\examples{
\dontrun{
tws <- twsConnect()
reqManagedAccts(tws)
}
}
\keyword{ utilities }
