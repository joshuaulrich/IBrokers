\name{exerciseOptions}
\alias{exerciseOptions}
\title{
Exercise Options Contracts
}
\description{
Send message to exercise option contracts.
}
\usage{
exerciseOptions(twsconn, 
                contract, 
                exerciseAction = 1, 
                exerciseQuantity = 1, 
                account = "", 
                override = 0, 
                tickerId = 1)
}
\arguments{
  \item{twsconn}{
A twsConnection object
}
  \item{contract}{
A twsContract object
}
  \item{exerciseAction}{
exercise=1 or lapse=2
}
  \item{exerciseQuantity}{
number of contracts to exercise
}
  \item{account}{
IB account [institutional orders]
}
  \item{override}{
override system's natural action. 0 for do not override, 1 for override
}
  \item{tickerId}{
id for request
}
}
\details{
Exercise option contract.
}
\value{
Called for its side-effect.
}
\references{
\url{http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/java/exerciseoptions.htm}
}
\author{
Jeffrey A. Ryan
}
\note{
exch=\sQuote{SMART} is not valid in \code{exerciseOptions} calls. See
the official API for further details.
}
\keyword{ misc }
