\name{reqIds}
\alias{reqIds}
\alias{.reqIds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Request Next Valid Id
}
\description{
Get the next valid order ID for use with the TWS.
}
\usage{
reqIds(conn, numIds = 1)
}
\arguments{
  \item{conn}{
  a valid twsConnection object of class \code{twsconn}.
}
  \item{numIds}{
  currently ignored by the TWS.
}
}
\details{
\code{twsconn} objects maintain the next valid id inside of the object,
returning the current id, and incrementing by 1 with each call
to reqIds.

For \code{twsconn} objects, \code{reqIds} and \code{.reqIds} results
are identical.
}
\value{
A character representation of the next numeric ID.
}
\author{
Jeffrey A. Ryan
}
\note{
The TWS will keep track of order ids across connection ids
and sessions. The values may be reset only as outlined by the
official TWS documentation.  IBrokers simply records and
manages the data as recieved from the TWS upon initial connection.
Each connection id will have a different order id associated with it.
}

\keyword{ utilities }
