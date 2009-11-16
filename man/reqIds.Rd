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

.reqIds(conn, numIds = 1)
}
\arguments{
  \item{conn}{
  a valid twsConnection object, may be of class
  twsConnection or twsconn. See details for differences.
}
  \item{numIds}{
  currently ignored by the TWS.
}
}
\details{
The two version of reqIds operate in 3 different ways.

Using a standard twsConnection object (standard connection)
a call will be made to the TWS via the API for the next valid
id to be used in placeOrder calls. This imparts overhead, as well
as a returned message that must be handled. The main
\code{reqIds} call will wait on the TWS for a valid return
value. All subsequent \R processing will be blocked until
this succeeds.  The \code{.reqIds}
function (with the prepended dot) will not manage the incoming
messages, instead will send the request and return control
to the caller. This requires that the user routine handle the
resulting values (including errors etc.) This is meant for
use inside of complete calling routines.

The use of the newer \code{twsconn} objects (currently returned
by twsConnect2) maintain the next valid id inside of the object,
returning the current id, and incrementing by 1 with each call
to reqIds.

This serves two purposes. The first is that it is no longer needed
to make a costly and difficult to manage API call. 
This alleviates the synchonicity and blocking
issues of the previous object cases. The second purpose is that
each connection object can manage its own resources without relying
on a global state variable or list of variables.

For \code{twsconn} objects, \code{reqIds} and \code{.reqIds} results
are identical.

}
\value{
A character representation of the next numeric ID.
}
\references{
%% ~put references to the literature/web site here ~
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
