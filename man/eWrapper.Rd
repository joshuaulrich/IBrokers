\name{eWrapper}
\alias{eWrapper}
\alias{eWrapper.MktData.CSV}
\alias{eWrapper.RealTimeBars}
\alias{eWrapper.RealTimeBars.CSV}
\alias{eWrapper.data}
\alias{eWrapper.MktDepth.CSV}
\title{ eWrapper Closure For Message Processing }
\description{
Create an eWrapper closure to allow for custom
incoming message management.
}
\usage{
eWrapper(debug = FALSE, errfile=stderr())

eWrapper.data(n)

eWrapper.MktData.CSV(n=1)
eWrapper.RealTimeBars.CSV(n=1)
}
\arguments{
  \item{debug}{ should debugging be enabled }
  \item{errfile}{ where error messages are directed (stderr) }
  \item{n}{ number of contracts being watched }
}
\details{
\pkg{IBrokers} implements an eWrapper scheme similar
to that provided by the official Java API.

The general idea is that each real-time data
capture function must manage all incoming signals
correctly, while allowing for the end user to
create custom handlers for each specific event.

Internal to the \code{reqRealTimeBars}, 
\code{reqMktData}, and \code{reqMktDepth}
functions is a single call to the CALLBACK routine
passed to it.  By default this is \code{twsCALLBACK} (see also).
A standard argument to this callback is an
eventWrapper --- which is an instance of eWrapper.

eWrapper is an \R closure that contains a list
of functions to manage all incoming message type, as
found in \code{.twsIncomingMSG}.  Each message has a corresponding
function in the eWrapper designed
to handle the particular details of each incoming message type.

There is also an embedded environment in which data
can be saved and retrieved via a handful of accessor functions mimicking
the standard \R tools.

The data environment is \code{.Data}, with accessor
methods \code{get.Data}, \code{assign.Data}, and \code{remove.Data}.

These methods can be called from the closure object \code{eWrapper$get.Data},
\code{eWrapper$assign.Data}, etc.

The basic eWrapper call simply produces a visually informative
display of the incoming stream.  E.g. bidSize data would be represented
with a \emph{bidSize} label, instead of the internal TWS code(s) returned
by the TWS.

By creating an instance of an eWrapper, accomplished by calling
it as a function call, one can then modify any or all the particular
methods embedded in the object.

This allows for rapid customization, as well as a built in assurance
that all incoming messages will be handled appropriately without
additional programmer time and resources.

An example of this ability to modify the object is given in
the \code{eWrapper.MktData.CSV} code. This object produces
output deisgned to be space efficient, as well as easily read back into
any R session as a standard CSV file.

Setting \code{debug=NULL} will cause empty function objects
to be created within the eWrapper object returned.  This object
can be treated as a template to implement only the methods
that are needed.  By default, all functions silently return
the entire message they would normally parse.  This includes
\emph{empty} functions created by setting debug to NULL.

\code{eWrapper.data()} allows for data states to be maintained
from call to call, as an xts history of updates/messages is stored
within the object.  This is designed to minimize calling overhead by
removing unneeded function calls from each message parsed.

Additional, but creating methods that update the internal environment
of the eWrapper object, it is possible to maintain a snapshot of
last k values for any field of interest.  This is directly applicable to
implementing an automated strategy from within a custom \code{twsCALLBACK}
method.
}
\value{
A list of functions [and optionally data] to be used for the \code{eventWrapper} argument
to \code{reqMktData} and \code{reqMktDepth}
}
\author{ Jeffrey A. Ryan }
\note{ 
It is possible to also attach data to the closure object, allowing for
a single in-memory object to contain current top of book data. This is
exemplified in the \code{eWrapper.MktData.CSV} code, and can be extended
in the user's own direction.
}
\seealso{ \code{\link{twsCALLBACK}}, \code{\link{processMsg}}  }
\examples{
myWrapper <- eWrapper()

str(myWrapper)

# remove tickPrice action
myWrapper$tickPrice <- function(msg, timestamp, file, ...) {}

# add new tickPrice action
myWrapper$tickPrice <- function(msg, timestamp, file, ...) { cat("tickPrice",msg) }

# add new data into the object, and retrieve
myWrapper$assign.Data("myData", 1010)
myWrapper$get.Data("myData")

\dontrun{
tws <- twsConnect()
reqMktData(tws, twsSTK("SBUX"))
reqMktData(tws, twsSTK("SBUX"), eventWrapper=myWrapper)
twsDisconnect(tws)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
