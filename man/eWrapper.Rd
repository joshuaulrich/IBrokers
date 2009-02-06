\name{eWrapper}
\alias{eWrapper}
\alias{eWrapper.MktData.CSV}
\title{ eWrapper Closure For Message Processing }
\description{
Create an eWrapper closure to allow for custom
incoming message management.
}
\usage{
eWrapper(debug = FALSE)

eWapper.MktData.CSV()
}
\arguments{
  \item{debug}{ should debugging be enabled }
}
\details{
\pkg{IBrokers} implements an eWrapper scheme similar
to that provided by the official Java API.

The general idea is that each real-time data
capture function must manage all incoming signals
correctly, while allowing for the end user to
create custom handlers for each specific event.

Internal to the \code{reqMktData} and \code{reqMktDepth}
functions is a single call to the CALLBACK routine
passed to it.  By default this is \code{twsCALLBACK} (see also).
A standard argument to this callback is an
eventWrapper --- which is an instance of eWrapper.

eWrapper is an \R closure that contains a list
of functions to manage all incoming message type, as
found in \code{.twsIncomingMSG}.  Each message has a corresponding
function in the eWrapper designed
to handle the particular details of each incoming message type.

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
}
\value{
A list of functions to be used for the \code{eventWrapper} argument
to \code{reqMktData} and \code{reqMktDepth}
}
\references{ ~put references to the literature/web site here ~ }
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
