\name{processMsg}
\alias{processMsg}
\title{ Main TWS-API Event Manager }
\description{
Function to manage all incoming messages
from the TWS in a consistent manner.

This is used within the context of an
event loop (often twsCALLBACK) and allows
for custom processing by message type
via the eWrapper argument.
}
\usage{
processMsg(curMsg, con, eWrapper, timestamp, file, twsconn, ...)
}
\arguments{
  \item{curMsg}{ The current incoming message }
  \item{con}{ a socket connection from a \code{twsConnection} }
  \item{eWrapper}{ a functional closure with methods for each message }
  \item{timestamp}{ the timestamp format needed }
  \item{file}{ the file or connection to write to }
  \item{twsconn}{ the twsConnection object }
  \item{\dots}{ additional arguments to internal calls }
}
\details{
This is used internally within the context
of a larger infinite listener/loop.

The basic process involves one or more requests
to the TWS for data/action, followed by a call
to \code{twsCALLBACK}.  Inside of the CALLBACK
is a loop that fetches the incoming message type,
and calls \code{processMsg} at each new message.

\code{processMsg} internally is a series of
if-else statements that branch according to a known
incoming message type.  The \code{eWrapper} object
is a closure containing a data environment that
is static and a collection of callback
functions for each type of incoming data.

This eWrapper function can be defined at
multiple points prior to the use within processMsg,
to allow for access to data outside of the processMsg
call, as well as facilitate custom handling in
an efficient manner.
}
\value{
Called for its side-effects.
}
\references{ Interactive Brokers: \url{https://www.interactivebrokers.com/} }
\author{ Jeffrey A. Ryan }
\note{ 
The entire mechanism (twsCALLBACK -> processMsg -> eWrapper)
is modeled after the official API.
}
\seealso{ \code{\link{twsCALLBACK}}, \code{\link{eWrapper}} }
\keyword{ utilities }
