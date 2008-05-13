\name{reqMktDepth}
\alias{reqMktDepth}
\alias{cancelMktDepth}
\title{ Request Market Depth Feed from TWS }
\description{
Allows for streaming market depth (order book) data
to be handled in \R.  
}
\usage{
reqMktDepth(conn, 
            Contract,
            tickerId = "1",
            numRows = "20",
            timeStamp = TRUE,
            file = "",
            verbose = TRUE,
            eventUpdateMktDepth,
            eventUpdateMktDepthL2,
            CALLBACK, ...)

cancelMktDepth(conn,tickerId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{ a valid \code{twsConnection} connection }
  \item{Contract}{ \code{twsContract} object(s) requested data for }
  \item{tickerId}{ the ticker id to associate with the returned data }
  \item{numRows}{ depth of book }
  \item{timeStamp}{ include \R time stamps }
  \item{file}{ passed to internal \code{cat} calls. See associated help. }
  \item{verbose}{ print diagnostics? }
  \item{eventUpdateMktDepth}{ callback function, if any }
  \item{eventUpdateMktDepthL2}{ callback function, if any }
  \item{CALLBACK}{ main reciever callback, if any }
  \item{\dots}{ additional args }
}
\details{
This function provides \R level access to book data
as returned by the TWS API.  The Interactive Brokers documentation
should be reference for the exact meaning of the returned data.

\code{timeStamps} is unique to the R API in that each incoming signal
will be marked with a (potentially) unique timestamp. 
Alternatively it is possible to pass a formatting string for use
in \code{format(Sys.time())}. To suppress the time stamp set the
argument to NULL.

Callbacks, via eventUpdateMktDepth, eventUpdateMktDepthL2, or
CALLBACK are designed to allow for \R level processing
of the real-time data stream.

The first two correspond to actions based upon the actual signal
recieved.  These may be user-defined functions taking
the appropriate arguments.  Each message recieved (each
update to the market depth) will invoke one
of these callbacks.  By default when nothing is
specified, the code will call the default method for
printing the results to the screen via \code{cat}.

Note that the use of the argument \code{file} will
be passed to these \code{cat} calls, and therefore
it will be possible to use the functionality of \code{cat}
directly - e.g. piping output or writing to a connection.  The
simplest use of file would be to specify the name of a file
to append the output of the stream to.

The \code{CALLBACK} argument is used for more control of the
incoming results.  This requires user-level error checking
as well as TWS API interaction.  It is here for advanced use
and until documented should be left alone. 
}
\value{
The book depth.
}
\references{ Interactive Brokers API:
              \url{http://individuals.interactivebrokers.com/php/apiguide/apiguide.htm} }
\author{ Jeffrey A. Ryan }
\note{ 
As \R is single threaded - this request will run until
interupted by an error or by user action. Both will
clean up after themselves when appropriate.
}
\seealso{ \code{\link{twsConnect}},\code{\link{twsContract}} }
\examples{
\dontrun{
tws <- twsConnect()
contract <- twsEquity("QQQQ","SMART","ISLAND")
reqMktDepth(tws, contract)

# write to a file
reqMktDepth(tws, contract, file='out.dat')
}
}
\keyword{ utilities }
