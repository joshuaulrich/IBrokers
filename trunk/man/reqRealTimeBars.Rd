\name{reqRealTimeBars}
\alias{reqRealTimeBars}
\title{ Request Real Time Bars from TWS }
\description{
Allows for streaming real-time bars to be handled in \R
}
\usage{
reqRealTimeBars(conn,
                Contract,
                whatToShow = "TRADES", 
                barSize = "5",
                useRTH = "1",
                tickerId = "1",
                file = "",
                verbose = TRUE,
                eventRealTimeBars,
                CALLBACK,
                ...) 
}
\arguments{
  \item{conn}{ a valid \code{twsConnection} object }
  \item{Contract}{ \code{twsContract} object(s) requested }
  \item{tickerId}{ the ticker id to associate with the returned bars }
  \item{whatToShow}{ what to show }
  \item{barSize}{ bar size - currently on 5 secs is TWS supported }
  \item{useRTH}{ regular trading hours }
  \item{file}{ passed to internal \code{cat} calls. See associated help. }
  \item{verbose}{ print diagnostics }
  \item{eventRealTimeBars}{ callback function, if any. }
  \item{CALLBACK}{ main reciever callback, if any. }
  \item{\dots}{ additional args to callback }
}
\details{
This function provides R level access to real time (5 second) bars
returned by the TWS API.  The Interactive Brokers documentation
should be reference for the exact meaning of the returned data.

Callbacks, via eventRealTimeBars and CALLBACK are
designed to allow for R level processing of the real-time data
stream.

eventRealTimeBars allows for direct manipulation of the actual
signal recieved.
These may be user-defined functions taking the
appropriate arguments.  Each message recieved (each new bar)
will invoke one of this callback.
By default when nothing is specified, the code will
call the default method for printing the results to the screen via
'cat'.

Note that the use of the argument 'file' will be passed to these
'cat' calls, and therefore it will be possible to use the
functionality of 'cat' directly - e.g. piping output or writing to
a connection.  The simplest use of file would be to specify the
name of a file, or open connection to append the output of the stream to.

The 'CALLBACK' argument is used for more control of the incoming
results.  This requires user-level error checking as well as TWS
API interaction.  It is here for advanced use and until documented
should be left alone.

}
\value{
The real-time bar data requested.
}
\references{ Interactive Brokers TWS API
\url{http://individuals.interactivebrokers.com/php/apiguide/apiguide.htm}
}
\author{ Jeffrey A. Ryan }
\note{
As R is single threaded - this request will run until interupted
by an error or by user action. Both will clean up after themselves
when appropriate.
}
\seealso{ \code{\link{twsConnect}},\code{\link{twsContract}} }
\examples{
\dontrun{
tws <- twsConnect()
contract <- twsEquity("QQQQ","SMART","ISLAND")
reqRealTimeBars(tws, contract)

# write to an open file connection
fh <- file('out.dat',open='a')
reqRealTimeBars(tws, contract, file=fh)
close(fh)
}
}
\keyword{ utilities }
