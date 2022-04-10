\name{reqMktData}
\alias{reqMktData}
\alias{cancelMktData}
\title{ Request Market Data Feed from TWS }
\description{
Allows for streaming market data 
to be handled in \R.  
}
\usage{
reqMktData(conn, 
           Contract,
           tickGenerics = "100,101,104,106,165,221,225,236",
           snapshot = FALSE,
           tickerId = "1",
           timeStamp = "\%Y\%m\%d \%H:\%M:\%OS",
           playback = 1,
           file = "",
           verbose = TRUE,
           eventWrapper = eWrapper(),
           CALLBACK = twsCALLBACK, ...)

cancelMktData(conn,tickerId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{ a valid \code{twsConnection} or \code{twsPlayback} connection }
  \item{Contract}{ \code{twsContract} object(s) requested data for }
  \item{tickGenerics}{ a comman delimited string of generic tick types }
  \item{snapshot}{ should snapshot data be returned }
  \item{tickerId}{ the ticker id to associate with the returned data }
  \item{timeStamp}{ include R time stamps }
  \item{playback}{ playback speed adjustment }
  \item{file}{ passed to internal \code{cat} calls. See associated help }
  \item{verbose}{ print diagnostics? }
  \item{eventWrapper}{ eWrapper object }
  \item{CALLBACK}{ main reciever callback}
  \item{\dots}{ additional args }
}
\details{
This function provides \R level access to market data streams
as returned by the TWS API.  The Interactive Brokers documentation
should be reference for the exact meaning of the returned data.

\code{timeStamps} is unique to the R API in that each
incoming signal will be marked with a (potentially) unique
timestamp.  Alternatively it is possible to
pass a formatting string for use in
\code{format(Sys.time())}. To suppress the time stamp
set the argument to NULL.  This is \emph{not} sent by
the TWS - merely prepended to the output by \R.

Callbacks, via CALLBACK and eventWrapper
are designed to allow for \R level processing
of the real-time data stream.

Each message recieved (each
update to the market data) will invoke one
the appropriately names eWrapper callback,
depending on the message type.  By default when nothing is
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
The real-time market data from the TWS.
}
\references{ Interactive Brokers API:
              \url{https://interactivebrokers.github.io/tws-api/index.html} }
\author{ Jeffrey A. Ryan }
\note{ 
As \R is single threaded - this request will run until
interupted by an error or by user action. Both will
clean up after themselves when appropriate.
}
\seealso{ \code{\link{twsCALLBACK}},
          \code{\link{eWrapper}},
          \code{\link{twsConnect}},
          \code{\link{twsContract}} }
\examples{
\dontrun{
tws <- twsConnect()
contract <- twsEquity("QQQQ","SMART","ISLAND")
reqMktData(tws, contract)

# write to an open file connection
fh <- file('out.dat',open='a')
reqMktData(tws, contract, file=fh)
close(fh)
}
}
\keyword{ utilities }
