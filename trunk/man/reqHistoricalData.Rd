\name{reqHistoricalData}
\alias{reqHistoricalData}
\alias{cancelHistoricalData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Request Historical Data From TWS }
\description{
Makes a request to the Interactive Brokers Trader Workstation (TWS),
and returns an xts object containing the results of the request
if successful.
}
\usage{
reqHistoricalData(conn, 
                  Contract,
                  endDateTime,
                  barSize = "1 day",
                  duration = "1 M",
                  useRTH = "1",
                  whatToShow = "TRADES", 
                  time.format = "1",
                  verbose = TRUE,
                  tickerId = "1",
                  timeout = 10,
                  callback,
                  file)

cancelHistoricalData(con,tickerId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{ a \code{tws} connection object }
  \item{Contract}{ a \code{twsContract} }
  \item{endDateTime}{ end date/time for request. See details. }
  \item{barSize}{ bar size to retrieve }
  \item{duration}{ time span the request will cover }
  \item{useRTH}{ limited to regular trading hours }
  \item{whatToShow}{ type of data to be extracted }
  \item{time.format}{ POSIX style or seconds from 1970-01-01 }
  \item{verbose}{ should progress be documented }
  \item{tickerId}{ a unique id to associte with the request }
  \item{timeout}{ length of time before cancelling request }
  \item{callback}{ callback function to process data }
  \item{file}{ file to write data to }
  \item{con}{ an \R connection object to the TWS API }
}
\details{
All arguments should be character strings.  Attempts will
be made to coerce, but should not be relied upon.

The \code{endDateTime} argument must be of the form
'CCYYMMDD HH:MM:SS TZ'. If not specified the current
time as returned from the TWS server will be used. This
is the preferred method for backfilling data. The \sQuote{TZ}
portion of the string is optional.

Legal \code{barSize} settings are technically
\sQuote{1 secs},\sQuote{5 secs},\sQuote{15 secs},
\sQuote{30 mins},\sQuote{1 min},\sQuote{2 mins},
\sQuote{3 mins},\sQuote{5 mins},\sQuote{15 mins},
\sQuote{30 mins},\sQuote{1 hour},\sQuote{1 day},
\sQuote{1 week},\sQuote{1 month},\sQuote{3 months},
and \sQuote{1 year}. They must be specified exactly
and there is no guarantee from the API that all
will work for all securities or durations. 

The duration string must be of the form \sQuote{n S}
where the last character may be any one of
\sQuote{S} (seconds),
\sQuote{D} (days),
\sQuote{W} (weeks),
\sQuote{M} (months),
and \sQuote{Y} (year).
At present the limit for years is 1.

\code{useRTH} takes either \sQuote{1} or \sQuote{0},
indicating the request to return only regular trade
hour data, or all data, respectively.

\code{whatToShow} can be any one of the following,
though depending on the overall request it may not succeed.
\sQuote{TRADES}, \sQuote{MIDPOINT}, \sQuote{BID},
\sQuote{ASK}, \sQuote{BID/ASK}.

\code{time.format} should simply be left alone. :D

\code{callback} accepts a user function to process the raw
data returned by the TWS.  This consists of a character
vector that includes the first five elements of header
information, with the fifth element specifying the number
of rows in the results set.  Passing \code{NULL} to 
\code{callback} will return the raw character vector.
If nothing is specified, an xts object is returned.

The \code{callback} function, if any, is called after all
data has been received by the client.

The \code{file} argument calls \code{write.table} to produce
output suitable to reading in by \code{read.csv}. The \code{file}
argument is passed to the write.table call, and if an empty
string will return the output to the console.

The \emph{hasGaps} column is converted automatically from
(true,false) to 1 or 0, respectively. 
}
\value{
Returns an \code{xts} object containing the requested data, along
with additional information stored in the objects \code{xtsAttributes},
unless \code{callback} or \code{file} is specified.
}
\references{ Interactive Brokers \url{www.interactivebrokers.com} }
\author{ Jeffrey A. Ryan }
\note{
Historical data via the TWS API is at best unreliable. As historical
data is not a priority of the IB API, outside of regular trading
hours it may become unresponsive or unavailable.  The only current
way to handle this is to generally to disconnect from the TWS
via \code{twsDisconnect} and then reconnect with \code{twsConnect}.

There is a strictly enforced 6 seconds between request pacing rule
implemented by the TWS. Keep this in mind.
IBrokers currently does \emph{not} manage this for the user.

The TWS workstation must be running on the same machine
as \R.

The TWS must also be set up to allow for the API to be connected
to.  See the associated help on the main Interactive Brokers
website.

The underlying API is subject to changes on an ongoing basis by
Interactive Brokers.  All attempts have been made to keep this
current.  If something should fail, please contact the maintainer
of this package.
}
\seealso{ \code{\link{twsContract}}, \code{\link{twsConnect}} }
\examples{
\dontrun{
tws <- twsConnect()
contract <- twsEquity('QQQQ','SMART','ISLAND')

reqHistoricalData(tws,Contract=contract)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
