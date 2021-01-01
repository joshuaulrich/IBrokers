\name{reqHistoricalData}
\alias{reqHistoricalData}
\alias{reqHistory}
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
                  timeFormat = "1",
                  tzone = "",
                  verbose = TRUE,
                  tickerId = "1",
                  eventHistoricalData,
                  file)

reqHistory(conn, Contract, barSize, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{ a \code{twsConnection} object }
  \item{Contract}{ a \code{twsContract} }
  \item{endDateTime}{ end date/time for request. See details. }
  \item{barSize}{ bar size to retrieve }
  \item{duration}{ time span the request will cover }
  \item{useRTH}{ limited to regular trading hours }
  \item{whatToShow}{ type of data to be extracted }
  \item{timeFormat}{ POSIX style or seconds from 1970-01-01 }
  \item{tzone}{ time zone of the resulting intraday series (if applicable) }
  \item{verbose}{ should progress be documented }
  \item{tickerId}{ a unique id to associte with the request }
  \item{eventHistoricalData}{ callback function to process data }
  \item{file}{ file to write data to }
  \item{\dots}{ args to pass to reqHistoricalData }
}
\details{
The \code{reqHistory} function is a simple wrapper to
request maximal history from IB. It is meant to be
used directlty, or as a template for new wrappers.

All arguments should be character strings.  Attempts will
be made to coerce, but should not be relied upon.

The \code{endDateTime} argument must be of the form
'CCYYMMDD HH:MM:SS TZ'. If not specified the current
time as returned from the TWS server will be used. This
is the preferred method for backfilling data. The \sQuote{TZ}
portion of the string is optional.

Legal \code{barSize} values are
\sQuote{1 secs},\sQuote{5 secs},\sQuote{15 secs},
\sQuote{30 mins},\sQuote{1 min},\sQuote{2 mins},
\sQuote{3 mins},\sQuote{5 mins},\sQuote{15 mins},
\sQuote{30 mins},\sQuote{1 hour},\sQuote{1 day},
\sQuote{1 week},\sQuote{1 month},\sQuote{3 months},
and \sQuote{1 year}.

Partial matching is attempted, but it is best to specify the \code{barSize}
value exactly as they are given above. There is no guarantee from the API
that all will work for all securities or durations.

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
\sQuote{ASK}, \sQuote{BID_ASK}.

\code{time.format} should simply be left alone. :D

\code{eventHistoricalData} accepts a user function to process the raw
data returned by the TWS.  This consists of a character
vector that includes the first five elements of header
information, with the fifth element specifying the number
of rows in the results set.  Passing \code{NULL} to 
\code{eventHistoricalData} will return the raw character vector.
If nothing is specified, an xts object is returned.

The \code{eventHistoricalData} function, if any, is called after all
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
The rules for historical data requests are somewhat vague. Not all
symbols have data, and those that do may only be available with specific
combinations of barSize and duration arguments. At present the only
way to know is to try the combination in question.

There is a strictly enforced 10 seconds between request pacing rule
implemented by the TWS. Keep this in mind.
IBrokers currently does \emph{not} manage this for the user via
reqHistoricalData, though reqHistory does.
}
\seealso{ \code{\link{twsContract}}, \code{\link{twsConnect}} }
\examples{
\dontrun{
tws <- twsConnect()
contract <- twsEquity('QQQQ','SMART','ISLAND')

# by default retreives 30 days of daily data
reqHistoricalData(tws, Contract=contract)

# by default retreives a year of 1 minute bars
Sys.sleep(10) # mandatory 10s between request to avoid IB pacing violation
reqHistory(tws, Contract=contract)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
