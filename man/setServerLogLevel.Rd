\name{setServerLogLevel}
\alias{setServerLogLevel}
\title{ Enable API Logging Via TWS }
\description{
Set level of API logging to be done by TWS.
}
\usage{
setServerLogLevel(conn, logLevel = 2)
}
\arguments{
  \item{conn}{ a valid \code{twsConnection} }
  \item{logLevel}{ an integer from 1 to 5 }
}
\details{
Calling this function will set the logging
level for the current connection according
to the following table:

\enumerate{
  \item{1:}{SYSTEM (least detail)}
  \item{2:}{ERROR (default)}
  \item{3:}{WARNING}
  \item{4:}{INFORMATION}
  \item{5:}{DETAIL (most detail)}
}

See TWS documentation for further details.
}

\value{
This function is called for its side-effects.
}
\references{ TWS API Logging
             \url{https://interactivebrokers.github.io/tws-api/support.html#tws_logs}
             \url{https://interactivebrokers.github.io/tws-api/classIBApi_1_1EClient.html#a62ed6f4f391c86743c566d44c29dae48} }
\author{ Jeffrey A. Ryan }
\note{ 
The online documentation warns of performance overhead
when setting \code{logLevel=5}.
}
\keyword{ utilities }
