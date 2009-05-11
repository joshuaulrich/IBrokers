\name{twsCALLBACK}
\alias{twsCALLBACK}
\title{ Internal Data Callback Routine }
\description{
twsCALLBACK is the primary function that is called
after a request for data is sent.  Within this
call messages are recieved from the TWS, processed, and
further actions can be handled.
}
\usage{
twsCALLBACK(twsCon, eWrapper, timestamp, file, playback = 1, ...)
}
\arguments{
  \item{twsCon}{ a \code{twsConnection} object } 
  \item{eWrapper}{ a closure created by \code{eWrapper()} }
  \item{timestamp}{ a logical indicating if timestamps should be created }
  \item{file}{ the file or connection to write to }
  \item{playback}{ is this a live or playback connection }
  \item{\dots}{ additional arguments to internal calls }
}
\details{
This function is used as the primary management tool
within all data calls built into \pkg{IBrokers}.

It works as is, or can be modified to manage unique
data and trading requirements.

The general logic of the function is to
recieve the header to each incoming message from the TWS.
This then gets passed to the \code{processMsg} function, along with
the eWrapper object.

The eWrapper object can maintain state data (prices), and
has functions for managing all incoming message types from the TWS.

Once the processMsg call returns, another cycle of the infinite loop
occurs.

If the eWrapper object is used to maintain state information, it is
possible to access this information from outside of the processMsg
call, and thus be able to apply trade logic based upon the data
acquired from the TWS.

An example will soon be available in the vignettes included in the package.
}
\value{
No value is returned.  This function is called for its side effects.
}
\author{ Jeffrey A. Ryan }
\seealso{ \code{\link{eWrapper}} }
\keyword{ misc }% __ONLY ONE__ keyword per line
\keyword{ utilities }% __ONLY ONE__ keyword per line
