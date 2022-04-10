\name{twsScannerSubscription}
\alias{twsScannerSubscription}
\title{
Create ScannerSubscription
}
\description{
Create an object for use with \code{reqScannerSubscription} and
\code{.reqScannerSubscription}.
}
\usage{
twsScannerSubscription(numberOfRows = -1, 
                       instrument = "", 
                       locationCode = "", 
                       scanCode = "", 
                       abovePrice = "", 
                       belowPrice = "", 
                       aboveVolume = "",
                       averageOptionVolumeAbove = "",
                       marketCapAbove = "", 
                       marketCapBelow = "", 
                       moodyRatingAbove = "", 
                       moodyRatingBelow = "", 
                       spRatingAbove = "", 
                       spRatingBelow = "", 
                       maturityDateAbove = "", 
                       maturityDateBelow = "", 
                       couponRateAbove = "", 
                       couponRateBelow = "", 
                       excludeConvertible = "", 
                       scannerSettingPairs = "", 
                       stockTypeFilter = "")
}
\arguments{
  \item{numberOfRows}{
Number of rows of scanner results returned
}
  \item{instrument}{
A character string of STK, ...
}
  \item{locationCode}{
A character string of STK.NA, STK.US, STK.US.MAJOR, ...
}
  \item{scanCode}{
One of the available scans. See details
}
  \item{abovePrice}{
Price to filter above
}
  \item{belowPrice}{
Price to filter below
}
  \item{aboveVolume}{
Volume to filter above
}
  \item{averageOptionVolumeAbove}{
Average option volume above this
}
  \item{marketCapAbove}{
Market cap to filter above
}
  \item{marketCapBelow}{
Market cap to filter below
}
  \item{moodyRatingAbove}{
Moody rating to filter above
}
  \item{moodyRatingBelow}{
Moody rating to filter below
}
  \item{spRatingAbove}{
S&P rating to filter above
}
  \item{spRatingBelow}{
S&P rating to filter below
}
  \item{maturityDateAbove}{
Maturity date to filter above
}
  \item{maturityDateBelow}{
Maturity date to filter below
}
  \item{couponRateAbove}{
Coupon rate to filter above
}
  \item{couponRateBelow}{
Coupon rate to filter below
}
  \item{excludeConvertible}{
?
}
  \item{scannerSettingPairs}{
?
}
  \item{stockTypeFilter}{
"ALL"?
}
}
\details{
By necessity, design, or otherwise - scanner data is difficult
to correctly use at the API level.  The valid values and some
small examples are returned by the API using the related
\code{reqScannerParameters} function. The XML returned by that
call isn`t very clear in its value or purpose though.
}
\value{
A (potentially) valid twsScannerSubscription object for
\code{reqScannerSubscription} calls.
}
\references{
\url{https://interactivebrokers.github.io/tws-api/classIBApi_1_1ScannerSubscription.html}
}
\author{
Jeffrey A. Ryan
}
\note{
Further documentation will be forthcoming. Users are encouraged to email use cases
to make for better documentation.
}


\seealso{
  \code{\link{reqScannerSubscription}},
}
\examples{
scnr <- twsScannerSubscription(numberOfRows=10,
                               instrument="STK",
                               locationCode="STK.US.MAJOR",
                               scanCode="TOP_PERC_GAIN",
                               aboveVolume=0,
                               marketCapAbove=1e8,
                               scannerSettingPairs="Annual,true",
                               stockTypeFilter="ALL")
scnr
}
\keyword{ class }
