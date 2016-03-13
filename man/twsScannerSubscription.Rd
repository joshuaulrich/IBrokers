\name{twsScannerSubscription}
\alias{twsScannerSubscription}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twsScannerSubscription(numberOfRows = -1, instrument = "", locationCode = "", scanCode = "", abovePrice = "", belowPrice = "", aboveVolume = "", averageOptionVolumeAbove = "", marketCapAbove = "", marketCapBelow = "", moodyRatingAbove = "", moodyRatingBelow = "", spRatingAbove = "", spRatingBelow = "", maturityDateAbove = "", maturityDateBelow = "", couponRateAbove = "", couponRateBelow = "", excludeConvertible = "", scannerSettingPairs = "", stockTypeFilter = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numberOfRows}{
%%     ~~Describe \code{numberOfRows} here~~
}
  \item{instrument}{
%%     ~~Describe \code{instrument} here~~
}
  \item{locationCode}{
%%     ~~Describe \code{locationCode} here~~
}
  \item{scanCode}{
%%     ~~Describe \code{scanCode} here~~
}
  \item{abovePrice}{
%%     ~~Describe \code{abovePrice} here~~
}
  \item{belowPrice}{
%%     ~~Describe \code{belowPrice} here~~
}
  \item{aboveVolume}{
%%     ~~Describe \code{aboveVolume} here~~
}
  \item{averageOptionVolumeAbove}{
%%     ~~Describe \code{averageOptionVolumeAbove} here~~
}
  \item{marketCapAbove}{
%%     ~~Describe \code{marketCapAbove} here~~
}
  \item{marketCapBelow}{
%%     ~~Describe \code{marketCapBelow} here~~
}
  \item{moodyRatingAbove}{
%%     ~~Describe \code{moodyRatingAbove} here~~
}
  \item{moodyRatingBelow}{
%%     ~~Describe \code{moodyRatingBelow} here~~
}
  \item{spRatingAbove}{
%%     ~~Describe \code{spRatingAbove} here~~
}
  \item{spRatingBelow}{
%%     ~~Describe \code{spRatingBelow} here~~
}
  \item{maturityDateAbove}{
%%     ~~Describe \code{maturityDateAbove} here~~
}
  \item{maturityDateBelow}{
%%     ~~Describe \code{maturityDateBelow} here~~
}
  \item{couponRateAbove}{
%%     ~~Describe \code{couponRateAbove} here~~
}
  \item{couponRateBelow}{
%%     ~~Describe \code{couponRateBelow} here~~
}
  \item{excludeConvertible}{
%%     ~~Describe \code{excludeConvertible} here~~
}
  \item{scannerSettingPairs}{
%%     ~~Describe \code{scannerSettingPairs} here~~
}
  \item{stockTypeFilter}{
%%     ~~Describe \code{stockTypeFilter} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (numberOfRows = -1, instrument = "", locationCode = "", 
    scanCode = "", abovePrice = "", belowPrice = "", aboveVolume = "", 
    averageOptionVolumeAbove = "", marketCapAbove = "", marketCapBelow = "", 
    moodyRatingAbove = "", moodyRatingBelow = "", spRatingAbove = "", 
    spRatingBelow = "", maturityDateAbove = "", maturityDateBelow = "", 
    couponRateAbove = "", couponRateBelow = "", excludeConvertible = "", 
    scannerSettingPairs = "", stockTypeFilter = "") 
{
    if (missing(scanCode)) 
        warning("'scanCode' needs to be specified")
    structure(list(numberOfRows = numberOfRows, instrument = instrument, 
        locationCode = locationCode, scanCode = scanCode, abovePrice = abovePrice, 
        belowPrice = belowPrice, aboveVolume = aboveVolume, averageOptionVolumeAbove = averageOptionVolumeAbove, 
        marketCapAbove = marketCapAbove, marketCapBelow = marketCapBelow, 
        moodyRatingAbove = moodyRatingAbove, moodyRatingBelow = moodyRatingBelow, 
        spRatingAbove = spRatingAbove, spRatingBelow = spRatingBelow, 
        maturityDateAbove = maturityDateAbove, maturityDateBelow = maturityDateBelow, 
        couponRateAbove = couponRateAbove, couponRateBelow = couponRateBelow, 
        excludeConvertible = excludeConvertible, scannerSettingPairs = scannerSettingPairs, 
        stockTypeFilter = stockTypeFilter), class = "twsScannerSubscription")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
