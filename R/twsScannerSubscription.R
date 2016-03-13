twsScannerSubscription <-
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
