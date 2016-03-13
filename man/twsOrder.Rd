\name{twsOrder}
\alias{twsOrder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twsOrder(orderId, action = "BUY", totalQuantity = "10", orderType = "LMT", lmtPrice = "0.0", auxPrice = "0.0", tif = "", outsideRTH = "0", openClose = "O", origin = .twsOrderID$CUSTOMER, ocaGroup = "", account = "", orderRef = "", transmit = TRUE, parentId = "0", blockOrder = "0", sweepToFill = "0", displaySize = "0", triggerMethod = "0", hidden = "0", discretionaryAmt = "0.0", goodAfterTime = "", goodTillDate = "", faGroup = "", faMethod = "", faPercentage = "", faProfile = "", shortSaleSlot = "0", designatedLocation = .twsOrderID$EMPTY_STR, ocaType = "0", rule80A = "", settlingFirm = "", clearingAccount = "", clearingIntent = "", allOrNone = "0", minQty = "", percentOffset = "", eTradeOnly = "0", firmQuoteOnly = "0", nbboPriceCap = "", auctionStrategy = "0", startingPrice = "", stockRefPrice = "", delta = "", stockRangeLower = "", stockRangeUpper = "", overridePercentageConstraints = "0", volatility = "", volatilityType = "", deltaNeutralOrderType = "", deltaNeutralAuxPrice = "", continuousUpdate = "0", referencePriceType = "", trailStopPrice = "", basisPoints = "", basisPointsType = "", scaleInitLevelSize = "", scaleSubsLevelSize = "", scalePriceIncrement = "", notHeld = FALSE, algoStrategy = "", algoParams = NULL, whatIf = FALSE, clientId = "", permId = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orderId}{
%%     ~~Describe \code{orderId} here~~
}
  \item{action}{
%%     ~~Describe \code{action} here~~
}
  \item{totalQuantity}{
%%     ~~Describe \code{totalQuantity} here~~
}
  \item{orderType}{
%%     ~~Describe \code{orderType} here~~
}
  \item{lmtPrice}{
%%     ~~Describe \code{lmtPrice} here~~
}
  \item{auxPrice}{
%%     ~~Describe \code{auxPrice} here~~
}
  \item{tif}{
%%     ~~Describe \code{tif} here~~
}
  \item{outsideRTH}{
%%     ~~Describe \code{outsideRTH} here~~
}
  \item{openClose}{
%%     ~~Describe \code{openClose} here~~
}
  \item{origin}{
%%     ~~Describe \code{origin} here~~
}
  \item{ocaGroup}{
%%     ~~Describe \code{ocaGroup} here~~
}
  \item{account}{
%%     ~~Describe \code{account} here~~
}
  \item{orderRef}{
%%     ~~Describe \code{orderRef} here~~
}
  \item{transmit}{
%%     ~~Describe \code{transmit} here~~
}
  \item{parentId}{
%%     ~~Describe \code{parentId} here~~
}
  \item{blockOrder}{
%%     ~~Describe \code{blockOrder} here~~
}
  \item{sweepToFill}{
%%     ~~Describe \code{sweepToFill} here~~
}
  \item{displaySize}{
%%     ~~Describe \code{displaySize} here~~
}
  \item{triggerMethod}{
%%     ~~Describe \code{triggerMethod} here~~
}
  \item{hidden}{
%%     ~~Describe \code{hidden} here~~
}
  \item{discretionaryAmt}{
%%     ~~Describe \code{discretionaryAmt} here~~
}
  \item{goodAfterTime}{
%%     ~~Describe \code{goodAfterTime} here~~
}
  \item{goodTillDate}{
%%     ~~Describe \code{goodTillDate} here~~
}
  \item{faGroup}{
%%     ~~Describe \code{faGroup} here~~
}
  \item{faMethod}{
%%     ~~Describe \code{faMethod} here~~
}
  \item{faPercentage}{
%%     ~~Describe \code{faPercentage} here~~
}
  \item{faProfile}{
%%     ~~Describe \code{faProfile} here~~
}
  \item{shortSaleSlot}{
%%     ~~Describe \code{shortSaleSlot} here~~
}
  \item{designatedLocation}{
%%     ~~Describe \code{designatedLocation} here~~
}
  \item{ocaType}{
%%     ~~Describe \code{ocaType} here~~
}
  \item{rule80A}{
%%     ~~Describe \code{rule80A} here~~
}
  \item{settlingFirm}{
%%     ~~Describe \code{settlingFirm} here~~
}
  \item{clearingAccount}{
%%     ~~Describe \code{clearingAccount} here~~
}
  \item{clearingIntent}{
%%     ~~Describe \code{clearingIntent} here~~
}
  \item{allOrNone}{
%%     ~~Describe \code{allOrNone} here~~
}
  \item{minQty}{
%%     ~~Describe \code{minQty} here~~
}
  \item{percentOffset}{
%%     ~~Describe \code{percentOffset} here~~
}
  \item{eTradeOnly}{
%%     ~~Describe \code{eTradeOnly} here~~
}
  \item{firmQuoteOnly}{
%%     ~~Describe \code{firmQuoteOnly} here~~
}
  \item{nbboPriceCap}{
%%     ~~Describe \code{nbboPriceCap} here~~
}
  \item{auctionStrategy}{
%%     ~~Describe \code{auctionStrategy} here~~
}
  \item{startingPrice}{
%%     ~~Describe \code{startingPrice} here~~
}
  \item{stockRefPrice}{
%%     ~~Describe \code{stockRefPrice} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{stockRangeLower}{
%%     ~~Describe \code{stockRangeLower} here~~
}
  \item{stockRangeUpper}{
%%     ~~Describe \code{stockRangeUpper} here~~
}
  \item{overridePercentageConstraints}{
%%     ~~Describe \code{overridePercentageConstraints} here~~
}
  \item{volatility}{
%%     ~~Describe \code{volatility} here~~
}
  \item{volatilityType}{
%%     ~~Describe \code{volatilityType} here~~
}
  \item{deltaNeutralOrderType}{
%%     ~~Describe \code{deltaNeutralOrderType} here~~
}
  \item{deltaNeutralAuxPrice}{
%%     ~~Describe \code{deltaNeutralAuxPrice} here~~
}
  \item{continuousUpdate}{
%%     ~~Describe \code{continuousUpdate} here~~
}
  \item{referencePriceType}{
%%     ~~Describe \code{referencePriceType} here~~
}
  \item{trailStopPrice}{
%%     ~~Describe \code{trailStopPrice} here~~
}
  \item{basisPoints}{
%%     ~~Describe \code{basisPoints} here~~
}
  \item{basisPointsType}{
%%     ~~Describe \code{basisPointsType} here~~
}
  \item{scaleInitLevelSize}{
%%     ~~Describe \code{scaleInitLevelSize} here~~
}
  \item{scaleSubsLevelSize}{
%%     ~~Describe \code{scaleSubsLevelSize} here~~
}
  \item{scalePriceIncrement}{
%%     ~~Describe \code{scalePriceIncrement} here~~
}
  \item{notHeld}{
%%     ~~Describe \code{notHeld} here~~
}
  \item{algoStrategy}{
%%     ~~Describe \code{algoStrategy} here~~
}
  \item{algoParams}{
%%     ~~Describe \code{algoParams} here~~
}
  \item{whatIf}{
%%     ~~Describe \code{whatIf} here~~
}
  \item{clientId}{
%%     ~~Describe \code{clientId} here~~
}
  \item{permId}{
%%     ~~Describe \code{permId} here~~
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
function (orderId, action = "BUY", totalQuantity = "10", orderType = "LMT", 
    lmtPrice = "0.0", auxPrice = "0.0", tif = "", outsideRTH = "0", 
    openClose = "O", origin = .twsOrderID$CUSTOMER, ocaGroup = "", 
    account = "", orderRef = "", transmit = TRUE, parentId = "0", 
    blockOrder = "0", sweepToFill = "0", displaySize = "0", triggerMethod = "0", 
    hidden = "0", discretionaryAmt = "0.0", goodAfterTime = "", 
    goodTillDate = "", faGroup = "", faMethod = "", faPercentage = "", 
    faProfile = "", shortSaleSlot = "0", designatedLocation = .twsOrderID$EMPTY_STR, 
    ocaType = "0", rule80A = "", settlingFirm = "", clearingAccount = "", 
    clearingIntent = "", allOrNone = "0", minQty = "", percentOffset = "", 
    eTradeOnly = "0", firmQuoteOnly = "0", nbboPriceCap = "", 
    auctionStrategy = "0", startingPrice = "", stockRefPrice = "", 
    delta = "", stockRangeLower = "", stockRangeUpper = "", overridePercentageConstraints = "0", 
    volatility = "", volatilityType = "", deltaNeutralOrderType = "", 
    deltaNeutralAuxPrice = "", continuousUpdate = "0", referencePriceType = "", 
    trailStopPrice = "", basisPoints = "", basisPointsType = "", 
    scaleInitLevelSize = "", scaleSubsLevelSize = "", scalePriceIncrement = "", 
    notHeld = FALSE, algoStrategy = "", algoParams = NULL, whatIf = FALSE, 
    clientId = "", permId = "") 
{
    if (missing(orderId)) 
        orderId <- ""
    structure(list(orderId = orderId, clientId = clientId, permId = permId, 
        action = action, totalQuantity = as.character(as.numeric(totalQuantity)), 
        orderType = orderType, lmtPrice = as.character(lmtPrice), 
        auxPrice = as.character(auxPrice), tif = tif, ocaGroup = ocaGroup, 
        ocaType = ocaType, orderRef = orderRef, transmit = as.character(as.integer(transmit)), 
        parentId = parentId, blockOrder = blockOrder, sweepToFill = sweepToFill, 
        displaySize = displaySize, triggerMethod = triggerMethod, 
        outsideRTH = outsideRTH, hidden = hidden, goodAfterTime = goodAfterTime, 
        goodTillDate = goodTillDate, overridePercentageConstraints = overridePercentageConstraints, 
        rule80A = rule80A, allOrNone = allOrNone, minQty = minQty, 
        percentOffset = percentOffset, trailStopPrice = trailStopPrice, 
        faGroup = faGroup, faProfile = faProfile, faMethod = faMethod, 
        faPercentage = faPercentage, openClose = openClose, origin = origin, 
        shortSaleSlot = shortSaleSlot, designatedLocation = designatedLocation, 
        discretionaryAmt = discretionaryAmt, eTradeOnly = eTradeOnly, 
        firmQuoteOnly = firmQuoteOnly, nbboPriceCap = nbboPriceCap, 
        auctionStrategy = auctionStrategy, startingPrice = startingPrice, 
        stockRefPrice = stockRefPrice, delta = delta, stockRangeLower = stockRangeLower, 
        stockRangeUpper = stockRangeUpper, volatility = volatility, 
        volatilityType = volatilityType, continuousUpdate = continuousUpdate, 
        referencePriceType = referencePriceType, deltaNeutralOrderType = deltaNeutralOrderType, 
        deltaNeutralAuxPrice = deltaNeutralAuxPrice, basisPoints = basisPoints, 
        basisPointsType = basisPointsType, scaleInitLevelSize = scaleInitLevelSize, 
        scaleSubsLevelSize = scaleSubsLevelSize, scalePriceIncrement = scalePriceIncrement, 
        account = account, settlingFirm = settlingFirm, clearingAccount = clearingAccount, 
        clearingIntent = clearingIntent, algoStrategy = algoStrategy, 
        algoParams = algoParams, whatIf = as.character(as.integer(whatIf)), 
        notHeld = as.character(as.integer(notHeld))), class = "twsOrder")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
