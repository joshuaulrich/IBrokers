\name{twsOrder}
\alias{twsOrder}
\title{
Create twsOrder Object
}
\description{
Create twsOrder object for \code{placeOrder} API call.
}
\usage{
twsOrder(orderId,
         action = "BUY", 
         totalQuantity = "10", 
         orderType = "LMT",
         lmtPrice = "0.0", 
         auxPrice = "0.0",
         tif = "",
         outsideRTH = "0",
         openClose = "O",
         origin = .twsOrderID$CUSTOMER,
         ocaGroup = "",
         account = "",
         orderRef = "",
         transmit = TRUE,
         parentId = "0",
         blockOrder = "0",
         sweepToFill = "0",
         displaySize = "0",
         triggerMethod = "0",
         hidden = "0",
         discretionaryAmt = "0.0",
         goodAfterTime = "",
         goodTillDate = "",
         faGroup = "", 
         faMethod = "",
         faPercentage = "",
         faProfile = "",
         shortSaleSlot = "0",
         designatedLocation = .twsOrderID$EMPTY_STR,
         ocaType = "0",
         rule80A = "",
         settlingFirm = "",
         clearingAccount = "",
         clearingIntent = "",
         allOrNone = "0",
         minQty = "",
         percentOffset = "",
         eTradeOnly = "0",
         firmQuoteOnly = "0",
         nbboPriceCap = "",
         auctionStrategy = "0",
         startingPrice = "",
         stockRefPrice = "",
         delta = "",
         stockRangeLower = "",
         stockRangeUpper = "",
         overridePercentageConstraints = "0",
         volatility = "",
         volatilityType = "",
         deltaNeutralOrderType = "",
         deltaNeutralAuxPrice = "",
         continuousUpdate = "0",
         referencePriceType = "",
         trailStopPrice = "",
         basisPoints = "",
         basisPointsType = "",
         scaleInitLevelSize = "",
         scaleSubsLevelSize = "",
         scalePriceIncrement = "",
         notHeld = FALSE,
         algoStrategy = "",
         algoParams = NULL,
         whatIf = FALSE,
         clientId = "",
         permId = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{orderId}{
The id for the order. Use \code{reqIds}.
}
  \item{action}{
Identifies the side.  ( BUY, SELL, SSHORT )
}
  \item{totalQuantity}{
Order quantity.
}
  \item{orderType}{
Order type. ( MKT, MKTCLS, LMT, LMTCLS, PEGMKT, SCALE, STP, STPLMT, TRAIL, REL, VWAP, TRAILLIMIT )
}
  \item{lmtPrice}{
The \emph{LIMIT} price for LMT, STPLMT and REL \code{orderType}
}
  \item{auxPrice}{
The \emph{STOP} price for STPLMT (stop-limit) orders, and the offset for REL (relative) orders
}
  \item{tif}{
Time in force. (DAY, GTC, IOC, GTD)
}
  \item{outsideRTH}{
Allow orders to trigger outside of regular trading hours.
}
  \item{openClose}{
Specify whether order is open or close only. (Institutional Accounts Only)
}
  \item{origin}{
The order origin. 0=customer, 1=firm (Institutional Accounts Only)
}
  \item{ocaGroup}{
Identifies OCA group.
}
  \item{account}{
The account (Institutional Accounts Only)
}
  \item{orderRef}{
The order reference (Institutional Accounts Only)
}
  \item{transmit}{
Specify whether the order is transmitted to the TWS.  If FALSE, order is created but not
sent. (not implemented)
}
  \item{parentId}{
The orderId of the parent order, used for bracket and auto trailing stop orders.
}
  \item{blockOrder}{
ISE block order?
}
  \item{sweepToFill}{
Sweep to fill order?
}
  \item{displaySize}{
Publicly disclosed order size for Iceberg orders.
}
  \item{triggerMethod}{
How should \emph{simulated} orders be triggered. Valid values are 0-8.
See the official API for details.
}
  \item{hidden}{
Hide order on ISLAND?
}
  \item{discretionaryAmt}{
Amount off limit for discretionary orders.
}
  \item{goodAfterTime}{
Trades Good After Time: YYYYMMDD hh:mm:ss or ""
}
  \item{goodTillDate}{
Trades Good Till Date: YYYYMMDD hh:mm:ss or ""
}
  \item{faGroup}{
NA
}
  \item{faMethod}{
NA
}
  \item{faPercentage}{
NA
}
  \item{faProfile}{
NA
}
  \item{shortSaleSlot}{
1 or 2
}
  \item{designatedLocation}{
Only when \code{shortSaleSlot=2}
}
  \item{ocaType}{
Cancel on Fill with Block = 1
Reduce on Fill with Block = 2
Reduce on Fill without Block = 3
}
  \item{rule80A}{
Valid values: I, A, W, J, U, M, K, Y, N.  See API.
}
  \item{settlingFirm}{
(Institutional Only)
}
  \item{clearingAccount}{
IBExecution customers only.
}
  \item{clearingIntent}{
IBExecution customers only.
}
  \item{allOrNone}{
yes=1, no=0
}
  \item{minQty}{
Minimum quantity order type.
}
  \item{percentOffset}{
Percent offset for REL (relative) orders.
}
  \item{eTradeOnly}{
Trade with electronic quotes. yes=1, no=0.
}
  \item{firmQuoteOnly}{
Trade with firm quotes. yes=1, no=0.
}
  \item{nbboPriceCap}{
The maximum Smart order distance from the NBBO.
}
  \item{auctionStrategy}{
BOX only.  See API.
}
  \item{startingPrice}{
BOX only.  See API.
}
  \item{stockRefPrice}{
The stock reference price.  VOL orders. See API.
}
  \item{delta}{
BOX only.  See API.
}
  \item{stockRangeLower}{
See API.
}
  \item{stockRangeUpper}{
See API.
}
  \item{overridePercentageConstraints}{
See API.
}
  \item{volatility}{
See API.
}
  \item{volatilityType}{
See API.
}
  \item{deltaNeutralOrderType}{
See API.
}
  \item{deltaNeutralAuxPrice}{
See API.
}
  \item{continuousUpdate}{
See API.
}
  \item{referencePriceType}{
See API.
}
  \item{trailStopPrice}{
For TRAILLIMIT orders only.
}
  \item{basisPoints}{
EFP orders only.
}
  \item{basisPointsType}{
EFP orders only.
}
  \item{scaleInitLevelSize}{
For Scale orders.  See API.
}
  \item{scaleSubsLevelSize}{
For Scale orders.  See API.
}
  \item{scalePriceIncrement}{
For Scale orders.  See API.
}
  \item{notHeld}{
See API and guess.
}
  \item{algoStrategy}{
See API and guess.
}
  \item{algoParams}{
See API and guess.
}
  \item{whatIf}{
Use to request pre-trade commissions and margin information. TRUE/FALSE
}
  \item{clientId}{
Id of the client that placed the order.
}
  \item{permId}{
TWS id used to identify orders. Constant over a session.
}
}
\details{
Read the API documentation, code, and experiment with the paper accounts. And good luck!
}
\value{
Called for its side-effects.
}
\references{
Order API:
\url{http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/java/order.htm}
}
\author{
Jeffrey A. Ryan
}
\note{
Documentation is far from complete on this topic. Experiment and share your experiences.
}

\seealso{
\code{\link{placeOrder}}
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
