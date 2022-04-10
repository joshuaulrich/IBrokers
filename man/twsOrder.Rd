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
         permId = "",
         exemptCode = "-1",
         hedgeType = "",
         hedgeParam = "",
         optOutSmartRouting = FALSE,
         scaleTable = "",
         activeStartTime = "",
         activeStopTime = "",
         trailingPercent = "",
         deltaNeutralConId = "0",
         deltaNeutralSettlingFirm = "",
         deltaNeutralClearingAccount = "",
         deltaNeutralClearingIntent = "",
         deltaNeutralOpenClose = "",
         deltaNeutralShortSale = "0",
         deltaNeutralShortSaleSlot = "0",
         deltaNeutralDesignatedLocation = "",
         scalePriceAdjustValue = "0",
         scalePriceAdjustInterval = "0",
         scaleProfitOffset = "0",
         scaleAutoReset = "0",
         scaleInitPosition = "0",
         scaleInitFillQty = "0",
         scaleRandomPercent = "0",
         smartComboRoutingParams = NULL,
         smartComboRoutingParamsCount = "0",
         orderComboLegs = NULL,
         orderComboLegsCount = "0",
         comboLegs = NULL,
         comboLegsCount = "0",
         orderMiscOptions = NULL
         )
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
  \item{exemptCode}{
Mark order as exempt from short sale uptick rule.
}
  \item{hedgeType}{
For hedge orders. Possible values include: D=delta, B=beta, F=FX, P=Pair
}
  \item{hedgeParam}{
Beta = x for Beta hedge orders, ratio = y for Pair hedge order
}
  \item{optOutSmartRouting}{
Use to opt out of default SmartRouting for orders routed directly to ASX.
This attribute defaults to false unless explicitly set to true. When set to
false, orders routed directly to ASX will NOT use SmartRouting. When set to
true, orders routed directly to ASX orders WILL use SmartRouting.
}
  \item{scaleTable}{
Used for scale orders
}
  \item{activeStartTime}{
for GTC orders
}
  \item{activeStopTime}{
for GTC orders
}
  \item{trailingPercent}{
Specifies the trailing amount of a trailing stop order as a percentage. See
the API docs for guidelines.
}
  \item{deltaNeutralConId}{
See API docs
}
  \item{deltaNeutralSettlingFirm}{
See API docs
}
  \item{deltaNeutralClearingAccount}{
See API docs
}
  \item{deltaNeutralClearingIntent}{
See API docs
}
  \item{deltaNeutralOpenClose}{
Specifies whether the order is an Open or a Close order and is used when the
hedge involves a CFD and and the order is clearing away.
}
  \item{deltaNeutralShortSale}{
Used when the hedge involves a stock and indicates whether or not it is sold
short.
}
  \item{deltaNeutralShortSaleSlot}{
Has a value of 1 (the clearing broker holds shares) or 2 (delivered from a
third party). If you use 2, then you must specify a deltaNeutralDesignatedLocation.
}
  \item{deltaNeutralDesignatedLocation}{
Used only when deltaNeutralShortSaleSlot = 2.
}
  \item{scalePriceAdjustValue}{
For extended Scale orders
}
  \item{scalePriceAdjustInterval}{
For extended Scale orders
}
  \item{scaleProfitOffset}{
For extended Scale orders
}
  \item{scaleAutoReset}{
For extended Scale orders
}
  \item{scaleInitPosition}{
For extended Scale order
}
  \item{scaleInitFillQty}{
For extended Scale orders
}
  \item{scaleRandomPercent}{
For extended Scale orders
}
  \item{smartComboRoutingParams}{
Advanced parameters for Smart combo routing .
}
  \item{smartComboRoutingParamsCount}{
Number of parameters
}
  \item{orderComboLegs}{
List of Per-leg price following the same sequence combo legs are added. The
combo price must be left unspecified when using per-leg prices.
}
  \item{orderComboLegsCount}{
Number of parameters
}
  \item{comboLegs}{
See API docs
}
  \item{comboLegsCount}{
See API docs
}
  \item{orderMiscOptions}{
See API docs
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
\url{https://interactivebrokers.github.io/tws-api/order_management.html}
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
\keyword{utilities}
