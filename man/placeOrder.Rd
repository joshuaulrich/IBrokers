\name{placeOrder}
\alias{placeOrder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
placeOrder(twsconn, Contract, Order)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsconn}{
%%     ~~Describe \code{twsconn} here~~
}
  \item{Contract}{
%%     ~~Describe \code{Contract} here~~
}
  \item{Order}{
%%     ~~Describe \code{Order} here~~
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
function (twsconn, Contract, Order) 
{
    if (!is.twsConnection(twsconn)) 
        stop("requires twsConnection object")
    if (!is.twsContract(Contract)) 
        stop("requires twsContract object for Contract arg")
    if (!inherits(Order, "twsOrder")) 
        stop("requires twsOrder object for Order arg")
    con <- twsconn[[1]]
    VERSION <- "42"
    if (is.null(Order$hedgeType) | is.null(Order$hedgeParam)) 
        stop(" NEW twsOrder has to be used")
    if (Order$orderId == "") 
        Order$orderId <- reqIds(twsconn)
    order <- c(.twsOutgoingMSG$PLACE_ORDER, VERSION, as.character(Order$orderId), 
        as.character(Contract$conId), Contract$symbol, Contract$sectype, 
        Contract$expiry, Contract$strike, Contract$right, Contract$multiplier, 
        Contract$exch, Contract$primary, Contract$currency, Contract$local, 
        {
            if (is.null(Contract$tradingClass)) "" else Contract$tradingClass
        }, Contract$secIdType, Contract$secId, Order$action, 
        Order$totalQuantity, Order$orderType, Order$lmtPrice, 
        Order$auxPrice, Order$tif, Order$ocaGroup, Order$account, 
        Order$openClose, Order$origin, Order$orderRef, Order$transmit, 
        Order$parentId, Order$blockOrder, Order$sweepToFill, 
        Order$displaySize, Order$triggerMethod, Order$outsideRTH, 
        Order$hidden)
    order <- c(order, "", Order$discretionaryAmt, Order$goodAfterTime, 
        Order$goodTillDate, Order$faGroup, Order$faMethod, Order$faPercentage, 
        Order$faProfile, Order$shortSaleSlot, Order$designatedLocation, 
        Order$exemptCode, Order$ocaType, Order$rule80A, Order$settlingFirm, 
        Order$allOrNone, Order$minQty, Order$percentOffset, Order$eTradeOnly, 
        Order$firmQuoteOnly, Order$nbboPriceCap, Order$auctionStrategy, 
        Order$startingPrice, Order$stockRefPrice, Order$delta, 
        Order$stockRangeLower, Order$stockRangeUpper, Order$overridePercentageConstraints, 
        Order$volatility, Order$volatilityType, Order$deltaNeutralOrderType, 
        Order$deltaNeutralAuxPrice, Order$continuousUpdate, Order$referencePriceType, 
        Order$trailStopPrice, Order$trailingPercent, Order$scaleInitLevelSize, 
        Order$scaleSubsLevelSize, Order$scalePriceIncrement, 
        Order$scaleTable, Order$activeStartTime, Order$activeStopTime)
    if (Order$hedgeType != "") 
        order <- c(order, Order$hedgeType, Order$hedgeParam)
    else order <- c(order, Order$hedgeType)
    order <- c(order, Order$optOutSmartRouting, Order$clearingAccount, 
        Order$clearingIntent, Order$notHeld, "0", Order$algoStrategy, 
        Order$whatIf, "")
    cat("placeOrder VERSION", VERSION, "\n")
    cat(order, "\n", sep = "*")
    writeBin(order, con)
    assign(".Last.orderId", as.integer(Order$orderId), .GlobalEnv)
    invisible(as.integer(Order$orderId))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
