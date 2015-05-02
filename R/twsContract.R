# Added 'tradingClass' slot
twsContract <-
function (conId="0", symbol, sectype, exch, primary="", expiry="", strike="0", 
    currency, right, local, multiplier, combo_legs_desc, comboleg, 
    include_expired, secIdType = "", secId = "", tradingClass="") 
{
    if (is.null(names(match.call()[-1]))) 
        return(do.call("twsContract", rep(list(""), 14)))
    structure(list(conId = conId, symbol = symbol, sectype = sectype, 
        exch = exch, primary = primary, expiry = expiry, strike = strike, 
        currency = currency, right = right, local = local, multiplier = multiplier, 
        combo_legs_desc = combo_legs_desc, comboleg = comboleg, 
        include_expired = include_expired, secIdType = secIdType, 
        secId = secId, tradingClass = tradingClass), class = "twsContract")
}


print.eventPortfolioValue <- `print.twsContractDetails` <- `print.twsContract` <-
function(x, ...) {
  str(unclass(x))
}
print.eventAccountValue <- function(x, ...) {
  print(t(as.data.frame(unclass(x))), quote=FALSE)
}

is.twsContract <- function(x)
{
  inherits(x, 'twsContract')
}

is.twsContractDetails <- function(x)
{
  inherits(x, 'twsContractDetails')
}

as.twsContract <- function(x, ...)
{
  UseMethod("as.twsContract")
}

as.twsContract.list <- function(x, ...)
{
  lapply(x, as.twsContract, ...)
}

as.twsContract.twsContract <- function(x, ...)
{
  x
}

as.twsContract.list <- function(x, ...)
{
  lapply(x, function(xx) {
                if(is.twsContract(xx)) {
                  return(xx)
                } else
                if(is.twsContractDetails(xx)) {
                  return(as.twsContract(xx))
                }})
}

as.twsContract.twsContractDetails <- function(x, ...)
{
  x$contract
}

as.twsContract.twsComboLeg <- function(x) {
  con <- twsContract()
  con$conId <- x$conId
  con$include_expired <- "1"
  con
}

twsContractDetails <-
function(version=NULL,
         contract=twsContract(),
         marketName=NULL,
         tradingClass=NULL,
         conId=NULL,
         minTick=NULL,
         multiplier=NULL,
         orderTypes=NULL,
         validExchanges=NULL,
         priceMagnifier=NULL,
         underConId=NULL,
         longName=NULL,
         contractMonth=NULL,
         industry=NULL,
         category=NULL,
         subcategory=NULL,
         timeZoneId=NULL,
         tradingHours=NULL,
         liquidHours=NULL
         )
  structure(
            list(version=version,
                 contract=contract,
                 marketName=marketName,
                 tradingClass=tradingClass,
                 conId=conId,
                 minTick=minTick,
                 orderTypes=orderTypes,
                 validExchanges=validExchanges,
                 priceMagnifier=priceMagnifier,
                 underConId=underConId,
                 longName=longName,
                 contractMonth=contractMonth,
                 industry=industry,
                 category=category,
                 subcategory=subcategory,
                 timeZoneId=timeZoneId,
                 tradingHours=tradingHours,
                 liquidHours=liquidHours
                ),
            class='twsContractDetails'
           )
