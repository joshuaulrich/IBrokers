`twsContract` <-
function(symbol,sectype,exch,primary,expiry,strike,
         currency,right,local,multiplier,combo_legs_desc,
         comboleg,include_expired)
{
  if(is.null(names(match.call()[-1])))
    return(do.call("twsContract", rep(list(NULL), 13)))

  structure(
            list(symbol=symbol,
                 sectype=sectype,
                 exch=exch,
                 primary=primary,
                 expiry=expiry,
                 strike=strike,
                 currency=currency,
                 right=right,
                 local=local,
                 multiplier=multiplier,
                 combo_legs_desc=combo_legs_desc,
                 comboleg=comboleg,
                 include_expired=include_expired),
            class='twsContract'
           )
}

`print.twsContractDetails` <- `print.twsContract` <-
function(x, ...) {
  str(unclass(x))
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

twsContractDetails <-
function(version=NULL,
         contract=do.call('twsContract',rep(list(NULL),13)),
         marketName=NULL,
         tradingClass=NULL,
         conId=NULL,
         minTick=NULL,
         multiplier=NULL,
         orderTypes=NULL,
         validExchanges=NULL,
         priceMagnifier=NULL
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
                 priceMagnifier=priceMagnifier
                ),
            class='twsContractDetails'
           )
