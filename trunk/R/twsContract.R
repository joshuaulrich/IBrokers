`twsContract` <-
function(symbol,sectype,exch,primary,expiry,strike,
         currency,right,local,multiplier,include_expired)
{
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
                 include_expired=include_expired),
            class='twsContract'
           )
}

