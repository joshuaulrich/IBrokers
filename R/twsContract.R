`twsContract` <-
function(symbol,sectype,exch,primary,expiry,strike,
         currency,right,local,multiplier,combo_legs_desc,
         comboleg,include_expired)
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
                 combo_legs_desc=combo_legs_desc,
                 comboleg=comboleg,
                 include_expired=include_expired),
            class='twsContract'
           )
}

