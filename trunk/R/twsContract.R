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


`twsContractDetails` <-
function(symbol,sectype,expiry,right,exch,
         currency,local,cusip,maturity,issue_date,
         ratings,bond_type,coupon_type,desc_append,
         next_option_date,next_option_type,notes,
         market_name, trading_class, multiplier,
         order_types, valid_exchanges)
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
