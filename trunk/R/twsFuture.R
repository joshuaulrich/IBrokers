`twsFuture` <-
function(symbol,exch,expiry,primary='',
         currency='USD',right='',local='',multiplier='',include_expired='0')
{
  twsContract(symbol,'FUT',exch,primary,expiry,strike='0.0',
              currency,right,local,multiplier,include_expired)
}

