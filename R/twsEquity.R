`twsEquity` <-
function(symbol,exch,primary,strike='0.0',
         currency='USD',right='',local='',multiplier='',include_expired='0')
{
  twsContract(symbol,'STK',exch,primary,expiry='',strike,
              currency,right,local,multiplier,NULL,NULL,include_expired)
}

