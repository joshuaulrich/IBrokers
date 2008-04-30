`twsOption` <-
function(symbol,expiry,strike,right="C",exch,primary="",
         currency="USD",local="",include_expired="0")
{
  twsContract(symbol,"OPT",exch,primary,
              expiry,strike,currency,right,
              local,multiplier="100",NULL,NULL,
              include_expired)
}

