`twsOption` <-
function(local,expiry="",
         strike="",right="C",exch="SMART",primary="",
         currency="USD",symbol="",include_expired="0")
{
  twsContract(symbol,"OPT",exch,primary,
              expiry,strike,currency,right,
              local,multiplier="100",NULL,NULL,
              include_expired)
}

`twsOPT` <- twsOption
