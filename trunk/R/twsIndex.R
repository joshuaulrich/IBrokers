twsIND <- twsIndex <-
function(symbol, exch="")
{
  twsContract(conId="",
              symbol,
              'IND',
              exch=exch,
              primary="",
              expiry="",
              strike="",
              currency="",
              right="",
              local="",
              multiplier="",
              NULL,NULL,
              include_expired="")
}
