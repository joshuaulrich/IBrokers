twsFuture <-
function (symbol, exch, expiry, primary = "", currency = "USD", 
    right = "", local = "", multiplier = "", include_expired = "0", 
    conId = 0) 
{
    twsContract(conId, symbol, "FUT", exch, primary, expiry, 
        strike = "0.0", currency, right, local, multiplier, NULL, 
        NULL, include_expired)
}
