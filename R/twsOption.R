twsOption <-
function (local, expiry = "", strike = "", right = "", exch = "SMART", 
    primary = "", currency = "USD", symbol = "", multiplier = "100", 
    include_expired = "0", conId = 0) 
{
    twsContract(conId, symbol, "OPT", exch, primary, expiry, 
        strike, currency, right, local, multiplier, NULL, NULL, 
        include_expired)
}
