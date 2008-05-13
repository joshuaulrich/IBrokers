`twsCurrency` <-
function (symbol,currency="USD",exch="IDEALPRO", primary="", strike = "0.0", 
    right = "", local = "", multiplier = "", include_expired = "0") 
{
    twsContract(symbol, "CASH", exch, primary, expiry = "", strike, 
        currency, right, local, multiplier, NULL, NULL, include_expired)
}

