twsIndex <-
function (symbol, exch = "", currency = "", local = "") 
{
    twsContract(conId = "", symbol, "IND", exch = exch, primary = "", 
        expiry = "", strike = "", currency = currency, right = "", 
        local = local, multiplier = "", NULL, NULL, include_expired = "")
}
