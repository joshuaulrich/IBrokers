twsContract <-
function (conId = "0", symbol, sectype, exch, primary = "", expiry = "", 
    strike = "0", currency, right, local, multiplier, combo_legs_desc, 
    comboleg, include_expired, secIdType = "", secId = "", tradingClass = "") 
{
    if (is.null(names(match.call()[-1]))) 
        return(do.call("twsContract", rep(list(""), 14)))
    structure(list(conId = conId, symbol = symbol, sectype = sectype, 
        exch = exch, primary = primary, expiry = expiry, strike = strike, 
        currency = currency, right = right, local = local, multiplier = multiplier, 
        combo_legs_desc = combo_legs_desc, comboleg = comboleg, 
        include_expired = include_expired, secIdType = secIdType, 
        secId = secId, tradingClass = tradingClass), class = "twsContract")
}
