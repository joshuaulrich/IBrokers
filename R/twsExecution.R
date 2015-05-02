twsExecution <- function (orderId, clientId, execId, time, acctNumber, exchange, 
    side, shares, price, permId, liquidation, cumQty, avgPrice, orderRef="", evRule="", evMultiplier="") 
{
    if (is.null(names(match.call()[-1]))) 
        #return(do.call("twsExecution", rep(list(NULL), 13)))
        return(do.call("twsExecution", rep(list(NULL), 16)))
    structure(list(orderId = orderId, clientId = clientId, execId = execId, 
        time = time, acctNumber = acctNumber, exchange = exchange, 
        side = side, shares = shares, price = price, permId = permId, 
        liquidation = liquidation, cumQty = cumQty, avgPrice = avgPrice,
        orderRef=orderRef, evRule=evRule, evMultiplier=evMultiplier
        ), 
        class = "twsExecution")
}

