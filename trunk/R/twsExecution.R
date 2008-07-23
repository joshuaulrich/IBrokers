`twsExecution` <- 
function(orderId,
         clientId,
         execId,
         time,
         acctNumber,
         exchange,
         side,
         shares,
         price,
         permId,
         liquidation) {

  # special constructor if called with no args
  if(is.null(names(match.call()[-1])))
    return(do.call('twsExecution', rep(list(NULL),11)))

  structure(list(orderId=orderId,
                 clientId=clientId,
                 execId=execId,
                 time=time,
                 acctNumber=acctNumber,
                 exchange=exchange,
                 side=side,
                 shares=shares,
                 price=price,
                 permId=permId,
                 liquidation=liquidation),
            class="twsExecution")

}

