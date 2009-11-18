twsExecutionFilter <- function(clientId="0", 
                               acctCode="",
                               time="",
                               symbol="",
                               secType="",
                               exchange="",
                               side="")
{
  structure(list(clientId=clientId,
                 acctCode=acctCode,
                 time=time,
                 symbol=symbol,
                 secType=secType,
                 exchange=exchange,
                 side=side), class="twsExecutionFilter")
}
