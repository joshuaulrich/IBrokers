.calculateOptionPrice <- 
function(twsconn, Contract, volatility, underPrice, reqId=1) {
  if( !is.twsConnection(twsconn))
    stop('invalid tws connection')

  if( !is.twsContract(Contract))
    stop('invalid twsContract')

  VERSION <- "1"
  msg <- c(.twsOutgoingMSG$REQ_CALC_OPTION_PRICE,
           VERSION,
           as.character( reqId),
           
           # contract fields
           Contract$conId,
           Contract$symbol,
           Contract$sectype,
           Contract$expiry,
           Contract$strike,
           Contract$right,
           Contract$multiplier,
           Contract$exch,
           Contract$primary,
           Contract$currency,
           Contract$local,

           as.character( volatility),
           as.character( underPrice))

   writeBin(msg, twsconn[[1]])
}

calculateOptionPrice <- 
function (twsconn, Contract, volatility, underPrice, reqId = 1)  
{
  .calculateOptionPrice(twsconn, Contract, volatility, underPrice, reqId)
  eW <- eWrapper(NULL)
  eW$tickOptionComputation <- function(curMsg, msg, ...) {
    as.numeric(msg[6]) # option price
  }   
  con <- twsconn[[1]]
  while (TRUE) {
    socketSelect(list(con), FALSE, NULL)
    curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
    msg <- processMsg(curMsg, con, eW, NULL, "") 
    if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
        return(msg)
    }   
  }   
}

