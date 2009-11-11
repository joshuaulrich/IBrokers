.reqContractDetails <- function(conn, Contract, reqId="1", conId="")
{
    if(!is.twsConnection(conn))
      stop("requires twsConnection object")

    if(!inherits(Contract, "twsContract"))
      stop("requires twsContract object")

    con <- conn[[1]]

    VERSION <- "5"

    request <- c(.twsOutgoingMSG$REQ_CONTRACT_DATA,
                 VERSION,
                 reqId,
                 conId,
                 Contract$symbol,
                 Contract$sectype,
                 Contract$expiry,
                 Contract$strike,
                 Contract$right,
                 Contract$multiplier,
                 Contract$exch,
                 Contract$currency,
                 Contract$local,
                 Contract$include_expired)
    writeBin(as.character(request), con)            
}

reqContractDetails <-
function(conn, Contract, reqId="1", conId="", verbose=FALSE,
         eventWrapper=eWrapper(), CALLBACK=twsCALLBACK, ...) {

    .reqContractDetails(conn, Contract, reqId, conId)

    if(is.null(CALLBACK))
      invisible(return(NULL))

    # create an internal eWrapper to *only* handle contract request data,
    # all else is discarded.
    eW <- eWrapper(NULL)
    eW$contractDetails <- function(curMsg, msg, timestamp, file, ...) {
      # custom contractData function called from processMsg
      twsContractDetails(version=msg[1],
                         #reqId=msg[2],
                         contract=twsContract(symbol=msg[3],
                                              sectype=msg[4],
                                              expiry=msg[5],  
                                              primary=msg[21],
                                              strike=msg[5+1],
                                              right=msg[6+1],
                                              exch=msg[7+1],
                                              currency=msg[8+1],
                                              multiplier=msg[14+1],
                                              include_expired="",
                                              combo_legs_desc="", comboleg="",
                                              local=msg[9+1]),
                         marketName=msg[10+1],
                         tradingClass=msg[11+1],
                         conId=msg[12+1],
                         minTick=msg[13+1],
                         orderTypes=unlist(strsplit(msg[15+1],",")),
                         validExchanges=unlist(strsplit(msg[16+1],",")),
                         priceMagnifier=msg[17+1],
                         underConId=msg[18+1],
                         longName=msg[19+1])
    }

    contracts <- list()
    con <- conn[[1]]
    while (TRUE) {
      socketSelect(list(con), FALSE, NULL) 
      curMsg <- readBin(con, character(), 1)
      if(curMsg != .twsIncomingMSG$CONTRACT_DATA) {
        processMsg(curMsg, con, eW, timestamp, file)
        if(curMsg == .twsIncomingMSG$CONTRACT_DATA_END)
          break      
      }
      if(curMsg == .twsIncomingMSG$CONTRACT_DATA) {
      contracts[[length(contracts)+1]] <-
        processMsg(curMsg, con, eW, timestamp, file)
      }

    }
    return(contracts)
}
