`reqContractDetails` <-
function(conn, Contract, reqId="1", conId="", verbose=FALSE) {
    if(!inherits(conn, "twsConnection")) 
      stop("requires twsConnection object")

    if(!inherits(Contract, "twsContract"))
      stop("requires twsContract object")

    con <- conn[[1]]

    VERSION <- "4"

    writeBin(.twsOutgoingMSG$REQ_CONTRACT_DATA, con)
    writeBin(VERSION, con)
    if(as.numeric(VERSION) > 4)
      writeBin(as.character(reqId),con)
    writeBin(as.character(conId),con)
    writeBin(as.character(Contract$symbol),con)
    writeBin(as.character(Contract$sectype),con)
    writeBin(as.character(Contract$expiry),con)
    writeBin(as.character(Contract$strike),con)
    writeBin(as.character(Contract$right),con)
    writeBin(as.character(Contract$multiplier),con)
    writeBin(as.character(Contract$exch),con)
    writeBin(as.character(Contract$currency),con)
    writeBin(as.character(Contract$local),con)
    writeBin(as.character(Contract$include_expired),con)

    waiting <- TRUE
    while (waiting) {
        curMsg <- readBin(con, character(), 1)
        if (curMsg == .twsIncomingMSG$ERR_MSG) {
          if (!errorHandler(con, verbose, OK = c(165, 300, 366, 2104, 2106, 2107))) {
            cat("\n")
            stop("Unable to complete ContractDetails request")
           }
        }

        if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
          msg <- readBin(con, character(), 17)
          offsetReqId <- 0
          if(as.numeric(VERSION) <= 4) waiting <- FALSE
        }
        if (as.numeric(VERSION) > 4 && curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
          offsetReqId <- 1
          waiting <- FALSE
        }
    }
    twsContractDetails(contract=list(symbol=msg[2+offsetReqId],
                                     sectype=msg[3+offsetReqId],
                                     expiry=msg[4+offsetReqId],
                                     strike=msg[5+offsetReqId],
                                     right=msg[6+offsetReqId],
                                     exch=msg[7+offsetReqId],
                                     currency=msg[8+offsetReqId],
                                     local=msg[9+offsetReqId]),
                       marketName=msg[10+offsetReqId],
                       tradingClass=msg[11+offsetReqId],
                       conId=msg[12+offsetReqId],
                       minTick=msg[13+offsetReqId],
                       multiplier=msg[14+offsetReqId],
                       orderTypes=unlist(strsplit(msg[15+offsetReqId],",")),
                       validExchanges=unlist(strsplit(msg[16+offsetReqId],",")),
                       priceMagnifier=msg[17+offsetReqId])
}
