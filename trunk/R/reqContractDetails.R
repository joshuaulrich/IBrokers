`reqContractDetails` <-
function(conn, Contract, reqId="1", conId="", verbose=FALSE) {
    if(!inherits(conn, "twsConnection")) 
      stop("requires twsConnection object")

    if(!inherits(Contract, "twsContract"))
      stop("requires twsContract object")

    con <- conn[[1]]

    VERSION <- "5"

    writeBin(.twsOutgoingMSG$REQ_CONTRACT_DATA, con)
    writeBin(VERSION, con)
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
          msg <- readBin(con, character(), 18)
        }
        if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
          waiting <- FALSE
        }
    }
    twsContractDetails(contract=list(symbol=msg[3],
                                     sectype=msg[4],
                                     expiry=msg[5],
                                     strike=msg[6],
                                     right=msg[7],
                                     exch=msg[8],
                                     currency=msg[9],
                                     local=msg[10]),
                       marketName=msg[11],
                       tradingClass=msg[12],
                       conId=msg[13],
                       minTick=msg[14],
                       multiplier=msg[15],
                       orderTypes=unlist(strsplit(msg[16],",")),
                       validExchanges=unlist(strsplit(msg[17],",")),
                       priceMagnifier=msg[18])
}
