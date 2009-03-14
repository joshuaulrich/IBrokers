`reqContractDetails` <-
function(conn, Contract, reqId="1", conId="", verbose=FALSE,
         eventWrapper=eWrapper(), CALLBACK=twsCALLBACK) {
    if(!inherits(conn, "twsConnection")) 
      stop("requires twsConnection object")

    if(!inherits(Contract, "twsContract"))
      stop("requires twsContract object")

    con <- conn[[1]]

    VERSION <- "5"

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

    if(is.null(CALLBACK))
      invisible(return(NULL))

    msg <- list()
    offsetReqId <- ifelse(as.numeric(VERSION) > 4, 1, 0)
    while (TRUE) {
        curMsg <- readBin(con, character(), 1)
        if (curMsg == .twsIncomingMSG$ERR_MSG) {
          if (!errorHandler(con, verbose, OK = c(165, 300, 366, 2104, 2106, 2107))) {
            cat("\n")
            stop("Unable to complete ContractDetails request")
           }
        } else 
        if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
          msg <- c(msg, list(readBin(con, character(), 17+offsetReqId)))
          if(as.numeric(VERSION) <= 4) waiting <- FALSE
        } else
        if (as.numeric(VERSION) > 4 && curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
          readBin(con, character(), 2)
          break
        } else {
          # catch all the rest.
          processMsg(curMsg, con, eWrapper, timestamp, file, ...)
        }
    }
    lapply(msg, function(msg) {
    twsContractDetails(version=msg[1],
                       # reqId = msg[2],
                       contract=twsContract(symbol=msg[2+offsetReqId],
                                            sectype=msg[3+offsetReqId],
                                            expiry=msg[4+offsetReqId],
                                            primary="",
                                            strike=msg[5+offsetReqId],
                                            right=msg[6+offsetReqId],
                                            exch=msg[7+offsetReqId],
                                            currency=msg[8+offsetReqId],
                                            multiplier=msg[14+offsetReqId],
                                            include_expired="",
                                            combo_legs_desc="", comboleg="",
                                            local=msg[9+offsetReqId]),
                       marketName=msg[10+offsetReqId],
                       tradingClass=msg[11+offsetReqId],
                       conId=msg[12+offsetReqId],
                       minTick=msg[13+offsetReqId],
                       orderTypes=unlist(strsplit(msg[15+offsetReqId],",")),
                       validExchanges=unlist(strsplit(msg[16+offsetReqId],",")),
                       priceMagnifier=msg[17+offsetReqId])
     })
}
