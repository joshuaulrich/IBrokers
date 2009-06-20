.reqContractDetails <- function(conn, Contract, reqId="1", conId="")
{
    if(!inherits(conn, "twsConnection")) 
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

`reqContractDetails` <-
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
     
      contracts[[length(contracts)+1]] <- processMsg(curMsg, con, eW, timestamp, file)

      if(curMsg == .twsIncomingMSG$CONTRACT_DATA_END)
        break      
    }
#        if (curMsg == .twsIncomingMSG$ERR_MSG) {
#          if (!errorHandler(con, verbose, OK = c(165, 300, 366, 2104, 2106, 2107))) {
#            cat("\n")
#            stop("Unable to complete ContractDetails request")
#           }
#        } else 
#        if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
#          msg <- c(msg, list(readBin(con, character(), 20+offsetReqId)))
#          if(as.numeric(VERSION) <= 4) waiting <- FALSE
#        } else
#        if (as.numeric(VERSION) > 4 && curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
#          readBin(con, character(), 2)
#          break
#        } else {
#          # catch all the rest.
#          processMsg(curMsg, con, eWrapper, timestamp, file, ...)
#        }
#    }
    return(contracts)
#    lapply(msg, function(msg) {
#    twsContractDetails(version=msg[1],
#                       # reqId = msg[2],
#                       contract=twsContract(symbol=msg[2+offsetReqId],
#                                            sectype=msg[3+offsetReqId],
#                                            expiry=msg[4+offsetReqId],
#                                            primary=msg[20+offsetReqId],
#                                            strike=msg[5+offsetReqId],
#                                            right=msg[6+offsetReqId],
#                                            exch=msg[7+offsetReqId],
#                                            currency=msg[8+offsetReqId],
#                                            multiplier=msg[14+offsetReqId],
#                                            include_expired="",
#                                            combo_legs_desc="", comboleg="",
#                                            local=msg[9+offsetReqId]),
#                       marketName=msg[10+offsetReqId],
#                       tradingClass=msg[11+offsetReqId],
#                       conId=msg[12+offsetReqId],
#                       minTick=msg[13+offsetReqId],
#                       orderTypes=unlist(strsplit(msg[15+offsetReqId],",")),
#                       validExchanges=unlist(strsplit(msg[16+offsetReqId],",")),
#                       priceMagnifier=msg[17+offsetReqId],
#                       underConId=msg[18+offsetReqId],
#                       longName=msg[19+offsetReqId])
#     })
}
