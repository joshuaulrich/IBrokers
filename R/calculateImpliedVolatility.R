.calculateImpliedVolatility <-
  function(twsconn, Contract, optionPrice, underPrice, reqId = 1) {
    if (!is.twsConnection(twsconn)) {
      stop("invalid tws connection")
    }

    if (!is.twsContract(Contract)) {
      stop("invalid twsContract")
    }

    VERSION <- "1"
    msg <- c(
      .twsOutgoingMSG$REQ_CALC_IMPLIED_VOLAT,
      VERSION,
      as.character(reqId),

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

      as.character(optionPrice),
      as.character(underPrice)
    )

    writeBin(msg, twsconn[[1]])
  }

calculateImpliedVolatility <-
  function(twsconn, Contract, optionPrice, underPrice, reqId = 1) {
    .calculateImpliedVolatility(
      twsconn, Contract, optionPrice,
      underPrice, reqId
    )
    eW <- eWrapper(NULL)
    eW$tickOptionComputation <- function(msg, string, ...) {
      as.numeric(string[4])
    }
    con <- twsconn[[1]]
    while (TRUE) {
      socketSelect(list(con), FALSE, NULL)
      curMsg <- readBin(con, "character", 1)
      msg <- processMsg(curMsg, con, eW, NULL, "")
      if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
        return(msg)
      }
    }
  }
