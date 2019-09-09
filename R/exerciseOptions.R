exerciseOptions <- function(twsconn, contract, exerciseAction = 1,
                            exerciseQuantity = 1, account = "", override = 0, tickerId = 1) {
  if (!is.twsConnection(twsconn)) {
    stop("invalid twsconn")
  }
  if (!is.twsContract(contract)) {
    stop("invalid twsContract")
  }

  # check parameters for valid ranges
  if (!as.numeric(exerciseAction) %in% 1:2) {
    stop("valid exerciseAction is 1 or 2")
  }
  if (!as.numeric(override) %in% 0:1) {
    stop("valid overrride is 0 or 1")
  }

  VERSION <- "1"

  msg <- c(
    .twsOutgoingMSG$EXERCISE_OPTION,
    VERSION,
    tickerId,
    contract$symbol,
    contract$sectype,
    contract$expiry,
    contract$strike,
    contract$multiplier,
    contract$exch,
    contract$currency,
    contract$local,
    exerciseAction,
    exerciseQuantity,
    account,
    override
  )

  writeBin(as.character(msg), twsconn[[1]])
}
