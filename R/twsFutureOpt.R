`twsFutureOpt` <-
  function(symbol, exch, expiry, strike = "", right = "", primary = "",
             currency = "USD", local = "", multiplier = "", include_expired = "0", conId = 0) {
    twsContract(
      conId, symbol, "FOP", exch, primary, expiry, as.character(strike),
      currency, right, local, multiplier, NULL, NULL, include_expired
    )
  }

`twsFOP` <- twsFutureOpt
