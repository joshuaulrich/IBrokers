`twsCurrency` <-
  function(symbol, currency = "USD", exch = "IDEALPRO", primary = "", strike = "0.0",
             right = "", local = "", multiplier = "", include_expired = "0", conId = 0) {
    twsContract(conId, symbol, "CASH", exch, primary,
      expiry = "", strike,
      currency, right, local, multiplier, NULL, NULL, include_expired
    )
  }

`twsCASH` <- twsCurrency
