twsSTK <- twsEquity <-
  function(symbol, exch = "SMART", primary = "", strike = "0.0",
             currency = "USD", right = "", local = "", multiplier = "", include_expired = "0", conId = 0) {
    twsContract(conId, symbol, "STK", exch, primary,
      expiry = "", strike,
      currency, right, local, multiplier, NULL, NULL, include_expired
    )
  }

twsCFD <- function(symbol, exch, primary = "", strike = "",
                   currency, right = "", local = "", multiplier = "",
                   include_expired = "0", conId = 0) {
  twsContract(conId, symbol, "CFD", exch, primary,
    expiry = "", strike,
    currency, right, local, multiplier, NULL, NULL, include_expired
  )
}
