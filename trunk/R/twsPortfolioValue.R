twsPortfolioValue <- function(x, zero.pos=TRUE, ...) {
  UseMethod("twsPortfolioValue")
}

twsPortfolioValue.eWrapper <- function(x, zero.pos=TRUE, ...) {
  # to extract the .Portfolio data element from
  # eWrappers in future releases
}

twsPortfolioValue.AccountUpdate <- function(x, zero.pos=TRUE, ...) {
  portf <- do.call(rbind,
                   lapply(x[[2]], function(x) 
                   data.frame(local=x$contract$local,
                              sectype=x$contract$sectype,
                              marketValue=x[[2]]$marketValue, 
                              averageCost=x[[2]]$averageCost*x[[2]]$position,
                              return=with(x[[2]],marketValue/(averageCost*position)-1),
                              position=x[[2]]$position, 
                              realizedPNL=x[[2]]$realizedPNL,
                              unrealizedPNL=x[[2]]$unrealizedPNL)))
  if(!zero.pos)
    portf[which(portf$position != 0),]
  else
    portf
}

summary.AccountUpdate <- function(object, ...) {
  twsPortfolioValue.AccountUpdate(object, ...)
}

