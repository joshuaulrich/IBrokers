eWrapper.updateAccount <- function() {
  eW <- eWrapper(NULL)
  eW$assign.Data("PORTFOLIO_VALUE",vector("list"))
  eW$updateAccountValue <- function(curMsg,msg,timestamp,file,...) {
    PV <- eW$get.Data("PORTFOLIO_VALUE")
    PV$version <- as.numeric(msg[1])    

  }
  eW$updatePortfolioValue <- function(curMsg,msg,timestamp,file,...) {
    
  }
}
