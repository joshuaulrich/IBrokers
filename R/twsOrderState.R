`twsOrderState` <-
function(status=NULL,
         initMargin=NULL,
         maintMargin=NULL,
         equityWithLoan=NULL,
         commission=0.0,
         minCommission=0.0,
         maxCommission=0.0,
         commissionCurrency=NULL,
         warningText=NULL
        )
{
  structure(
            list(status=status,
                 initMargin=initMargin,
                 maintMargin=maintMargin,
                 equityWithLoan=equityWithLoan,
                 commission=commission,
                 minCommission=minCommission,
                 maxCommission=maxCommission,
                 commissionCurrency=commissionCurrency,
                 warningText=warningText
                ),
            class='twsOrderState'
           )
}


`print.twsOrderState` <-
function(x, ...) {
  str(unclass(x))
}
