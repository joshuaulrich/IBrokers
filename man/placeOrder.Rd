\name{.placeOrder}
\alias{.placeOrder}
\alias{placeOrder}
\alias{cancelOrder}
\title{
TWS Orders
}
\description{
Place or cancel an order to the TWS.
}
\usage{
placeOrder(twsconn, Contract, Order)

cancelOrder(twsconn, orderId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsconn}{
A twsConnection object.
}
  \item{Contract}{
A twsContract object.
}
  \item{Order}{
A twsOrder object.
}
  \item{orderId}{
A valid order id.
}
}
\details{
As described by the official Interactive Brokers (tm) documentation.  Caveat Emptor!!
}
\value{
Called for its side effect of placing or cancelling an order on the TWS. This also
returns the orderId used for placeOrder.  An additional side-effect is that
a variable .Last.orderId will be created or updated in the GlobalEnv as well.
}
\references{
Official Place Order API:
\url{https://interactivebrokers.github.io/tws-api/classIBApi_1_1EClient.html#aa6ff6f6455c551bef9d66c34d1c8586c}
}
\author{
Jeffrey A. Ryan
}
\note{
Orders via the API are quite complicated, or at least can be.  It is strongly advised to
only proceed with trading real money after one understands not only all the R
code in this package, but the official API as well.  If you are more comfortable
clicking shiny buttons in a GUI, it is probably better that you keep clicking
the buttons and not pretend to program.

Not for the faint of heart. All profits and losses related are yours and
yours alone.  If you don't like it, write it yourself.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{twsContract}}
\code{\link{twsOrder}}
\code{\link{reqIds}}
}
\examples{
\dontrun{
tws <- twsConnect()
id <- reqIds(tws)

placeOrder(tws, twsSTK("AAPL"), twsOrder(id))
cancelOrder(tws, id)
}
}
\keyword{utilities}
