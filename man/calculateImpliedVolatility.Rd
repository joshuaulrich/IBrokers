\name{calculateImpliedVolatility}
\alias{calculateImpliedVolatility}
\alias{calculateOptionPrice}
\title{
Calculate Option Values
}
\description{
Using the IB API, calculates the implied volatility or option
price given parameters.
}
\usage{
calculateImpliedVolatility(twsconn, 
                           Contract, 
                           optionPrice, 
                           underPrice, 
                           reqId = 1)

calculateOptionPrice(twsconn, 
                     Contract, 
                     volatility, 
                     underPrice, 
                     reqId = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{twsconn}{
A twsConnection object
}
  \item{Contract}{
A twsContract object
}
  \item{optionPrice}{
The option price from which to calculate implied
}
  \item{volatility}{
The volatility from which to calculate price
}
  \item{underPrice}{
The underlying price
}
  \item{reqId}{
The request id
}
}
\details{
Both calls will use the IB described method for calculation.
See the official API for documentation.
}
\value{
A numeric value corresponding to the request
}
\references{
\url{http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/java/calculateimpliedvolatility.htm}
\url{http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/java/calculateoptionprice.htm}
}
\author{
Jeffrey A. Ryan
}


\keyword{ misc }
