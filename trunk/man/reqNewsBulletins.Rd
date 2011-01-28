\name{reqNewsBulletins}
\alias{reqNewsBulletins}
\alias{cancelNewsBulletins}
\title{
Subscribe or Unsubscribe To News Bulletins
}
\description{
Subscription start and end methods for the API.
}
\usage{
reqNewsBulletins(twsconn, allMsgs=TRUE)

cancelNewsBulletins(twsconn)
}
\arguments{
  \item{twsconn}{
A twsConnection object
}
  \item{allMsgs}{
Should all existing bulletins be returned (TRUE), or just new ones?
}
}
\details{
Calling reqNewsBulletins will start a subscription via the API. This will
continue and incoming messages will be handled by eWrapper \sQuote{updateNewBulletin}
method. Bulletins are cancelled by calling the cancel version.
}
\value{
Called for its side-effects.
}
\references{
\url{http://www.interactivebrokers.com/php/apiUsersGuide/apiguide/java/reqnewsbulletins.htm}
}
\author{
Jeffrey A. Ryan
}
\note{
This is not \dQuote{news} per se, it is a subscription to the API bulletins.
}
\keyword{ misc }
