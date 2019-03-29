\name{SwitchDates}
\alias{SwitchDates}

\title{Deriving the dates on which the index constituents are going to be reevaluated
}
\description{
\code{SwitchDates} derives the dates on which the index constituents are going to be reevaluated. 
}
\usage{
SwitchDates(price, specificDate = NULL, WeekDay = NULL, Appearance = 1)
}
\arguments{
 \item{price}{
An xts object with the price data. An entry is always required.
}
  \item{specificDate}{
A specific date of each month on which the index members get reevaluated. A common date would be the 1st of each month or the 15th of each month. \code{specificDate} is dominating \code{WeekDay}.
}
\item{WeekDay}{
Only active when \code{specificDate} is \code{NULL}. A specific weekday of each month on which the index members get reevaluated. The input has to be a character describing the weekday in Engish. By default the first weekday with this appearancce is returned. The argument \code{Appearance} defines if it is the 1st, 2nd or another appearance of this weekday. E.g. the 3rd Friday of each month can be returned.
}
  \item{Appearance}{
Defines if the 1st, 2nd or another appearance of a weekday gets returned. E.g. the 3rd Friday of each month can be returned. Only active when \code{specificDate} is \code{NULL}. The argument works in combination with \code{WeekDay}.
}
}
\value{
A vector of class date with the respective dates on which the index members become reevaluated. This is a necessary input to \code{IndexComp}.
}
\references{
Trimborn, S. and Haerdle, W.K. (2018). CRIX an Index for cryptocurrencies, \emph{Journal of Empirical Finance} 49, pp. 107-122. \url{https://doi.org/10.1016/j.jempfin.2018.08.004}
}
\examples{
data(CryptoData)

SwitchDates(price, specificDate = "1")
}