\name{ECRIX}
\alias{ECRIX}

\title{Derivation of the ECRIX index
}
\description{
\code{ECRIX} is a wrapper function for \code{IndexComp} which derives the index according the methodology for ECRIX as described in Trimborn and Haerdle (2018) and visualized on \url{http://thecrix.de/}.
}
\usage{
ECRIX(market, price, vol = NULL, days.line)
}
\arguments{
\item{market}{
An xts object with the market capitalization data. The default is \code{NULL}, an entry is necessary if weighting is set to \code{"market"}.
}
 \item{price}{
An xts object with the price data. An entry is always required.
}
  \item{vol}{
An xts object with the trading volume (liquidity) data. The default is \code{NULL}, an entry is necessary iw weighting is set to \code{"volume"}.
}
\item{days.line}{
The days of the month to perform the recalculation on. Can be calculated from SwitchDates.
}
}
\details{
For more details, please see the methodology section of the paper Trimborn and Haerdle (2018).
}
\value{
A list, entry 1 is the optimal index, entry 2 the index of all constituents, entry 3 the index of all constituents rebased at the index (entry 1) each time after altering the number of index constituents which is useful for comparisons with the market, entry 4 the number of assets available for analysis in each period, entry 5 the absolute weight given to each assets price in the respective periods
}
\references{
Trimborn, S. and Haerdle, W.K. (2018). CRIX an Index for cryptocurrencies, \emph{Journal of Empirical Finance} 49, pp. 107-122. \url{https://doi.org/10.1016/j.jempfin.2018.08.004}
}
\examples{
data(CryptoData)

price = price["2014-03-31::2015-01-31"]
vol = vol["2014-03-31::2015-01-31"]
days.line = SwitchDates(price, specificDate = "1")

ECRIX(market = market, price = price, vol = vol, days.line = days.line)
}