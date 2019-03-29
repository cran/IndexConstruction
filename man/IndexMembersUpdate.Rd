\name{IndexMembersUpdate}
\alias{IndexMembersUpdate}

\title{Reevaluation of Index constituents weights
}
\description{
\code{IndexMembersUpdate} derives the new weights for the coming period. The methodology is according to Trimborn and Haerdle (2018). The method derives the new weights over the data period provided. The data input defines the length of the period, hence it can be different from full month.
}
\usage{
IndexMembersUpdate(market, price, vol, weighting, index.const, last.value)
}
\arguments{
\item{market}{
An xts object with the market capitalization data. The default is \code{NULL}, an entry is necessary if weighting is set to \code{"market"}.
}
 \item{price}{
An xts object with the price data. An entry is always required.
}
  \item{vol}{
An xts object with the trading volume (liquidity) data. The default is \code{NULL}, an entry is necessary if weighting is set to \code{"volume"}.
}
  \item{weighting}{
The weighting scheme to be applied. \code{"market"} refers to weighting by market capitalization, \code{"volume"} refers to weighting by trading volume.
}
  \item{index.const}{Number of Index constituents. The number can be derived from \code{IndexComp}, \code{IndexMemberSelection} or be chosen by alternative means.}
  \item{last.value}{
The last index value before rederivation.
}
}
\details{
\code{IndexMembersUpdate} derives the new weights for the coming period. The methodology is according to Trimborn and Haerdle (2018). The method derives the new weights over the data period provided. The data input defines the length of the period, hence it can be different from full month. For more details, please see the methodology section of the paper Trimborn and Haerdle (2018).
}
\value{
A list, entry 1 is the ordered names of index members, entry 2 the respective consideration of the index constituents, entry 3 the weights of the index members which gives multiplied with entry 2 the actual weight and entry 4 the new divisor of the index.
}
\references{
Trimborn, S. and Haerdle, W.K. (2018). CRIX an Index for cryptocurrencies, \emph{Journal of Empirical Finance} 49, pp. 107-122. \url{https://doi.org/10.1016/j.jempfin.2018.08.004}
}
\examples{
data(CryptoData)

price = price["2017-01-01::2017-01-31"]
market = market["2017-01-01::2017-01-31"]
vol = vol["2017-01-01::2017-01-31"]
IndexMembersUpdate(market = market, price = price, vol = vol, 
weighting = "market", index.const = 5, last.value = 1000)

}