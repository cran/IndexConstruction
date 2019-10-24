\name{relativeWeights}
\alias{relativeWeights}

\title{Retrieving the relative weights of the assets in the index
}
\description{
\code{relativeWeights} retrieves the relative weights of the assets in the index from the absolute weights expressed in shares of the assets. The latter is a direct output of \code{indexComp}.
}
\usage{
relativeWeights(price, index.weights)
}
\arguments{
 \item{price}{
An xts object with the price data. An entry is always required.
}
  \item{index.weights}{
A vector with the absolute weights expressed as number of shares of each asset. The weights are provided by \code{indexComp}. They can be also easily derived from the market capitalization by dividing with the respective price.
}
}
\value{
The relative weights of the assets in the Index.
}
\references{
Trimborn, S. and Haerdle, W.K. (2018). CRIX an Index for cryptocurrencies, \emph{Journal of Empirical Finance} 49, pp. 107-122. \url{https://doi.org/10.1016/j.jempfin.2018.08.004}
}
\examples{
data(CryptoData)

const.names = c("btc", "eth", "xrp", "ltc", "xmr")
index.weights = c(16136712, 88440036, 36856524148, 49589181, 13859864)

relativeWeights(price = price["2017-02-01", const.names], index.weights = index.weights)
}