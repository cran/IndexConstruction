\name{indexUpdate}
\alias{indexUpdate}

\title{Updating an existing index with new index values
}
\description{
\code{indexUpdate} derives the next values of an Index from the given price, weights and its divisor. This function is meant for continuous updating and display of an index on a website. For the derivation of an index for analysis purposes, please refer to the function \code{"indexComp"}.
}
\usage{
indexUpdate(price, index.weights, divisor)
}
\arguments{
 \item{price}{
An xts object with the price data. An entry is always required.
}
  \item{index.weights}{
A vector with the absolute weights expressed as number of shares of each asset. The weights are provided by \code{indexComp}. They can be also easily derived from the market capitalization by dividing with the respective price.
}
  \item{divisor}{
The divisor required for the index derivation. The divisor is provided by \code{indexComp}. For details on its derivation, see Trimborn and Haerdle (2018).
}
}
\details{
For more details, please see the methodology section of the paper Trimborn and Haerdle (2018).
}
\value{
The next value(s) of the Index.
}
\references{
Trimborn, S. and Haerdle, W.K. (2018). CRIX an Index for cryptocurrencies, \emph{Journal of Empirical Finance} 49, pp. 107-122. \url{https://doi.org/10.1016/j.jempfin.2018.08.004}
}
\examples{
data(CryptoData)

const.names = c("btc", "eth", "xrp", "ltc", "xmr")
index.weights = c(16136712, 88440036, 36856524148, 49589181, 13859864)
divisor = 17185084

indexUpdate(price["2017-02-01", const.names], index.weights = index.weights, divisor = divisor)
}