\name{indexComp}
\alias{indexComp}

\title{Index derivation for price and liquidity indices
}
\description{
\code{indexComp} derives an Index from the given price and market capitalization or liquidity data. The number of constituents can be fixed or being chosen flexible based on the methodology from Trimborn and Haerdle (2018). This is the main function of the package. The derived index is meant for analysis purposes. For a continuous updating and display of an index on a website, please refer to the remaining functions. 
}
\usage{
indexComp(market, price, vol = NULL, weighting = "market", weighting.all = "market", 
ic = "AIC", eval.seq = c("sequential", "all.together"), 
optimum = c("local", "global"), start.const = 1, steps = 1, fixed.value = NULL, 
base.value = 1000, derivation.period = 1, derivation.period.ic = 3, days.line)
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
\item{weighting.all}{
The weighting scheme to be applied to the full market index. \code{"market"} refers to weighting by market capitalization, \code{"volume"} refers to weighting by trading volume.}
\item{ic}{
Information Criterion to be used for the evaluation of the appropriate index to be used. Possible entries are \code{"AIC"}, \code{"GCV"}, \code{"GFCV"}, \code{"SH"}, \code{"Cp"} and \code{"FPE"}.
}
\item{eval.seq}{
Indicates how the evaluation of the candidate indices by the ic shall be performed. \code{"all.together"} evaluates all indices against each other, \code{"sequential"} evaluates always two consecutive indices against each other.
}
\item{optimum}{
Define how to choose the optimal index. Either a \code{"local"} optimum is chosen, thus the derivation stops the first time the results become worse under the chosen \code{ic}, or a \code{"global"} optimum is chosen, thus all indices are derived and the best fitting one under the \code{ic} is chosen.
}
\item{start.const}{
The number of constituents to start constructing the indices with. The default is \code{1}. 
}
\item{steps}{
The step width for the number of constituents to construct the next index from. The default is \code{1}.
}
\item{fixed.value}{
In case no \code{ic} for the number of constituents for the index shall be applied, give the number of constituents the index shall contain. In that case, \code{"ic"}, \code{"eval.seq"}, \code{"optimum"}, \code{"start.const"} and \code{"steps"} are inactive parameters. The default is \code{NULL}.
}
\item{base.value}{
The starting value for the index. The default is \code{1000}.
}
\item{derivation.period}{
The number of month after which the weights of the index are reallocated. The default is \code{1}.
}
\item{derivation.period.ic}{
The number of month after which the composition of the index is derived again, thus the number of constituents is reevaluated. The default is \code{3}.
}
\item{days.line}{
The days of the month to perform the recalculation on. Can be calculated from switchDates.
}
}
\details{
For more details, please see the methodology section of the paper Trimborn and Haerdle (2018).
}

\value{
  An object of the \code{class} IndexConstruction with the components
  \item{results}{A list containing the results of the model fitting}
  \itemize{
    \item{\code{index} }{The optimal index}
    \item{\code{totalIndex} }{The index of all constituents}
    \item{\code{totalIndexRebased} }{The index of all constituents rebased at the index each time after altering the number of index constituents which is useful for comparisons with the market}
    \item{\code{assets} }{A list containing the assets considered for index construction in each period}
    \item{\code{weights} }{A list containing the weights assigned to the selected index constituents in each period}
    \item{\code{weightsRelative} }{A list containing the relative weights assigned to the selected index constituents in each period}
  }
  \item{inputs}{A list containing the inputs for model fitting}
  \itemize{
  \item{\code{marketCap} }{The provided dataset of the market capitalization of each asset for index construction}
  \item{\code{price} }{The provided dataset of the price series of each asset for index construction}
  \item{\code{tradingVolume} }{The provided dataset of the trading volume of each asset for index construction}
  \item{\code{daysDerivation} }{The provided vector of dates on which to rederive the index weights and number of index constituents}
  }
  \item{weighting}{The selected weighting scheme}
  \item{weighting.all}{The selected weighting.all scheme}
  \item{ic}{The selected ic}
  \item{eval.seq}{The selected eval.seq scheme}
  \item{optimum}{The selected optimization scheme}
  \item{start.const}{The selected number of starting constituents for the index}
  \item{steps}{The selected step size for the selection of the constituents for the index}
  \item{derivation.period}{The selected period for rederivation of the weights of the index constituents}
  \item{derivation.period.ic}{The selected period for rederivation of the number of index constituents}
}

\references{
Trimborn, S. and Haerdle, W.K. (2018). CRIX an Index for cryptocurrencies, \emph{Journal of Empirical Finance} 49, pp. 107-122. \url{https://doi.org/10.1016/j.jempfin.2018.08.004}
}
\examples{
data(CryptoData)

price = price["2014-03-31::2015-01-31"]
market = market["2014-03-31::2015-01-31"]
vol = vol["2014-03-31::2015-01-31"]
days.line = switchDates(price, specificDate = "1")

indexComp(market = market, price = price, vol = vol, weighting = "market", 
weighting.all = "market", ic = "AIC", eval.seq = "sequential", optimum = "local", 
start.const = 5, steps = 5, days.line = days.line)
}