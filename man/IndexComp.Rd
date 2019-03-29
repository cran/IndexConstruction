\name{IndexComp}
\alias{IndexComp}

\title{Index derivation for price and liquidity indices
}
\description{
\code{IndexComp} derives an Index from the given price and market capitalization or liquidity data. The number of constituents can be fixed or being chosen based on the methodology from Trimborn and Haerdle (2018).
}
\usage{
IndexComp(market, price, vol = NULL, weighting = "market", weighting.all = "market", 
IC = "AIC", EvalSeq = c("Sequential", "AllTogether"), 
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
An xts object with the trading volume (liquidity) data. The default is \code{NULL}, an entry is necessary iw weighting is set to \code{"volume"}.
}
  \item{weighting}{
The weighting scheme to be applied. \code{"market"} refers to weighting by market capitalization, \code{"volume"} refers to weighting by trading volume.
}
\item{weighting.all}{
The weighting scheme to be applied to the full market index. \code{"market"} refers to weighting by market capitalization, \code{"volume"} refers to weighting by trading volume.}
\item{IC}{
Information Criterion to be used for the evaluation of the appropriate index to be used. Possible entries are \code{"AIC"}, \code{"GCV"}, \code{"GFCV"}, \code{"SH"}, \code{"Cp"} and \code{"FPE"}.
}
\item{EvalSeq}{
Indicates how the evaluation of the candidate indices by the IC shall be performed. \code{"AllTogether"} evaluates all indices against each other, \code{"Sequential"} evaluates always two consecutive indices against each other.
}
\item{optimum}{
Define how to choose the optimal index. Either a \code{"local"} optimum is chosen, thus the derivation stops the first time the results become worse under the chosen IC, or a \code{"global"} optimum is chosen, thus all indices are derived and the best fitting one under the IC is chosen.
}
\item{start.const}{
The number of constituents to start constructing the indices with. The default is \code{1}. 
}
\item{steps}{
The step width for the number of constituents to construct the next index from. The default is \code{1}.
}
\item{fixed.value}{
In case no IC for the number of constituents for the index shall be applied, give the number of constituents the index shall contain. In that case, \code{"IC"}, \code{"EvalSeq"}, \code{"optimum"}, \code{"start.const"} and \code{"steps"} are inactive parameters. The default is \code{NULL}.
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
market = market["2014-03-31::2015-01-31"]
vol = vol["2014-03-31::2015-01-31"]
days.line = SwitchDates(price, specificDate = "1")

IndexComp(market = market, price = price, vol = vol, weighting = "market", 
weighting.all = "market", IC = "AIC", EvalSeq = "Sequential", optimum = "local", 
start.const = 5, steps = 5, days.line = days.line)
}