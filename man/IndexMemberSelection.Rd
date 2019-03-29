\name{IndexMemberSelection}
\alias{IndexMemberSelection}

\title{Number of Index Members Derivation
}
\description{
\code{IndexMemberSelection} derives the number of index members for the coming period based on an Information Criterion, e.g. AIC. The methodology is according to Trimborn and Haerdle (2018). The method derives the new weights according to the specifications of the weight reevaluation. The function expects the data period provided to be twice the number of months specified in derivation.period.ic. In case of a mismatch, a warning is given. 
}
\usage{
IndexMemberSelection(market, price, vol, weighting = "market", 
weighting.all = "market", IC = "AIC", EvalSeq = c("Sequential", "AllTogether"), 
optimum = c("local", "global"), start.const = 1, steps = 1, fixed.value = NULL, 
derivation.period = 1, derivation.period.ic = 3, base.value = 1000, days.line)
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
\code{IndexMemberSelection} derives the number of index members for the coming period based on an Information Criterion, e.g. AIC. The methodology is according to Trimborn and Haerdle (2018). The method derives the new weights according to the specifications of the weight reevaluation. The function expects the data period provided to be twice the number of months specified in derivation.period.ic. In case of a mismatch, a warning is given. The data from the first period are used to derived the likelihood, the second period is used for out-of-sample derivation of the number of constituents. Hence for a 3 month reevaluation period, 6 month of data are required by this function. For more details, please see the methodology section of the paper Trimborn and Haerdle (2018).
}
\value{
Returns the number of index members for application in the next period.
}
\references{
Trimborn, S. and Haerdle, W.K. (2018). CRIX an Index for cryptocurrencies, \emph{Journal of Empirical Finance} 49, pp. 107-122. \url{https://doi.org/10.1016/j.jempfin.2018.08.004}
}
\examples{
data(CryptoData)

price = price["2016-07-31::2017-01-31"]
market = market["2016-07-31::2017-01-31"]
vol = vol["2016-07-31::2017-01-31"]
days.line = SwitchDates(price, specificDate = "1")

IndexMemberSelection(market = market, price = price, vol = vol, 
weighting = "market", weighting.all = "market", IC = "AIC", EvalSeq = "Sequential", 
optimum = "local", start.const = 5, steps = 5, days.line = days.line)

}