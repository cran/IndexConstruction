CRIX = function(market, price, vol = NULL, days.line) {
  IndexComp(market = market, price = price, vol = vol, weighting = "market", weighting.all = "market", IC = "AIC", EvalSeq = "AllTogether", optimum = "local", 
            start.const = 5, steps = 5, fixed.value = NULL, base.value = 1000, 
            derivation.period = 1, derivation.period.ic = 3, 
            days.line = days.line)
}

ECRIX = function(market, price, vol = NULL, days.line) {
  IndexComp(market = market, price = price, vol = vol, weighting = "market", weighting.all = "market", IC = "AIC", EvalSeq = "AllTogether", optimum = "local", 
            start.const = 1, steps = 1, fixed.value = NULL, base.value = 1000, 
            derivation.period = 1, derivation.period.ic = 3, 
            days.line = days.line)
}

EFCRIX = function(market, price, vol = NULL, days.line) {
  IndexComp(market = market, price = price, vol = vol, weighting = "market", weighting.all = "market", IC = "AIC", EvalSeq = "AllTogether", optimum = "global", 
            start.const = 1, steps = 1, fixed.value = NULL, base.value = 1000, 
            derivation.period = 1, derivation.period.ic = 3, 
            days.line = days.line)
}

LCRIX = function(market, price, vol = NULL, days.line) {
  IndexComp(market = market, price = price, vol = vol, weighting = "volume", weighting.all = "volume", IC = "AIC", EvalSeq = "AllTogether", optimum = "local", 
            start.const = 5, steps = 5, fixed.value = NULL, base.value = 1000, 
            derivation.period = 1, derivation.period.ic = 3, 
            days.line = days.line)
}

LECRIX = function(market, price, vol = NULL, days.line) {
  IndexComp(market = market, price = price, vol = vol, weighting = "volume", weighting.all = "volume", IC = "AIC", EvalSeq = "AllTogether", optimum = "local", 
            start.const = 1, steps = 1, fixed.value = NULL, base.value = 1000, 
            derivation.period = 1, derivation.period.ic = 3, 
            days.line = days.line)
}

LEFCRIX = function(market, price, vol = NULL, days.line) {
  IndexComp(market = market, price = price, vol = vol, weighting = "volume", weighting.all = "volume", IC = "AIC", EvalSeq = "AllTogether", optimum = "global", 
            start.const = 1, steps = 1, fixed.value = NULL, base.value = 1000, 
            derivation.period = 1, derivation.period.ic = 3, 
            days.line = days.line)
}
