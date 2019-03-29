IndexEval = function(plot.diff, index.numb, IC = "AIC", plot.diff.first) {
    switch(IC, AIC = {
    logLik1        = c()
    bw = dpik(plot.diff.first, kernel = "epanech", gridsize = 3201L) 
    for (cv1 in 1:length(plot.diff)){
        eva = function(x) {
          eva_dat = (x - plot.diff.first)/bw 
          erg_dat = c()
          for (i in 1:length(eva_dat)) {
            if (abs(eva_dat[i]) <= sqrt(5)) {
              erg_dat[i] = 3/(4*sqrt(5)) * (1 - (eva_dat[i]^2/5))
            } else {
              erg_dat[i] = 1e-20 # computational 0, used to circumvent the problem of infinity after tacking log
            }
          }
          erg_d = sum(erg_dat)/(bw*length(plot.diff.first)) 
        }
        eva1         = eva(plot.diff[cv1])
        logLik1[cv1] = log(eva1)
    }
    
    logLik                = sum(logLik1)
    aic = 2 * index.numb - 2 * logLik
    return(aic)
    },
    GCV =  {
        gcv = mean(plot.diff^2) / ((1 - index.numb/length(plot.diff))^2)
        return(gcv)
    }, 
    GFCV = {
        gfcv = mean(plot.diff^2) * ((1 + index.numb/length(plot.diff))^2)
        return(gfcv)
    },
    SH = {
        sh = ((length(plot.diff) + 2*index.numb) / (length(plot.diff)^2)) * sum(plot.diff^2)
        return(sh)
    }, 
    Cp = {
        cp_var = try(sum(garchFit(data = plot.diff)@h.t), silent = TRUE)
        if (class(cp_var) == "try-error"){
            cp_var = var(plot.diff)
        }
        cp = (sum(plot.diff^2)/cp_var) - length(plot.diff) + 2 * index.numb
        return(cp)
    }, 
    FPE = {
        fpe = ((length(plot.diff) + index.numb) / (length(plot.diff) + index.numb)) * mean(plot.diff^2)
        return(fpe)
    }
    ) # close switch
}