indexComp = function(market, price, vol = NULL, weighting = "market", weighting.all = "market", ic = "AIC", eval.seq = c("sequential", "all.together"), optimum = c("local", "global"), start.const = 1, steps = 1, fixed.value = NULL, base.value = 1000, derivation.period = 1, derivation.period.ic = 3, 
                     days.line) {
  
  if (class(price)[1] != "xts" | class(market)[1] != "xts") {
    stop("The data for 'price' and 'market' have to be in the format 'xts'. Please check the R library 'xts' to convert the data.")
  }
  if (class(vol)[1] != "xts" & is.null(vol) == FALSE) {
    stop("The data for 'vol' have to be in the format 'xts'. Please check the R library 'xts' to convert the data.")
  }
  if ((weighting != "market" & weighting != "volume") | (weighting.all != "market" & weighting.all != "volume")) {
    stop("The weighting scheme has to be either 'market' or 'volume'. Please chose either of the two options.")
  }
  if ((eval.seq != "sequential" & eval.seq != "all.together")) {
    stop("The evaluation scheme 'eval.seq' has to be either 'sequential' or 'all.together'. Please chose either of the two options.")
  }
  if ((optimum != "local" & optimum != "global")) {
    stop("The optimal point 'optimum' has to be either 'local' or 'global'. Please chose either of the two options.")
  }
  if ((ic != "AIC" & ic != "GCV" & ic != "GFCV" & ic != "SH" & ic != "Cp" & ic != "FPE")) {
    stop("The Information Criterion 'ic' has to be either 'AIC', 'GCV', 'GFCV', 'SH', 'Cp' or 'FPE'. Please chose either of the options.")
  }
  if ((weighting == "volume" | weighting.all == "volume") & is.null(vol)) {
    stop("When weighting by trading volume is chosen, a data entry for volume is required.")
  }
  if (length(days.line) < derivation.period.ic*2) {
    stop(paste("The number of periods to select the number of constituents for is not long enough. ", derivation.period.ic*2, " month of data are required for the derivation. Please provide a longer dataset or decrease the number of month over which the number of constituents shall be derived.", sep = ""))
  }
    index_periods = max(floor(derivation.period.ic / derivation.period), 1)
    numb_aic1     = (length(days.line) - (derivation.period)) / derivation.period.ic
    if ((numb_aic1%%1 == 0) == TRUE){
        numb_aic = numb_aic1 - 1
    } else {
        numb_aic = floor(numb_aic1)
    }
    
    index_comp_numb = seq(start.const, dim(price)[[2]], steps)
    aic_matrix    = matrix(NA, nrow = numb_aic, ncol = length(index_comp_numb))
    aic_compare    = matrix(NA, nrow = numb_aic, ncol = length(index_comp_numb))
    max_coin_numb = c()
    crix = crix_all = crix_all_comp = c()
    plot_diff_values = rep(list(list()), numb_aic)
    cryptos_available = list()
    weights_list = list()
    weights_list2 = list()
    weights_list3 = list()
    
    ### start calculation index
    for (per in 1:numb_aic){
        print(paste(per, " / ", numb_aic, sep = ""))
        
        ### in case no optimization is wished for, just a fixed value index
        if (is.null(fixed.value)) {
        
        ### days to alter the index members for choice of optimal settings
        current_lines         = days.line[seq((1 + derivation.period.ic * 
                                                   (per - 1)), (derivation.period.ic * per + (derivation.period + 1)), 
                                              derivation.period)] + 1
        begin_line            = current_lines[2]
        ### total market index
        index_t_v_all         = index.comp(market = market, price = price, vol = vol, weighting = weighting.all, index.const = "all", base.value = base.value,
                                           index.periods = index_periods, order.derive = TRUE, current.lines.func = current_lines, 
                                           begin.line.func = begin_line, comp1 = FALSE, comp = TRUE, per = per, numb.aic = numb_aic, 
                                           crix = crix, crix.all = crix_all, crix.all.comp = crix_all_comp)
        max_coin_numb[per] = max(sapply(index_t_v_all[[2]], length))
        
        if (eval.seq == "all.together") {
          index_t_v_numb = index.comp(market = market, price = price, vol = vol, weighting = weighting, index.const = index_comp_numb[1], base.value = base.value,
                                      index.periods = index_periods, order.derive = TRUE, current.lines.func = current_lines, 
                                      begin.line.func = begin_line, comp1 = FALSE, comp = TRUE, per = per, numb.aic = numb_aic,
                                      crix = crix, crix.all = crix_all, crix.all.comp = crix_all_comp)
        }

        ### indices with different numbers of constituents
        for (per1 in 1:(length(index_comp_numb)-1)){
          if (eval.seq == "sequential") {
            index_t_v_numb = index.comp(market = market, price = price, vol = vol, weighting = weighting, index.const = index_comp_numb[per1], base.value = base.value,
                                        index.periods = index_periods, order.derive = TRUE, current.lines.func = current_lines,
                                        begin.line.func = begin_line, comp1 = FALSE, comp = TRUE, per = per, numb.aic = numb_aic,
                                        crix = crix, crix.all = crix_all, crix.all.comp = crix_all_comp)
          }

            if (is.null(index_t_v_numb)) {break}
            d = c()
            p = c()
            for (l in 1:length(index_t_v_numb[[2]])) {
                if (l == 1) {
                    for (k in 1:length(index_t_v_numb[[2]])) {
                      if (eval.seq == "sequential") {
                        p[k] = length(which(!is.na(index_t_v_numb[[2]][[k]][(index_comp_numb[per1] + 1):index_comp_numb[per1 + 1]]) == TRUE))
                      } else if (eval.seq == "all.together") {
                        p[k] = length(which(!is.na(index_t_v_numb[[2]][[k]][(index_comp_numb[1] + 1):index_comp_numb[per1 + 1]]) == TRUE))
                      }
                    }
                }
              if (eval.seq == "all.together" && any(p != per1*steps)) {break}
              if (eval.seq == "sequential") {
                a = price[paste(current_lines[l+1]-1, "::", current_lines[l+2] - 1, sep = ""), index_t_v_numb[[2]][[l]][(index_comp_numb[per1] + 1):index_comp_numb[per1 + 1]]]
              } else if (eval.seq == "all.together") {
                a = price[paste(current_lines[l+1]-1, "::", current_lines[l+2] - 1, sep = ""), index_t_v_numb[[2]][[l]][(index_comp_numb[1] + 1):index_comp_numb[per1 + 1]]]
              }
                
                a[a == 0] = NA
                a = na.locf(a, na.rm = FALSE)
                a = na.locf(a, fromLast = TRUE)
                b = diff(log(a))
                # this part is only relevant for sequential
                if (dim(b)[[2]] < max(p)) {
                    while(dim(b)[[2]] < max(p)) {
                        b = cbind(b, 0)
                    }
                }
                #
                colnames(b) = 1:dim(b)[[2]]
                d = rbind(d,b[-1,])
            }
            if (eval.seq == "all.together" && any(p != per1*steps)) {break}
            if (per1 == 1) {
              plot_diff_first = diff(log(c(base.value, index_t_v_all[[1]]))) - diff(log(c(base.value, index_t_v_numb[[1]])))
            }
            plot_diff = diff(log(c(base.value, index_t_v_all[[1]]))) - diff(log(c(base.value, index_t_v_numb[[1]])))
            data_x = d
            erg = lm(plot_diff ~ data_x - 1)
            plot_diff_values[[per]][[per1]] = plot_diff
            
            ### evaluation of current index with an IC method
            aic_compare[per, per1] = IndexEval(plot.diff = erg$residuals, index.numb = length(erg$coefficients), ic = ic, plot.diff.first = plot_diff_first)
            aic_matrix[per, per1] = IndexEval(plot.diff = plot_diff, index.numb = 0, ic = ic, plot.diff.first = plot_diff_first)
            
            if (optimum == "local" && eval.seq == "sequential") {
             #   if (per1 >= 2){
                    if (aic_matrix[per, per1] <= aic_compare[per, per1]){
                        break
                    }
             #   }
            } else if (optimum == "local" && eval.seq == "all.together") {
                 if (per1 >= 2){
                    if (aic_compare[per, per1-1] <= aic_compare[per, per1]){
                      break
                    }
                 } else if (per1 == 1) {
                   if (aic_matrix[per, per1] <= aic_compare[per, per1]){
                     break
                   }
                 }
            }
        }
            
        ### choice of optimal index members
            if (optimum == "local" && eval.seq == "sequential") {
                if (per1 > 1) {
                    index_members = index_comp_numb[per1 - 1]
                } else if (per1 == 1) {
                    index_members = index_comp_numb[per1]
                }
            } else if (optimum == "local" && eval.seq == "all.together") {
                index_members = index_comp_numb[per1]
            } else if (optimum == "global" && eval.seq == "sequential") { # works for sequential and all.together
                index_members = which.min(aic_compare[per,]) * steps
            } else if (optimum == "global" && eval.seq == "all.together") { # works for sequential and all.together
                index_members = which.min(c(aic_matrix[per, 1], aic_compare[per,])) * steps
            }
        
        ### end if which controls fixed_value = NULL
        } else {
            index_members = fixed.value
        }
            
        ### days to alter the index members for application of optimal settings
        current_lines1 = days.line[seq((1 + derivation.period.ic * (per)), 
                                       (derivation.period.ic * (per + 1) + (derivation.period + 1)), 
                                       derivation.period)] + 1
        begin_line1    = current_lines1[2]
        ### calculation of index under the chosen settings, means the chosen number of constituents
        crix1          = index.comp(market = market, price = price, vol = vol, weighting = weighting, index.const = index_members, base.value = base.value,
                                    index.periods = index_periods, order.derive = TRUE, current.lines.func = current_lines1, 
                                    begin.line.func = begin_line1, comp1 = TRUE, comp = TRUE, per = per, numb.aic = numb_aic,
                                    crix = crix, crix.all = crix_all, crix.all.comp = crix_all_comp)
        
        while (is.null(crix1)){
            index_members = index_members - 1
            crix1         = index.comp(market = market, price = price, vol = vol, weighting = weighting, index.const = index_members, base.value = base.value,
                                       index.periods = index_periods, order.derive = TRUE, current.lines.func = current_lines1, 
                                       begin.line.func = begin_line1, comp1 = TRUE, comp = TRUE, per = per, numb.aic = numb_aic,
                                       crix = crix, crix.all = crix_all, crix.all.comp = crix_all_comp)
        }
        crix2     = crix1[[1]]
        weights_list[[per]] = crix1[[4]]
        weights_list2[[per]] = crix1[[5]]
        weights_list3[[per]] = crix1[[7]]
        ### total market index rebased at the total market index
        crix_all2 = index.comp(market = market, price = price, vol = vol, weighting = weighting.all, index.const = "all", base.value = base.value,
                               index.periods = index_periods, order.derive = TRUE, current.lines.func = current_lines1, 
                               begin.line.func = begin_line1, comp1 = TRUE, comp = FALSE, per = per, numb.aic = numb_aic,
                               crix = crix, crix.all = crix_all, crix.all.comp = crix_all_comp)
        cryptos_available[[per]] = crix_all2[[2]]
        
        crix_all1 = crix_all2[[1]]
        ### total market index rebased at the index
        crix_all4 = index.comp(market = market, price = price, vol = vol, weighting = weighting.all, index.const = "all", base.value = base.value,
                               index.periods = index_periods, order.derive = TRUE, current.lines.func = current_lines1, 
                               begin.line.func = begin_line1, comp1 = TRUE, comp = TRUE, per = per, numb.aic = numb_aic,
                               crix = crix, crix.all = crix_all, crix.all.comp = crix_all_comp)
        crix_all3 = crix_all4[[1]]
        if (per == 1){
            crix          = do.call(cbind,list(crix2))
            crix_all      = do.call(cbind,list(crix_all1))
            crix_all_comp = do.call(cbind,list(crix_all3))
        } else {
            crix          = rbind(crix, do.call(cbind, list(crix2)))
            crix_all      = rbind(crix_all, do.call(cbind, list(crix_all1)))
            crix_all_comp = rbind(crix_all_comp, do.call(cbind, list(crix_all3)))
        }
    }
    
    ### output
    print("Call print() for index settings, plot() for plotting of indeces and weights() for displaying the weights of the index constituents. Please see the manual for the structure of the output.")
    res = list(crix, crix_all, crix_all_comp, cryptos_available, weights_list, weights_list3)
    names(res) = c("index", "totalIndex", "totalIndexRebased", "assets", "weights", "weightsRelative")
    input = list(market, price, vol, c(days.line[-c(1:(derivation.period + derivation.period.ic))] + 1))
    names(input) = c("marketCap", "price", "tradingVolume", "daysDerivation")
    output = list(res, input, weighting, weighting.all, ic, eval.seq, optimum, start.const, steps, derivation.period, derivation.period.ic)
    names(output) = c("results", "inputs", "weighting", "weighting.all", "ic", "eval.sep", "optimum", "start.const", "steps", "derivation.period", "derivation.period.ic")
    structure(class = "IndexConstruction", output)
    
}




