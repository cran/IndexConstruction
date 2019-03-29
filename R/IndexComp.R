IndexComp = function(market, price, vol = NULL, weighting = "market", weighting.all = "market", IC = "AIC", EvalSeq = c("Sequential", "AllTogether"), optimum = c("local", "global"), start.const = 1, steps = 1, fixed.value = NULL, base.value = 1000, derivation.period = 1, derivation.period.ic = 3, 
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
  if ((EvalSeq != "Sequential" & EvalSeq != "AllTogether")) {
    stop("The evaluation scheme 'EvalSeq' has to be either 'Sequential' or 'AllTogether'. Please chose either of the two options.")
  }
  if ((optimum != "local" & optimum != "global")) {
    stop("The optimal point 'optimum' has to be either 'local' or 'global'. Please chose either of the two options.")
  }
  if ((IC != "AIC" & IC != "GCV" & IC != "GFCV" & IC != "SH" & IC != "Cp" & IC != "FPE")) {
    stop("The Information Criterion 'IC' has to be either 'AIC', 'GCV', 'GFCV', 'SH', 'Cp' or 'FPE'. Please chose either of the options.")
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
        
        if (EvalSeq == "AllTogether") {
          index_t_v_numb = index.comp(market = market, price = price, vol = vol, weighting = weighting, index.const = index_comp_numb[1], base.value = base.value,
                                      index.periods = index_periods, order.derive = TRUE, current.lines.func = current_lines, 
                                      begin.line.func = begin_line, comp1 = FALSE, comp = TRUE, per = per, numb.aic = numb_aic,
                                      crix = crix, crix.all = crix_all, crix.all.comp = crix_all_comp)
        }

        ### indices with different numbers of constituents
        for (per1 in 1:(length(index_comp_numb)-1)){
          if (EvalSeq == "Sequential") {
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
                      if (EvalSeq == "Sequential") {
                        p[k] = length(which(!is.na(index_t_v_numb[[2]][[k]][(index_comp_numb[per1] + 1):index_comp_numb[per1 + 1]]) == TRUE))
                      } else if (EvalSeq == "AllTogether") {
                        p[k] = length(which(!is.na(index_t_v_numb[[2]][[k]][(index_comp_numb[1] + 1):index_comp_numb[per1 + 1]]) == TRUE))
                      }
                    }
                }
              if (EvalSeq == "AllTogether" && any(p != per1*steps)) {break}
              if (EvalSeq == "Sequential") {
                a = price[paste(current_lines[l+1]-1, "::", current_lines[l+2] - 1, sep = ""), index_t_v_numb[[2]][[l]][(index_comp_numb[per1] + 1):index_comp_numb[per1 + 1]]]
              } else if (EvalSeq == "AllTogether") {
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
            if (EvalSeq == "AllTogether" && any(p != per1*steps)) {break}
            if (per1 == 1) {
              plot_diff_first = diff(log(c(base.value, index_t_v_all[[1]]))) - diff(log(c(base.value, index_t_v_numb[[1]])))
            }
            plot_diff = diff(log(c(base.value, index_t_v_all[[1]]))) - diff(log(c(base.value, index_t_v_numb[[1]])))
            data_x = d
            erg = lm(plot_diff ~ data_x - 1)
            plot_diff_values[[per]][[per1]] = plot_diff
            
            ### evaluation of current index with an IC method
            aic_compare[per, per1] = IndexEval(plot.diff = erg$residuals, index.numb = length(erg$coefficients), IC = IC, plot.diff.first = plot_diff_first)
            aic_matrix[per, per1] = IndexEval(plot.diff = plot_diff, index.numb = 0, IC = IC, plot.diff.first = plot_diff_first)
            
            if (optimum == "local" && EvalSeq == "Sequential") {
             #   if (per1 >= 2){
                    if (aic_matrix[per, per1] <= aic_compare[per, per1]){
                        break
                    }
             #   }
            } else if (optimum == "local" && EvalSeq == "AllTogether") {
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
            if (optimum == "local" && EvalSeq == "Sequential") {
                if (per1 > 1) {
                    index_members = index_comp_numb[per1 - 1]
                } else if (per1 == 1) {
                    index_members = index_comp_numb[per1]
                }
            } else if (optimum == "local" && EvalSeq == "AllTogether") {
                index_members = index_comp_numb[per1]
            } else if (optimum == "global" && EvalSeq == "Sequential") { # works for Sequential and AllTogether
                index_members = which.min(aic_compare[per,]) * steps
            } else if (optimum == "global" && EvalSeq == "AllTogether") { # works for Sequential and AllTogether
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
    list(crix, crix_all, crix_all_comp, cryptos_available, weights_list)
}




