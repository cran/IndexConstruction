index.comp = function(market, price, vol, weighting, index.const, base.value, index.periods, order.derive, 
                      current.lines.func, begin.line.func, comp, comp1, per, numb.aic, crix, crix.all, crix.all.comp){
    
    index             = list()
    index_members_m   = list()
    index_members_t   = list()
    order_market_list = list()
    weight_list       = list()
    weight_list2       = list()
    divisor_list      = list()
    
    if (per == 1){
        index_value = base.value
    } else if (comp1 == T) {
        if (comp == T){
            index_value = tail(crix[, 1], n = 1)[[1]]
        } else {
            index_value = tail(crix.all[, 1], n = 1)[[1]]
        }
    } else if (comp1 == F){
        index_value = base.value
    }
    ##########################################################
    ############## for loop for index computation ############
    ##########################################################
    for (i in 1:(index.periods)){
        if (i == 1){
            last_index_period_time   = price[paste(current.lines.func[1],(current.lines.func[2] - 1), sep="::")]
            last_index_period_market = market[paste(current.lines.func[1],(current.lines.func[2] - 1), sep="::")]
            last_index_period_vol = vol[paste(current.lines.func[1],(current.lines.func[2] - 1), sep="::")]
            if (is.null(vol) == TRUE | weighting == "market") {
              omit_last = Reduce(intersect, list(which(apply(is.na(
                last_index_period_time), 2, any)
                == F), which(apply(is.na(last_index_period_market), 2, any) == F)))
            } else {
              omit_last = Reduce(intersect, list(which(apply(is.na(
                last_index_period_time), 2, any)
                == F), which(apply(is.na(last_index_period_market), 2, any) == F), 
                which(apply(is.na(last_index_period_vol), 2, any) == F)))
            }
                
            last_index_period_time   = last_index_period_time[, omit_last]
            last_index_period_market = last_index_period_market[, omit_last]
            last_index_period_vol = last_index_period_vol[, omit_last]
            if (is.null(vol) == TRUE | weighting == "market") {
              omit_last_zero           = Reduce(intersect, list(which(apply(
                last_index_period_time == 0, 2, any) == F), which(apply(
                  last_index_period_market == 0, 2, any) == F)))
            } else {
              omit_last_zero           = Reduce(intersect, list(which(apply(
                last_index_period_time == 0, 2, any) == F), which(apply(
                  last_index_period_market == 0, 2, any) == F), which(apply(
                    last_index_period_vol == 0, 2, any) == F)))
            }
            
            last_index_period_time   = last_index_period_time[, omit_last_zero]
            last_index_period_market = last_index_period_market[, omit_last_zero]
            last_index_period_vol = last_index_period_vol[, omit_last_zero]
        } else {
          if (is.null(vol) == TRUE | weighting == "market") {
            omit_now = Reduce(intersect, list(which(apply(is.na(
              index_period_time), 2, any)
              == F), which(apply(is.na(index_period_market), 2, any) == F)))
          } else {
            omit_now = Reduce(intersect, list(which(apply(is.na(
              index_period_time), 2, any)
              == F), which(apply(is.na(index_period_market), 2, any) == F), 
              which(apply(is.na(index_period_vol), 2, any) == F)))
          }
                
            last_index_period_time    = index_period_time[, omit_now]
            last_index_period_market  = index_period_market[, omit_now]
            last_index_period_vol  = index_period_vol[, omit_now]
            
            if (is.null(vol) == TRUE | weighting == "market") {
              omit_now_zero            = Reduce(intersect, list(which(apply(
                index_period_time == 0, 2, any) == F), which(apply(
                  index_period_market == 0, 2, any) == F)))
            } else {
              omit_now_zero            = Reduce(intersect, list(which(apply(
                index_period_time == 0, 2, any) == F), which(apply(
                  index_period_market == 0, 2, any) == F), which(apply(
                    index_period_vol == 0, 2, any) == F)))
            }
            
            last_index_period_time   = index_period_time[, omit_now_zero]
            last_index_period_market = index_period_market[, omit_now_zero]
            last_index_period_vol = index_period_vol[, omit_now_zero]
        }
        
        if (dim(last_index_period_market)[2] <= index.const & index.const != "all"){
            return(NULL)
        }
        
        last_line = if (i == 1){
            begin.line.func
        } else {
            next_line
        } 
        break_loop = FALSE
        if (per == numb.aic & is.na(current.lines.func[i + (2)]) == T) {
            next_line = tail(index(price),n=1) + 1
            break_loop = TRUE
        } else {
            next_line = current.lines.func[i + 2]
        }
        last_index_time   = price[(last_line - 1)]
        last_index_market = market[(last_line - 1)]
        
        if (weighting == "volume") {
          last_index_vol = vol[(last_line - 1)]
            order_market = order(last_index_vol[,colnames(last_index_period_vol)], 
                                 decreasing = T)
        } else if (weighting == "market") {
            order_market = order(last_index_market[,colnames(last_index_period_market)], 
                                 decreasing = T)
        }
        
        if (order.derive == T){
            order_market_list = append(order_market_list, list(colnames(
                last_index_period_time[, order_market])))
        }
        
            if (length(order_market) >= index.const){
                order_market = order_market[1:index.const]
            }
        
        index_members_m[[i]]     = colnames(last_index_period_market)[order_market]
        index_members_t[[i]]     = colnames(last_index_period_time)[order_market]
        last_index_period_time   = last_index_period_time[, order_market]
        last_index_period_market = last_index_period_market[, order_market]
        last_index_period_vol = last_index_period_vol[, order_market]
        
        if (weighting == "volume") {
          index_weights = colSums(last_index_period_vol) / colSums(last_index_period_market)
        } else if (weighting == "market") {
          index_weights = rep(1, length(order_market))
        }
        
        if (is.null(dim(last_index_period_market)) == T) {
            if (i == 1){
                divisor = sum(index_weights * tail(last_index_period_market, n = 1)) / index_value
            } else if (i > 1){
                divisor = sum(index_weights * last_index_market[,index_members_m[[i]]]) / 
                    index[[i - 1]][length(index[[i - 1]])]
            }
        } else {
            if (i == 1){
                divisor = sum(index_weights * tail(last_index_period_market[, index_members_m[[i]]], 
                                   n = 1)) / index_value
            } else if (i > 1){
                divisor = sum(index_weights * last_index_market[,index_members_m[[i]]]) / 
                    index[[i - 1]][length(index[[i - 1]])]
            }
        }
        
        index_period_time   = price[paste(last_line,(next_line - 1),sep="::")]
        index_period_market = market[paste(last_line,(next_line - 1),sep="::")]
        index_period_vol = vol[paste(last_line,(next_line - 1),sep="::")]
        
        index_period_time1   = na.locf(price)[paste(last_line,(next_line - 1),sep="::")]
        
        index_old_amount = (last_index_market)[,index_members_m[[i]]] / 
            (last_index_time)[,index_members_t[[i]]]
        if (is.null(dim(index_period_time1))) {
            index_comp1 = t(t(index_period_time1[index_members_t[[i]]]) * (index_weights * as.vector(index_old_amount)))
            index[[i]] = sum(index_comp1)
        } else {
            index_comp1 = t(t(index_period_time1[, index_members_t[[i]]]) * (index_weights * as.vector(index_old_amount)))
            index[[i]] = apply(index_comp1, 1, sum)
        }
        index[[i]] = index[[i]] / divisor
        
        weight_list[[i]] = (index_weights * as.vector(index_old_amount))
        weight_list2[[i]] = t(t(index_period_time1[, index_members_t[[i]]]) * (index_weights * as.vector(index_old_amount)))
        divisor_list[[i]] = divisor
        
        if (break_loop == T){
            break
        }
    } # end for loop
    
    ################### Index derivation done
    # building whole index time series
    plot_index = c()
    for (i in 1:length(index)){
        plot_index = c(plot_index, index[[i]])
    }
    
    list(plot_index, order_market_list, index_weights, weight_list, weight_list2, divisor_list)
}

# end function
