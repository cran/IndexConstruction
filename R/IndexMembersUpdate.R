indexMembersUpdate = function(market, price, vol, weighting, index.const, last.value){
  
  if (class(price)[1] != "xts" | class(market)[1] != "xts") {
    stop("The data for 'price' and 'market' have to be in the format 'xts'. Please check the R library 'xts' to convert the data.")
  }
  if (class(vol)[1] != "xts" & is.null(vol) == FALSE) {
    stop("The data for 'vol' have to be in the format 'xts'. Please check the R library 'xts' to convert the data.")
  }
  if ((weighting != "market" & weighting != "volume")) {
    stop("The weighting scheme has to be either 'market' or 'volume'. Please chose either of the two options.")
  }
  if ((weighting == "volume") & is.null(vol)) {
    stop("When weighting by trading volume is chosen, a data entry for volume is required.")
  }
  order_market_list = list()
  
  index_value = last.value
  ##########################################################
  ################ index computation #######################
  ##########################################################
      last_index_period_time   = price
      last_index_period_market = market
      last_index_period_vol = vol
      omit_last = Reduce(intersect, list(which(apply(is.na(
        last_index_period_time), 2, any)
        == F), which(apply(is.na(last_index_period_market), 2, any) == F), 
        which(apply(is.na(last_index_period_vol), 2, any) == F)))
      last_index_period_time   = last_index_period_time[, omit_last]
      last_index_period_market = last_index_period_market[, omit_last]
      last_index_period_vol = last_index_period_vol[, omit_last]
      omit_last_zero           = Reduce(intersect, list(which(apply(
        last_index_period_time == 0, 2, any) == F), which(apply(
          last_index_period_market == 0, 2, any) == F), which(apply(
            last_index_period_vol == 0, 2, any) == F)))
      last_index_period_time   = last_index_period_time[, omit_last_zero]
      last_index_period_market = last_index_period_market[, omit_last_zero]
      last_index_period_vol = last_index_period_vol[, omit_last_zero]

    
    if (dim(last_index_period_market)[2] <= index.const & index.const != "all"){
      return(NULL)
    }
    
    last_line = tail(index(price), n = 1)
    
    last_index_time   = price[last_line]
    last_index_market = market[last_line]
    last_index_vol    = vol[last_line]
    
    if (weighting == "volume") {
      order_market = order(last_index_vol[,colnames(last_index_period_vol)], 
                           decreasing = T)
    } else if (weighting == "market") {
      order_market = order(last_index_market[,colnames(last_index_period_market)], 
                           decreasing = T)
    }
    
      order_market_list = append(order_market_list, list(colnames(
        last_index_period_time[, order_market])))
    
    if (length(order_market) >= index.const){
      order_market = order_market[1:index.const]
    }
    
    index_members_m          = colnames(last_index_period_market)[order_market]
    index_members_t          = colnames(last_index_period_time)[order_market]
    last_index_period_time   = last_index_period_time[, order_market]
    last_index_period_market = last_index_period_market[, order_market]
    last_index_period_vol    = last_index_period_vol[, order_market]
    
    if (weighting == "volume") {
      index_weights = colSums(last_index_period_vol) / colSums(last_index_period_market)
    } else if (weighting == "market") {
      index_weights = rep(1, length(order_market))
    }
    
    if (is.null(dim(last_index_period_market)) == T) {
        divisor = sum(index_weights * tail(last_index_period_market, n = 1)) / index_value
    } else {
        divisor = sum(index_weights * tail(last_index_period_market[, index_members_m], 
                                           n = 1)) / index_value
    }
    
    
    index_old_amount = (last_index_market)[,index_members_m] / 
      (last_index_time)[,index_members_t]

    weight = (index_weights * as.vector(index_old_amount))
    
  list(index_members_m, index_weights, weight, divisor)
}

# end function
