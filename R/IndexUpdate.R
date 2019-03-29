IndexUpdate = function(price, index.weights, divisor) {
  index_value = sum(price * index.weights) / divisor
  return(index_value)
}