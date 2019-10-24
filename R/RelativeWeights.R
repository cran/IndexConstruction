relativeWeights = function(price, index.weights) {
  relative_weights = (price * index.weights) / sum(price * index.weights)
  return(relative_weights)
}