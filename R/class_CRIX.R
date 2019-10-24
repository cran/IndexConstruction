print.IndexConstruction = function(x, ...){
  cat(strwrap(paste("Index constructed with the following characteristics:")), sep = "\n")
  cat("\n")
  out <- character()
  if (!is.null(x[[3]])) {
    out <- c(out, paste("weighting =", x[[3]]))
  }
  if (!is.null(x[[4]])) {
    out <- c(out, paste("weighting.all =", x[[4]]))
  }
  if (!is.null(x[[5]])) {
    out <- c(out, paste("IC =", x[[5]]))
  }
  if (!is.null(x[[6]])) {
    out <- c(out, paste("EvalSeq =", x[[6]]))
  }
  if (!is.null(x[[7]])) {
    out <- c(out, paste("optimum =", x[[7]]))
  }
  if (!is.null(x[[8]])) {
    out <- c(out, paste("start.const =", x[[8]]))
  }
  if (!is.null(x[[9]])) {
    out <- c(out, paste("steps =", x[[9]]))
  }
  if (!is.null(x[[10]])) {
    out <- c(out, paste("derivation.period =", x[[10]]))
  }
  if (!is.null(x[[11]])) {
    out <- c(out, paste("derivation.period.ic =", x[[11]]))
  }
  cat(paste(out, collapse = "\n"))
  cat("\n")
#  cat(strwrap(paste("Number of index constituents in the respective periods:")), sep = "\n")
#  out = character()
#  for (i in 1:length(x[[1]]$Weights)) {c(out, paste(length(x[[1]]$Weights[[i]][[1]])))}
#  paste(out, collapse = ", ")
#  cat("\n")

  invisible(x)
}

plot.IndexConstruction = function(x, ...) {
  switch(
  menu(c("index", "totalIndex", "totalIndexRebased"), title="Which index shall be plotted? Press 0 for exit."),
  plot(as.xts(x[[1]]$index), type = "l", lwd = 3, main = "Index plot"),
  plot(as.xts(x[[1]]$totalIndex), type = "l", lwd = 3, main = "Index plot"),
  plot(as.xts(x[[1]]$totalIndexRebased), type = "l", lwd = 3, main = "Index plot")
  )
}

weights.IndexConstruction = function(object, ...) {
  counter = 0
  for (i in 1:length(object[[1]]$weightsRelative)) {
    for (j in 1:length(object[[1]]$weightsRelative[[i]])) {
      counter = counter + 1
      print(object[[2]]$daysDerivation[counter])
      print(object[[1]]$weightsRelative[[i]][[j]])
    }
  }
}

