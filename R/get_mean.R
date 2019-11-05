
#' Sequence of Variable Means
#'
#' @param data - dataframe or matrix containing all variables in the model
#' @param ng - length of the sequence
#'
#' @return
#' @export
#'
#' @examples
get_mean = function(data, ng){
  p = ncol(data)

  var_means = matrix(0, ng, p)
  for (i in 1:p){
    var_means[ , i] = rep(mean(data[ , i]), ng)
  }
  return(var_means)
}
