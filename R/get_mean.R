
#' Sequence of Variable Means
#'
#' @param data - dataframe or matrix containing all variables in the model
#' @param ng - length of the sequence
#'
#' @return - matrix of mean values
#' @export
#'
#' @examples
get_mean = function(data, ng){
  p = ncol(data) # Get number of variables in data

  var_means = matrix(0, ng, p) # Create empty matrix to store means
  for (i in 1:p){
    # Find mean of each variable and repeat that ng times
    var_means[ , i] = rep(mean(data[ , i]), ng)
  }
  # Return matrix of means
  return(var_means)
}
