
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
  # Get number of variables in data
  p = ncol(data)

  # Create empty matrix to store means
  var_means = matrix(0, ng, p)

  # Loop over variables to sequence by mean
  for (i in 1:p){
    # Find mean of each variable and repeat that ng times
    var_means[ , i] = rep(stats::mean(data[ , i]), ng)
  }

  # Return matrix of means
  return(var_means)
}
