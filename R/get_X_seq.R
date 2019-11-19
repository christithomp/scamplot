
#' Sequence of Spline Terms
#'
#' @param data - matrix or dataframe containing only spline terms
#' @param ng - length of sequence
#'
#' @return - matrix of sequenced spline terms
#' @export
#'
#' @examples
get_X_seq = function(data, ng){
  # Get number of variables in data
  p = ncol(data)

  # Create empty matrix to store sequences
  X_seq = matrix(0, ng, p)

  # Loop to sequence each spline term
  for (i in 1:p){
    # Sequence each X value from the min to the max ng times
    X_seq[ , i] = seq(min(data[ , i]), max(data[ , i]), length = ng)
  }

  # Return matrix of equences spline terms
  return(var_means)
}
