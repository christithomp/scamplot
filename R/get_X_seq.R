
#' Sequence of Spline Terms
#'
#' @param data - matrix or dataframe containing only spline terms
#' @param ng - optional length of sequence
#'
#' @return - matrix of sequenced spline terms
#'
#' @examples
#' #Set length of x sequence
#' n = 100
#'
#' #Load data
#' data(mtcars)
#' #Show number of parameters in data
#' ncol(mtcars)
#'
#' #Apply get_means to dataset
#' z = .get_X_seq(mtcars, n)
#' #Show output
#' z
#' #Dimensions is n x (number of parameters)
#' dim(z)
#' #Show sequenced mpg in z compared to mpg in mtcars
#' sort(mtcars[ , 1]); z[ , 1]
.get_X_seq = function(data, ng = 1001){
  # Check if data is matrix or dataframe
  if (!is.data.frame(data) & !is.matrix(data)){
    stop(paste("Error: data supplied is not a matrix or dataframe"))
  }
  # Check if ng is a number
  if (!is.numeric(ng)){
    stop(paste("Error: ng suplied is not a number"))
  }
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
  return(X_seq)
}
