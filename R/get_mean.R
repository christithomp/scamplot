
#' Sequence of Variable Means
#'
#' @param data - dataframe or matrix containing all variables in the model (must be all numeric)
#' @param ng - optional length of the sequence
#'
#' @return - matrix of mean values
#'
#' @examples
#' #Set length of sequence
#' n = 1001
#'
#' #Load data
#' data(mtcars)
#' #Show number of parameters in data
#' ncol(mtcars)
#'
#' #Apply get_means to dataset
#' z = get_mean(mtcars, n)
#' #Show output
#' z
#' #Dimensions is n x (number of parameters)
#' dim(z)
get_mean = function(data, ng = 1001){
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

  # Create empty matrix to store means
  var_means = matrix(0, ng, p)

  # Loop over variables to sequence by mean
  for (i in 1:p){
    # Find mean of each variable and repeat that ng times
    var_means[ , i] = rep(mean(data[ , i]), ng)
  }

  # Return matrix of means
  return(var_means)
}
