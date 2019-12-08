
#' Create Formula for Call to Model
#'
#' @param y - a string of the name of the predictor variable
#' @param smooth_terms - a vector of strings of the names of the spline terms
#' @param linear_terms - a vector of strings of the names of the linear terms
#' @param shape_type - a vector of strings containing the shape constraints for the spline (in the same order as smooth_terms). Can only contain shape constraints that the scam function supports
#'
#' @return - string of call to model
#'
#' @examples
#' y = "house_price"
#' smooth_terms = c("sq_ft", "distance_from_work")
#' linear_terms = c("num_bedrooms", "num_bathrooms")
#' shape_type = c("mpi", "mpd")
#'
#' .get_model(y, smooth_terms, linear_terms, shape_type)
.get_model = function(y, smooth_terms, linear_terms, shape_type){
  # Check if shape_type is correct input
  shapes = c("cr", "mpi", "mpd", "mdcx", "micx", "mdcv", "micv")
  if (any(!(shape_type %in% shapes))){
    stop(paste("Error: shape_type supplied not supported in scam package"))
  }
  # Get numer of spline terms
  p_smooth = length(smooth_terms)
  # Get number of linear terms
  p_linear = length(linear_terms)

  # Begin building call to model with first term
  var = paste("s(", smooth_terms[1], ", bs = '", shape_type[1], "')", sep = "")
  # Add new term to model
  scam_model = paste(y, var, sep = " ~ ")

  # Loop over adding each spline term to model
  if (p_smooth > 1){
    for (i in 2:p_smooth){
      # Build call to model formula with other spline terms
      var = paste("s(", smooth_terms[i], ", bs = '", shape_type[i], "')", sep = "")
      # Add new term to model
      scam_model = paste(scam_model, var, sep = " + ")
    }
  }

  # Loop over adding each linear term to model
  for (i in 1:p_linear){
    # Build call to model formula with linear terms
    var = linear_terms[i]
    # Add new term to model
    scam_model = paste(scam_model, var, sep = " + ")
  }
  # Return string of call to model
  return(scam_model)
}
