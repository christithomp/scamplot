
#' Create call to model argument
#'
#' @param y - a string of the name of the predictor variable
#' @param smooth_terms - a vector of strings of the names of the spline terms
#' @param linear_terms - a vector of strings of the names of the linear terms
#'
#' @return - string of call to model
#' @export
#'
#' @examples
#' y = "house_price"
#' smooth_terms = c("sq_ft")
#' linear_terms = c("num_bedrooms", "num_bathrooms")
#'
#' get_model(y, smooth_terms, linear_terms)
get_model = function(y, smooth_terms, linear_terms){
  p_smooth = length(smooth_terms) # get numer of spline terms
  p_linear = length(linear_terms) # get number of linear terms

  # Begin building call to model
  var = paste("s(", smooth_terms[1], ")", sep = "")
  scam_model = paste(y, var, sep = " ~ ")

  # Loop over adding each spline term to model
  if (p_smooth > 1){
    for (i in 2:p_smooth){
      var = paste("s(", smooth_terms[i], ")", sep = "")
      scam_model = paste(scam_model, var, sep = " + ")
    }
  }

  # Loop over adding each linear term to model
  for (i in 1:p_linear){
    var = linear_terms[i]
    scam_model = paste(scam_model, var, sep = " + ")
  }
  # Return string of call to model
  return(scam_model)
}
