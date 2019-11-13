
get_model = function(y, smooth_terms, linear_terms){
  p_smooth = ncol(smooth_terms) # get numer of spline terms
  p_linear = ncol(linear_terms) # get number of linear terms

  # Begin building call to model
  var = paste("s(", names(smooth_terms)[1], ")", sep = "")
  scam_model = "y ~ var"

  # Loop over adding each spline term to model
  if (p_smooth > 1){
    for (i in 2:p_smooth){
      var = paste("s(", names(smooth_terms)[i], ")", sep = "")
      scam_model = paste(scam_model, var, sep = " + ")
    }
  }

  # Loop over adding each linear term to model
  for (i in 2:p_smooth){
    var = names(linear_terms)[i]
    scam_model = paste(scam_model, var, sep = " + ")
  }
}
