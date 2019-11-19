
make_scamplot = function(data, y, smooth_terms, linear_terms, type, title = "Prediction Plot"){
  # Check if y is a column in the data
  if (y %in% colnames(data)){
    paste("Error: y argument supplied is not a variable in the data supplied")
  }
  # Check if smooth_terms are columns in data
  if (any(!(smooth_terms %in% colnames(data)))){
    paste("Error: some of the smooth_terms supplied are not variables in the data supplied")
  }
  # Check if linear_terms are columns in data
  if (any(!(linear_terms %in% colnames(data)))){
    paste("Error: some of the linear_terms supplied are not variables in the data supplied")
  }
  # Check if type is of the right form
  if (type != "response" | type != "link"){
    paste("Error: type argument supplied is not a string that contains either link or response")
  }
  # Check if title is string
  if(!is.string(title)){
    paste("Error: title argument supplied is not a string")
  }

  mean_seq = get_means(data[ , c(smooth_terms, linear_terms)]) # Get means of all covariates
  X_seq = get_X_seq(data[ , smooth_terms]) #Get sequence of all smooth terms
  p_seq = length(smooth_terms) # Get number of smooth terms

  for (i in 1:p_seq){
    x_name = smooth_terms[p_seq] # Identify name of first spline term
    new_mean_seq = mean_seq # Make copy of mean_seq to manipulate
    new_mean_seq$x_name = NULL # Get rid of X column to add X sequence instead
    newdata = data.frame(X_seq[ , p_seq], new_mean_seq) # Create new data for prediction
    Xpred = get_pred(newdata) # Get fHat, ub, and lb for CI
  }
}
