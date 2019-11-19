
make_scamplot = function(data, y, smooth_terms, linear_terms, shape_type, type, title = "Prediction Plot"){
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

  # Check if shape_type contains string of acceptable values
  shapes = c("cr", "mpi", "mpd", "mdcx", "micx", "mdcv", "micv")
  if (any(!(shape_types %in% shapes))){
    paste("Error: shape_types supplied not supported in scam package")
  }

  # Convert all columns to numeric
  new_data = apply(data, 2, function(x) if(is.character(x)) as.numeric(as.factor(x)) else x)

  #Fit model for new_data
  mdl = get_model(y, smooth_terms, linear_terms, shape_types) # Get model call
  fit = scam::scam(mdl, family = "binomial", data = new_data) # Fit actual model

  mean_seq = get_means(new_data[ , c(smooth_terms, linear_terms)]) # Get means of all covariates
  X_seq = get_X_seq(new_data[ , smooth_terms]) #Get sequence of all smooth terms
  p_seq = length(smooth_terms) # Get number of smooth terms

  for (i in 1:p_seq){
    # Create new data for prediction
    xg = X_seq[ , p_seq] #Get spline term of interest
    x_name = smooth_terms[p_seq] # Identify name of first spline term
    new_mean_seq = mean_seq # Make copy of mean_seq to manipulate
    new_mean_seq$x_name = NULL # Get rid of X column to add X sequence instead

    newdata = data.frame(xg, new_mean_seq) # Create new data for prediction
    Xpred = get_pred(fit, newdata) # Get fHat, ub, and lb for CI

    # Create plot of spline term
    get_plot(xg, Xpred$fHat, Xpred$lb, Xpred$ub, title, xlab = x_name, ylab = y)
  }
}
