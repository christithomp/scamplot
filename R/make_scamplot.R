
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

  # Check if type is of the right form
  if (type != "response" | type != "link"){
    paste("Error: type argument supplied is not a string that contains link or response")
  }
  # Check if title is string
  if(!is.string(title)){
    paste("Error: title argument supplied is not a strong")
  }

}
