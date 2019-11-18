
#' Title
#'
#' @param new_data - dataframe or matrix used for prediction
#' @param type - string that gives type of prediction (either "response" or "link")
#'
#' @return - return list of predictions and range of y axis values for plot
#' @export
#'
#' @examples
get_pred = function(new_data, type){
  p = ncol(new_data) # Get number of columns of data
  n = nrow(new_data) # Get number of rows of data

  # Make predictions for the probabilities
  fHat = predict(fit, newdata = new_data, type = type, se = TRUE)
  # Calculate range of y values
  lb = fHat$fit - qnorm(0.975) * fHat$se.fit
  ub = fHat$fit + qnorm(0.975) * fHat$se.fit

  # Return list containing fHat and yrange
  return(list(fHat = fHat, yrange = yrange))
}
