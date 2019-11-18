
#' Title
#'
#' @param new_data - dataframe or matrix used for prediction
#' @param type - string that gives type of prediction (either "response" or "link")
#'
#' @return - return list of predictions and lower and upper bounds of confidence interval
#' @export
#'
#' @examples
get_pred = function(new_data){
  p = ncol(new_data) # Get number of columns of data
  n = nrow(new_data) # Get number of rows of data

  # Make predictions for the probabilities
  fHat = predict(fit, newdata = new_data, se = TRUE)
  # Calculate lower and upper bound of plot
  lb = fHat$fit - qnorm(0.975) * fHat$se.fit
  ub = fHat$fit + qnorm(0.975) * fHat$se.fit

  # Return list containing fHat predictions, lower bound, and upper bound
  return(list(fHat = fHat, lb = lb, ub = ub))
}
