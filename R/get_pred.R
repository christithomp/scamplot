
#' Create Predicted Values + Upper and Lower Bounds for Confidence Interval
#'
#' @param fit - model used for making predictions
#' @param new_data - dataframe or matrix used for prediction
#' @param type - string that gives type of prediction (either "response" or "link")
#'
#' @return - list of predictions and lower and upper bounds of confidence interval
#' @export
#'
#' @examples
get_pred = function(fit, new_data){
  # Get number of columns of data
  p = ncol(new_data)
  # Get number of rows of data
  n = nrow(new_data)

  # Make predictions for the probabilities
  fHat = predict(fit, newdata = new_data, se = TRUE)
  # Calculate lower and upper bound of plot
  lb = fHat$fit - qnorm(0.975) * fHat$se.fit
  ub = fHat$fit + qnorm(0.975) * fHat$se.fit

  # Return list containing fHat predictions, lower bound, and upper bound
  return(list(fHat = fHat, lb = lb, ub = ub))
}
