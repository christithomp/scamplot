
#' Create Predicted Values + Upper and Lower Bounds for Confidence Interval
#'
#' @param fit - model used for making predictions
#' @param new_data - dataframe or matrix used for prediction
#'
#' @return - list of predictions and lower and upper bounds of confidence interval
#' @export
#'
#' @examples
#' #Load data
#' data(mtcars)
#'
#' #Create simple regression model
#' fit = stats::lm(mpg ~ wt, data = mtcars)
#'
#' #Create new set of data for prediction
#' new_wt = seq(min(mtcars$wt), max(mtcars$wt))
#' new_data = data.frame(wt = new_wt)
#'
#' #Use get_pred to get predictions and confidence bounds
#' mpg_pred = get_pred(fit, new_data)
#'
#' #Show output for predicted y
#' mpg_pred$fHat
#'
#' #Show output for upper and lower bounds
#' mpg_pred$ub
#' mpg_pred$lb
get_pred = function(fit, new_data){
  # Check if new_data is matrix or dataframe
  if (!is.data.frame(new_data) & !is.matrix(new_data)){
    stop(paste("Error: new_data supplied is not a matrix or dataframe"))
  }
  # Get number of columns of data
  p = ncol(new_data)
  # Get number of rows of data
  n = nrow(new_data)

  # Make predictions for the probabilities
  pred = predict(fit, newdata = new_data, se = TRUE)
  fHat = pred$fit
  # Calculate lower and upper bound of plot
  lb = fHat - qnorm(0.975) * pred$se.fit
  ub = fHat + qnorm(0.975) * pred$se.fit

  # Return list containing fHat predictions, lower bound, and upper bound
  return(list(fHat = fHat, lb = lb, ub = ub))
}
