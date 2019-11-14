
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
  fHat = predict(fit, newdata = new_data, type = type)
  # Calculate range of y values
  yrange = c(min(fHat), max(fHat))

  # Return list containing fHat and yrange
  return(list(fHat = fHat, yrange = yrange))
}
