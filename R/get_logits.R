
#' Prediction for Logits
#'
#' @param new_data - dataframe or matrix used for prediction
#'
#' @return
#' @export
#'
#' @examples
get_logits = function(new_data){
  p = ncol(new_data) # Get number of columns of data
  n = nrow(new_data) # Get number of rows of data

  # Make predictions for the logit
  fHat = predict(fit, newdata = new_data, type = "link")
  # Calculate range of y values
  yrange = c(min(fHat), max(fHat))

  # Return list containing fHat and yrange
  return(list(fHat = fHat, yrange = yrange))
}
