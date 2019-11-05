
#' Prediction for Logits
#'
#' @param new_data - dataframe or matrix used for prediction
#'
#' @return
#' @export
#'
#' @examples
get_logits = function(new_data){
  p = ncol(new_data)
  n = nrow(new_data)

  fHat = predict(fit, newdata = new_data, type = "link")
  yrange = c(min(fHat), max(fHat)

  return(list(fHat = fHat, yrange = yrange))
}
