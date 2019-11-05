
#' Title
#'
#' @param new_data
#'
#' @return
#' @export
#'
#' @examples
get_logits = function(new_data){
  p = ncol(new_data)
  n = nrow(new_data)

  fHat = matrix(0, n, p)
  yrange = matrix(0, p, 2)

  for(i in 1:p){
    fHat[ , i] = predict(fit, newdata = new_data[ , i], type = "link")
    yrange[i, 1] = min(fHat[ , i])
    yrange[i, 2] = max(fHat[ , i])
  }

  return(list(fHat = fHat, yrange = yrange))
}
