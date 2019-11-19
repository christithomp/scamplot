
#' Predictive Plot for Model
#'
#' @param x - vector containing spline terms
#' @param fHat - vector of prediction values
#' @param lb- vector containing  lower bound of confidence interval
#' @param ub -vector containing upper bound of confidence interval
#' @param title - string for title of the plot
#' @param x_lab - string for title of x axis
#' @param y_lab - string for title of y axis
#' @param type - string indicating "link" or "response"
#' @return
#' @export
#'
#' @examples
get_plot = function(x, fHat, lb, ub, title, x_lab, y_lab, type){
  p = ncol(data) # Get number of columns of data
  #Function to calculate expit of x
  expit <- function(x) return(1/(1 + exp(-x)))

  plot(0, type = 'n', xlab = x_lab[i], ylab = y_lab, xlim = c(0, 1), ylim = c(0, 1),
       type = "l", lwd = 2, main = title)
  if (type == "link"){
    polygon(c(x, rev(x)), c(lb), rev(ub), col = 'palegreen', border = FALSE)
    lines(x, fHat, col = "darkgreen", lwd = 2)
  }
  else{
    polygon(c(x, rev(x)), c(expit(lb)), rev(expit(ub)), col = 'palegreen', border = FALSE)
    lines(x, expit(fHat), col = "darkgreen", lwd = 2)
  }
}
