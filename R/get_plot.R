
#' Predictive Plot for Model
#'
#' @param x - dataframe or matrix containing spline terms
#' @param fHat - matrix of prediction values
#' @param lb- mvector containing  lower bound of confidence interval
#' @param ub -vector containing upper bound of confidence interval
#' @param title - string for title of the plot
#' @param x_lab - string for title of x axis
#' @param y_lab - string for title of y axis
#' @param color - color of the line on the plot
#'
#' @return
#' @export
#'
#' @examples
get_plot = function(x, fHat, lb, ub, title, x_lab, y_lab){
  p = ncol(data) # Get number of columns of data

  # Plot each variable
  for(i in 1:p){
    plot(x[ ,i], fHat[ ,i], xlab = x_lab[i],
         ylab = y_lab, ylim = c(yrange[i, 1], yrange[i, 2]),
         type = "l", lwd = 2, main = title)
  }
}
