
#' Title
#'
#' @param x - dataframe or matrix containing spline terms
#' @param fHat - matrix of prediction values
#' @param yrange - matrix containing minimum and maximum for y axis
#' @param title - string for title of the plot
#' @param x_lab - string for title of x axis
#' @param y_lab - string for title of y axis
#' @param color - color of the line on the plot
#'
#' @return
#' @export
#'
#' @examples
get_plot = function(x, fHat, yrange, title, x_lab, y_lab, color){
  p = ncol(data) # Get number of columns of data

  # Plot each variable
  for(i in 1:p){
    plot(x[ ,i], fHat[ ,i], col =  color, xlab = x_lab,
         ylab = y_lab, ylim = c(yrange[i, 1], yrange[i, 2]),
         type = "l", lwd = 2, main = title)
  }
}