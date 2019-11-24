
#' Predictive Plot for Model
#'
#' @param x - vector containing spline terms
#' @param fHat - vector of prediction values
#' @param lb- vector containing  lower bound of confidence interval
#' @param ub -vector containing upper bound of confidence interval
#' @param title - string for title of the plot
#' @param x_lab - string for title of x axis
#' @param y_lab - string for title of y axis
#' @param pred_type - string indicating "link" or "response"
#' @return None
#' @export
#'
#' @examples
#' # Load required packaged
#' library(mgcv); library(HRW)
#' # Load the data
#' data(BostonMortgages)
#'
#' #Create model for data
#' fit = gam(deny ~ black + s(dir), family = binomial, data = BostonMortgages)
#'
#' #Create data frame for prediction
#' ng = 1001
#' dir_seq = seq(min(BostonMortgages$dir), max(BostonMortgages$dir), length = ng)
#' black = BostonMortgages$black
#' black_seq = rep(unique(black)[which.max(tabulate(match(black, unique(black))))], ng)
#'
#' #Get predicted y values, lower bound, and upper bound of confidence interval
#' newdata = data.frame(dir = dir_seq, black =black_seq)
#' pred_fit = predict(fit, newdata, se.fit = T)
#' fHat = pred_fit$fit
#' lb = fHat - qnorm(0.975) * pred_fit$se.fit
#' ub = fHat + qnorm(0.975) * pred_fit$se.fit
#'
#' #Plot the predictive plot
#' get_plot(dir_seq, fHat, lb, ub, "Prediction of DIR", x_lab = "dir", y_lab = "deny", pred_type = "link")
#' get_plot(dir_seq, fHat, lb, ub, "Prediction of DIR", x_lab = "dir", y_lab = "deny", pred_type = "response")
get_plot = function(x, fHat, lb, ub, title, x_lab, y_lab, pred_type){
  #Check if pred_type is link or response
  if (pred_type != 'type' & pred_type != 'response'){
    stop(paste("Error: pred_type supplied is not 'link' or 'response'"))
  }
  # Get number of columns of data
  p = ncol(data)

  #Function to calculate expit of x
  expit <- function(x) return(1/(1 + exp(-x)))

  # Check if plotting type = link
  if (pred_type == "link"){
    # Creat basic plot template
    plot(0, type = 'n', xlab = x_lab, ylab = y_lab, xlim = c(min(x), max(x)), ylim = c(min(lb), max(ub)),
         lwd = 2, main = title)
    # Create shaded confidence region
    polygon(c(x, rev(x)), c(lb, rev(ub)), col = 'palegreen', border = FALSE)
    # Create line of function shape
    lines(x, fHat, col = "darkgreen", lwd = 2)
  }
  # Check if plotting type = response
  else{
    # Creat basic plot template
    plot(0, type = 'n', xlab = x_lab, ylab = y_lab, xlim = c(min(x), max(x)), ylim = c(0, 1),
         lwd = 2, main = title)
    # Create shaded confidence region
    polygon(c(x, rev(x)), c(expit(lb), rev(expit(ub))), col = 'palegreen', border = FALSE)
    # Create line of function shape
    lines(x, expit(fHat), col = "darkgreen", lwd = 2)
  }
}
