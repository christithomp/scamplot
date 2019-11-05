
get_plot = function(x, fHat, yrange, title, x_lab, y_lab, color){
  p = ncol(data) # Get number of columns of data

  # Plot each variable
  for(i in 1:p){
    plot(x[ ,i], fHat[ ,i], col =  color, xlab = x_lab,
         ylab = y_lab, ylim = c(yrange[i, 1], yrange[i, 2]),
         type = "l", lwd = 2, main = title)
  }
}
