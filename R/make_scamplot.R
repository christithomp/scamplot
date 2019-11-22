
#' Create Prediction Plot of SCAM
#'
#' @param data - data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param y - string containing name of depednent variable in model
#' @param smooth_terms - vector of strings containing names of independent variables to be fit as splines in the model
#' @param linear_terms - vector of strings containing names of independent variables to be fit as linear terms in the model
#' @param shape_type - vector of strings containing the shape constraints for the spline (in the same order as smooth_terms). Can only contain shape constraints that the scam function supports
#' @param type - string indicating "link" or "response"
#' @param title - optional string containing title of plot
#'
#' @return
#' @export
#'
#' @examples
#' #Load the packages required
#' library(HRW)
#'
#' #Load the data
#' data(BostonMortgages)
#'
#' #Assign variables to be supplied to function
#' y = "deny"
#' smooth_terms = c("dir", "lvr")
#' BostonMortgages$ccs = as.factor(BostonMortgages$ccs)
#' linear_terms = c("ccs", "black", "pbcr", "self", "single")
#' shape_type = c("cr", "cr")
#' type = "link"
#'
#' make_scamplot(BostonMortgages, y, smooth_terms, linear_terms, shape_type, type)
#'
make_scamplot = function(data, y, smooth_terms, linear_terms, shape_type, type, title = "Prediction Plot"){
  # Check if y is a column in the data
  if (y %in% colnames(data)){
    stop(paste("Error: y argument supplied is not a variable in the data supplied"))
  }
  # Check if smooth_terms are columns in data
  if (any(!(smooth_terms %in% colnames(data)))){
    stop(paste("Error: some of the smooth_terms supplied are not variables in the data supplied"))
  }
  # Check if linear_terms are columns in data
  if (any(!(linear_terms %in% colnames(data)))){
    stop(paste("Error: some of the linear_terms supplied are not variables in the data supplied"))
  }
  # Check if type is of the right form
  if (type != "response" & type != "link"){
    stop(paste("Error: type argument supplied is not a string that contains either link or response"))
  }
  # Check if title is string
  if(!is.string(title)){
    stop(paste("Error: title argument supplied is not a string"))
  }

  # Check if shape_type contains string of acceptable values
  shapes = c("cr", "mpi", "mpd", "mdcx", "micx", "mdcv", "micv")
  if (any(!(shape_types %in% shapes))){
    stop(paste("Error: shape_types supplied not supported in scam package"))
  }

  # Convert all columns to numeric
  new_data = apply(data, 2, function(x) if(is.character(x)) as.numeric(as.factor(x)) else x)

  #Fit model for new_data

  # Get model call
  mdl = get_model(y, smooth_terms, linear_terms, shape_types)
  # Fit actual model
  fit = scam::scam(mdl, family = "binomial", data = new_data)

  # Get means of all covariates
  mean_seq = get_means(new_data[ , c(smooth_terms, linear_terms)])
  #Get sequence of all smooth terms
  X_seq = get_X_seq(new_data[ , smooth_terms])
  # Get number of smooth terms
  p_seq = length(smooth_terms)

  for (i in 1:p_seq){
    # Create new data for prediction

    #Get spline term of interest
    xg = X_seq[ , p_seq]
    # Identify name of first spline term
    x_name = smooth_terms[p_seq]
    # Make copy of mean_seq to manipulate
    new_mean_seq = mean_seq
    # Get rid of X column to add X sequence instead
    new_mean_seq$x_name = NULL

    # Create new data for prediction
    newdata = data.frame(xg, new_mean_seq)
    # Get fHat, ub, and lb for CI
    Xpred = get_pred(fit, newdata)

    # Create plot of spline term
    get_plot(xg, Xpred$fHat, Xpred$lb, Xpred$ub, title, xlab = x_name, ylab = y, type)
  }
}
