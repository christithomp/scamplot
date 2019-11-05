
get_X_seq = function(data, ng){
  p = ncol(data) # Get number of variables in data

  X_seq = matrix(0, ng, p) # Create empty matrix to store sequences
  for (i in 1:p){
    # Sequence each X value from the min to the max ng times
    X_seq[ , i] = seq(min(data[ , i]), max(data[ , i]), length = ng)
  }
  # Return matrix of means
  return(var_means)
}
