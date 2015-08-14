# Leeds-thesis
x <- pip_raw$t_sun
y <- unique (pip_raw$period)
UniqueVecDiff <- function (x, y) { # function UniqueVecDiff that takes two arguments x and y
  as.factor(y) # set the vector y as a factor
  for (i in 1: length (x)) { # for loop, stating for each value of x
    if(unique(y) > 1){  # if the unique values of y are more than one row, since y is a factor ## this line gives an error    #message that the condition is lenght of 1 so first value used 
    xdiff <- diff(x, lag = 1, differences = 1) # calculates the differences between each value in vector x
    pip_diff <- cbind (pip_raw, xdiff)          # this line gives an error that one vector is the new vector xdiff is longer #than the dataframe pip_raw                  
  }
  }
  return (pip_diff) 
}

UniqueVecDiff(x, y) # test of the function

