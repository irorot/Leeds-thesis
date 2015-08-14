pip_raw <- read.csv("desktop/leeds_pip.csv")

x <- pip_raw$t_sun
y <- unique (pip_raw$period)
UniqueVecDiff <- function (x, y) { # function UniqueVecDiff that takes two arguments x and y
  as.factor(y) # set the vector y as a factor
  for (i in 1: length (x)) { # for loop, stating for each value of x
    if(unique(y) > 1){  # if the unique values of y are more than one row, since y is a factor 
      xdiff <- diff(x, lag = 1, differences = 1) # calculates the differences between each value 
      pip_diff <- cbind (pip_raw, xdiff)                            #in vector x
    }
  }
  return (pip_diff) 
}

UniqueVecDiff(x, y)

