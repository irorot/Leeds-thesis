# Needs comment header

# Iroro: "I need to write a function to subset observations from a data frame
# based on two conditions on two vectors. The first condition is computing
# iterative differences of one vector (time) if they fall within the same
# unique values of another vector (time period). The second condition is to
# subset observations (i.e. row values from all other vectors) within the data
# frame for which the value of the time vector is < 3 secs."

# Data reading and cleaning
pip_raw <- read.csv("./leeds_pip.csv") # Use relative paths in scripts.
pip_raw$period <- as.factor(pip_raw$period) 

# DWS attempt. I do not know if I understood the problem correctly. I simply
# created a new difference column separately run for t_sun within each value of
# period.
library(plyr)

timediff <- function(x) {
    # explanaton
    if (length(x) == 1)  return(x)
    xdiff <- diff(x, lag = 1, differences = 1)
    # how to deal with first element? I'll add a zero. Maybe add a large number
    # instead?
    return(c(xdiff, 0)) # or c(xdiff, new_difftime(second=0))
}

# are t_sun values sorted correctly within period?  I'm assuming they are.

pip2 <- ddply(pip_raw, .(period), mutate, t_sun_diff = timediff(t_sun))

pip_subset <- subset(pip2, t_sun_diff < 3) # but what about zeros added above?
                                           # Do you want the first in series or
                                           # not?
