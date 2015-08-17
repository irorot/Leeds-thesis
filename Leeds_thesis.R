# Needs comment header

# Iroro: "I need to write a function to subset observations from a data frame
# based on two conditions on two vectors. The first condition is computing
# iterative differences of one vector (time) if they fall within the same
# unique values of another vector (time period). The second condition is to
# subset observations (i.e. row values from all other vectors) within the data
# frame for which the value of the time vector is < 3 secs."

# packages
library(lubridate)
library(plyr)

# Data reading and cleaning
trans18test <- read.csv("./transect18_raw.csv")  # NEVER USE ABSOLUTE PATHS IN SCRIPTS!
## datetrns <- dmy (trans18test$D500X_Date)
## timetrns <- hms (trans18test$D500X_Time)
## dat_tim <- paste(datetrns, timetrns) # R has now converted your datetime
##                                      # objects to character vectors. See:
## class(dat_tim)
## > class(dat_tim)
## [1] "character"

#  Ok, lets create a clean date time STRING (character vector)

trans18test$dt <- paste(trans18test$D500X_Date, trans18test$D500X_Time)
# now convert that to a vector of datetime objects
trans18test$dt <- dmy_hms(trans18test$dt)

# Now you can always check that your object types are what you think. this is
# iportant with times because R will always convert datetimes to string when
# printing to the screen

## > class(trans18test$dt)
## [1] "POSIXct" "POSIXt" 
## > 

# Order our observations within sam_period and Class
trans18test <- ddply(trans18test, .(Class, sam_period), function(x) x[order(x$dt), ])


timediff <- function(x) {
   #if (length(x) == 1)  return(4) # if a unique value of x occurs only once, retain 4 (being > 3)
  # But 4 is not a datetime! That makes no sense.  I cannot understand your logic.
  if (length(x) == 1)  return(new_difftime(second=4)) # don't you want the
                                                      # actual time? Again, I
                                                      # don't understand how
                                                      # you can combine times
                                                      # with differences, but
                                                      # you have not explained
                                                      # the data well enough
                                                      # for me. I still don't
                                                      # really get what you
                                                      # intend to do with
                                                      # groups for which there
                                                      # is only one
                                                      # measurement.
  
  xdiff <- diff(x, lag = 1, differences = 1)
  return(c(xdiff, new_difftime(second=0)))  # does zero diff make sense for
                                            # last measurement? I thought you
                                            # did not want the last one?
}

trans18run <- ddply(trans18test, .(Class, sam_period), mutate, dat_tim_diff = timediff(dt))
trans18_subset <- subset(trans18run, dat_tim_diff > 3)
