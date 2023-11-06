#Anatomy of a function
##1. function name
##2. argument
##3. body
##4. what the function returns

library(dplyr)
var_summary <- function(data, var) {
  data %>%
    summarise(
      n = n(),
      min = min({{ var }}),
      max = max ({{ var}}),
      .groups = "drop")  #dropping our groups
}

mtcars %>%
  group_by(cyl) %>%
  var_summary(mpg)

#to build packages, you have to write functions

#Anatomy of a package
##1. Metadata
##2. Source code
##3. namespace

#note: roxygen describe how the functin operates and its arguemnts, dependencies and other metadata