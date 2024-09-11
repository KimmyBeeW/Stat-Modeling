# For HW1, you should add comment before each line of the code below. 
# This is a comment -- a line starting with a "#" in R code is not
#   an instruction/command, but rather a note to whomever reads the code.
#   I've added comments for the first few lines

# NOTE: any line that says "library(PACKAGENAME)" only works if you have 
#    installed PACKAGENAME some time in the past. If you haven't installed 
#    "tidyverse" already, uncomment the line below and run it

# install.packages("tidyverse")

# After you have installed a package, you still need to load the package 
#    or set of packages to use that set of R functions in your R script
library(tidyverse)   # Use this line every time you start an R script

# Read the fish dataset from Dr. C's webpage into an R table and call it "fishies"
#  noting that the first line of the file contains its column names
fishies <- read_table("https://tofu.byu.edu/stat230/fish.txt",col_names=TRUE)

# Note that fishies is a table with columns for cooking method (1,2,or 3) and 
#   measurements of aroma, flavor, texture, and moisture. We'll focus on flavor.

# Note: below I show another acceptable way to comment a line or set of lines. 
#   Long comments generally put above the code
#   Short comments can be put on the same line as the code

fishies # display first several lines of fishies 

# Turn the method column into a factor -- the numbers 1, 2, and 3 
#  do not represent numbers like "1+2=3" but rather 3 LEVELS of a factor
fishies$method <- as.factor(fishies$method)
help(as.factor)



############ YOUR TURN ############

# Gives the average of all of the flavors in the dataset 'fishies'
mean(fishies$flavor)

# Gives the average of all of the flavors in the dataset 'fishies'
fishies$flavor %>% 
  mean()

# For each method, it finds the average flavor in the dataset 'fishies'
    # e.g. for method 1 the average flavor is 5.7333
tapply(fishies$flavor,fishies$method,mean)
help(tapply)

# For each method, it finds the average flavor in the dataset 'fishies'
    # In this version the result is in rows rather than columns.
fishies %>%
  group_by(method) %>%
  summarize(methodmeans = mean(flavor))

# Gives the standard deviation of flavor for each method
fishies %>%
  group_by(method) %>%
  summarize(methodsds = sd(flavor))

# Creates a boxplot for flavor
fishies$flavor %>% 
  boxplot()

# Creates a boxplot for flavor based on method. 
    # Shows the different results of flavor based on method.
fishies %>% 
  ggplot(aes(x=method, y=flavor, group=method)) + geom_boxplot()

# Creates a boxplot for flavor based on method in baseR. Shows same data ^^^
boxplot(flavor ~ method, data=fishies)

# Runs a two-sample t-test with method as the explanatory variable and flavor as the response variable.
    # Compares method 1 with method 2
    # Difference of means
t.test(fishies$flavor[fishies$method==1], fishies$flavor[fishies$method==2],
       var.equal=TRUE)

