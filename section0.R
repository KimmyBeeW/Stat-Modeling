
# R4DS Sec 8.4 RStudio Projects
# https://r4ds.had.co.nz/workflow-projects.html

# R experts keep all the files associated with a project together -- input data, 
# R scripts, analytical results, figures. This is such a wise and common practice 
# that RStudio has built-in support for this via projects.
#
# Let's make a project for you to use while you're taking Stat 230. 
# Click File > New Project
# Click New Directory
# Click Empty Project
# Call your project stat230 and think carefully about which sub-folder you put the
# project in. If you don't store it somewhere sensible, it will be hard to find it in the future!
# It could be the "My Documents" folder, but you may have a subfolder in that directory
# for "Statistics" (your stat classes) and a subfolder there for "Stat230" (this class)
# 
# Once this process is complete, you'll see a new RStudio project in the subfolder. 
# Notice the .Rproj file. Double-click that file to re-open the project. Notice you 
# get back to where you left off: all the files you were working on are still open.
#
# Good practice is to always start with a clean slate. That is, it will not remember
# the results of the code that you ran last time. 
# ON MAC:
#   Click RStudio > Preferences
#   Uncheck box "Restore .RData into workspace at startup"
#   Change "Save workspace to .RData on exit" to "Never"
# ON PC:
#   Click Tools > Project Options
#   For "Restore .RData into workspace at startup", select "No"
#   For  "Save workspace to .RData on exit", select "No"

# Some background

# this is a "comment"

# add the tidyverse library
# one time only, you will add the library by un-commenting the next line 
# (removing the # sign) and running the line below (highlight the line and click Run)
# install.packages("tidyverse")
# 
# -- OR -- you can Click Tools > Install Packages
# Type tidyverse in the "Packages" window
# Click Install 
#
# everytime you start RStudio:
library(tidyverse)


# enter tidy data (every row is an observation, every column is variable)
stat230 <- read_table("final
                        92
                        88
                        96
                        75
                        99")
# to see the data:
#  Double click on stat230 in the Global Environment (appears as spreadsheet, but can't edit)
#  type the name of the tibble
stat230

# compute some descriptive statistics
stat230 %>%
  summarize(mean(final), median(final), sd(final))

# help for existing R functions
help(sd)
# or Click Help tab and enter sd in search 


# create a graphic of different normal distributions

# one can create your own functions
#  this is why so many people have created R packages!
normfunction <- function(x, mu=0, sigma=1)  
{
  1/sqrt(2*pi*sigma^2) * exp(-1/(2*sigma^2) * (x-mu)^2)
}
# run the function with zero 
normfunction(0)   
# something like this already exists as a pre-existing function
dnorm(0)

# create a sequence of values to pass as arguments to the function
# thinking is: evaluate the function at lots of values then plot the pairs
xpts <- seq(from = -10, to = 10, by = .01)
# see a few of the 1001 values
head(xpts)
tail(xpts)
# N(-5, 1)
plot(xpts,normfunction(xpts, mu = -5, sigma = 1),type="l",col="blue",lwd=2, xlab="x", ylab="f(x)")
# overlay N(0, 4)
lines(xpts,normfunction(xpts, mu = 0, sigma = 4),col="red",lwd=2)
# overlay N(5, 3)
lines(xpts,normfunction(xpts, mu = 5, sigma = 3),col="darkgreen",lwd=2)
# add a legend)
legend(4,.35,c("N(-5, 1)","N(0, 4)","N(5, 3)"),col=c("blue","red","darkgreen"),lwd=2)

# use the built in R function for normal density
plot(xpts, dnorm(xpts, mean = 0, sd = 1))
# notice that this plotted dots, not lines

# plot the N(0, 1) "standard normal" marking the empirical rule
# create a sequence of possible values ... empirical rule says 99.7% of area is between -3 and 3
xpts <- seq(from = -3.5, to = 3.5, by = .01)
plot(xpts,normfunction(xpts,0,1),type="l",lwd=2, xlab="x", ylab="f(x)")
segments(x0=seq(-3,3,1),y0=0,y1=normfunction(seq(-3,3,1)))
abline(h=0)


### 
### One-sample t-test
###
## Generate 20 random numbers from the N(0.3,1) distribution
x <- rnorm(20, mean = 0.3, sd = 1)   
head(x)
##   note: why are yours different? set.seed
##    where 0.3 is the mean and 1 is the STD. DEV. (not variance)
##
## x is a vector, not a tibble (so summarize doesn't work)
mean(x)
sd(x)
n <- length(x)
# create a histogram
hist(x)

## If we want to use "summarize", we can change x from a vector to a tibble
newx <- as_tibble(list(dat=x))
newx
newx %>% summarize(mean(dat),sd(dat))
newx %>% summarize(mean(x),sd(x))
# Make a histogram using "tidyverse"
newx %>% ggplot( aes(x=dat)) + 
  geom_histogram(binwidth=.5,center=0) + 
  ggtitle("Histogram of Random Data")

## Test Ho: mu = 0 vs. Ha: mu > 0 (greater than 0)
# R as a calculator
t <- (mean(x) - 0) / (sd(x)/sqrt(n))
t
pval <- 1-pt(t, df = n-1)
pval

## Test Ho: mu = 0 vs. Ha: mu != 0 (not equal to 0)
pval <- 2*(1 - pt(abs(t), df = n-1))
pval

# or 
help(t.test)
# EASY WAY OF DOING 1-SAMPLE T-TEST
t.test(x, mu=0, alternative="greater")

## 98% confidence interval for mu

# R as calculator
lowercl <- mean(x) - qt(.99,n-1) * sd(x) / sqrt(n)
uppercl <- mean(x) + qt(.99,n-1) * sd(x) / sqrt(n)
cbind(lowercl,uppercl)

# or...EASY WAY:
t.test(x, alternative="two.sided", conf.level=.98)



### 
### Two-sample t-test: Shoe Material Example
###

## Type data into two vectors
A <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
B <- c(14.0,8.8,11.2,14.2,11.8,6.4,9.8,11.3,9.3,13.6)
# one row is one observation

## Type the data in adding a column/variable indicating which shoe type
shoes <- read_table("
Material Wear
A 13.2
A 8.2
A 10.9
A 14.3
A 10.7
A 6.6
A 9.5
A 10.8
A 8.8
A 13.3
B 6.4
B 9.8
B 11.3
B 9.3
B 13.6
B 14
B 8.8
B 11.2
B 14.2
B 11.8
")

# OR Read Method #2: reading from a file stored locally in the subfolder where the .RProj file is
shoes <- read_table("shoes.txt")

# OR Read Method #3: reading from a webpage 
shoes <- read_table("https://tofu.byu.edu/stat230/shoes.txt")


# side by side boxplots
boxplot(Wear ~ Material, data = shoes)
# or #
ggplot(data = shoes, mapping = aes(x = Material, y = Wear)) +
  geom_boxplot() 

# side by side histograms
ggplot(data = shoes, mapping = aes(x = Wear)) +
  geom_histogram(binwidth = 1) +
  facet_grid(. ~ Material)
# or #
par(mfrow=c(1, 2))
# create and store separate tibbles for each Material
A <- shoes %>%
  filter(Material == "A")
B <- shoes %>%
  filter(Material == "B")
# create histograms
hist(A$Wear,nclass=8)
hist(B$Wear,nclass=8)
par(mfrow = c(1, 1))

# Constructing the two-sample t test 'by hand'

shoes %>%
  group_by(Material) %>%
  summarize(mean(Wear), sd(Wear), var(Wear))
# or
mean(A$Wear)
mean(B$Wear)
sd(A$Wear)
sd(B$Wear)

spl2 <- ((10 - 1)*var(A$Wear) + (10 - 1)*var(B$Wear)) / (10 + 10 - 2)
df <- 10 + 10 - 2

t <- (mean(A$Wear) - mean(B$Wear))/ sqrt( (1/10 + 1/10) * spl2 )
t
pval <- 2*(1 - pt(abs(t), df))
pval


# EASY WAY OF DOING 2-SAMPLE T-TEST
t.test(Wear ~ Material, data = shoes, alternative="two.sided", var.equal=TRUE)
#or#
t.test(A$Wear, B$Wear, alternative="two.sided", var.equal=TRUE)


###
### Calculating power for hypothesis tests
###

## Note that power.t.test below obtains slightly different answers from the 
##   z-based formulas on the formula sheet

# One-sample example from Slide 0.31:
power.t.test(power=.8,sig.level=.05,delta=.5,sd=2.7,
             type="one.sample",alternative="one.sided")

# Two-sample example from Slide 0.34:
power.t.test(power=.9,sig.level=.05,delta=1,sd=3,
             type="two.sample",alternative="one.sided")






##  Paired comparison with Shoe Wear Data

## why isn't shoes2 tidy?
shoes2 <- read_table("https://tofu.byu.edu/stat230/shoespaired.txt")
shoes2 
# an observation is a person who wore both materials!

# you could type the data in as tidy
# or you could do some data wrangling if the rows needed to be collapsed

# separate into two tibbles by Material
A <- shoes2 %>%
  filter(Material == "A")
B <- shoes2 %>%
  filter(Material == "B")
# join matching Person into a single row
shoespaired <- A %>%
  inner_join(B, by = "Person")
# notice that "Wear" became renamed to distinguish which data set it came from
shoespaired <- shoespaired %>%
  rename(WearA = Wear.x, WearB = Wear.y)


# DOING PAIRED T TEST USING R AS CALCULATOR #
# compute the difference (A - B)
shoespaired <- shoespaired %>%
  mutate(d = WearA - WearB)

mean(shoespaired$d)
sd(shoespaired$d)
n <- length(shoespaired$d)

t <- mean(shoespaired$d) / (sd(shoespaired$d) / sqrt(n))
t

pval <- 2 * pt(t, 10-1 )
pval

## OR ##
# EASY WAY OF DOING PAIRED T TEST #
t.test(shoespaired$d, alternative="two.sided")
  #or#
t.test(shoespaired$WearA, shoespaired$WearB, paired=TRUE, alternative="two.sided")






