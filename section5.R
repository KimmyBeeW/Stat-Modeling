library(tidyverse)

## EDA for fish data set
fish <- read_table("../fish.txt")
# OR #
fish <- read_table("https://tofu.byu.edu/stat230/fish.txt")
# OR #
fish <- read_table("
method aroma flavor texture moisture
1 5.4 6.0 6.3 6.7
1 5.2 6.5 6.0 5.8
1 6.1 5.9 6.0 7.0
1 4.8 5.0 4.9 5.0
1 5.0 5.7 5.0 6.5
1 5.7 6.1 6.0 6.6
1 6.0 6.0 5.8 6.0
1 4.0 5.0 4.0 5.0
1 5.7 5.4 4.9 5.0
1 5.6 5.2 5.4 5.8
1 5.8 6.1 5.2 6.4
1 5.3 5.9 5.8 6.0
2 5.0 5.3 5.3 6.5
2 4.8 4.9 4.2 5.6
2 3.9 4.0 4.4 5.0
2 4.0 5.1 4.8 5.8
2 5.6 5.4 5.1 6.2
2 6.0 5.5 5.7 6.0
2 5.2 4.8 5.4 6.0
2 5.3 5.1 5.8 6.4
2 5.9 6.1 5.7 6.0
2 6.1 6.0 6.1 6.2
2 6.2 5.7 5.9 6.0
2 5.1 4.9 5.3 4.8
3 4.8 5.0 6.5 7.0
3 5.4 5.0 6.0 6.4
3 4.9 5.1 5.9 6.5
3 5.7 5.2 6.4 6.4
3 4.2 4.6 5.3 6.3
3 6.0 5.3 5.8 6.4
3 5.1 5.2 6.2 6.5
3 4.8 4.6 5.7 5.7
3 5.3 5.4 6.8 6.6
3 4.6 4.4 5.7 5.6
3 4.5 4.0 5.0 5.9
3 4.4 4.2 5.6 5.5
")
fish$flavor ## these are the flavor scores...also called fish[[3]]
fish$method ## this is the method variable...also called fish[[1]]

fish$method <- as_factor(fish$method)  ## THIS IS IMPORTANT
  # If we don't "tell" R that method is a factor with 
  # with categories as levels, it will mistake method for
  # a quantitative variable (which it is NOT)

# mean for fish$flavor
fish %>%
  summarize(mean(flavor))
# or #
mean(fish$flavor)   
# or #
mean(fish[[3]])   
# fish[[3]] returns the 3rd column as a numeric vector



# mean for each of the four possible response variables
fish %>%
  summarize(mean(aroma),mean(flavor),mean(texture),mean(moisture))
# or #
apply(fish[,2:5],2,mean)

# get the mean and sd of the flavor score separately for each cooking method
fish %>%
  group_by(method) %>%
  summarize(mean(flavor),sd(flavor))

# or #
tapply(fish$flavor,fish$method,mean)
tapply(fish$flavor,fish$method,sd)

boxplot(fish$flavor)
boxplot(fish$flavor ~ fish$method)

fish.lm <- lm(flavor ~ method, data=fish)
anova(fish.lm)
# Where does our F statistic of 9.4953 show up on 
#   the F_{2,33} distribution?
xvals <- seq(0,20,.1)
plot(xvals,df(xvals,2,33),type="l",col="blue")
abline(h=0)



ggplot(data=fish, mapping=aes(x=fish.lm$residuals)) +
  geom_histogram(binwidth=0.25)
# or #
hist(fish.lm$residuals)
# or #
hist(residuals(fish.lm))







## FRUIT FLY DATA
fly <- read_table("../fruitfly3.txt", 
                   col_names=c("fly","companions","type","lifespan","thorax","sleeppercent","group"))

## Group means & sds
fly %>% 
  group_by(group) %>%
  summarize(means=mean(lifespan),sds=sd(lifespan))


## Check the distributions of the groups (are they all normal-ish?)
boxplot(lifespan ~ group, data=fly)
# or #
ggplot(data = fly, mapping = aes(x = group, y = lifespan)) +
  geom_boxplot() 

### Histograms
ggplot(data = fly, mapping = aes(x = lifespan)) +
  geom_histogram(binwidth = 10) +
  facet_grid(. ~ group)


## ANOVA
fly.lm <- lm(lifespan~group,data=fly)
# alternatively, could say
fly.lm <- lm(fly$lifespan~fly$group)

anova(fly.lm)
summary(fly.lm)
residuals(fly.lm)
# OR #
names(fly.lm)  # gives the names of the components of "fly.lm" 
fly.lm$residuals

hist(residuals(fly.lm))   #is it normal??
# or #
ggplot(data = as_tibble(residuals(fly.lm)), mapping = aes(x = value)) +
  geom_histogram(binwidth = 10) 

# eta^2 and partial eta^2 ....these two are the SAME for a BF[1]
fly.eta2.group <- 11939.28 / (11939.28+26313.52)
fly.eta2.group 
  # 31% of the variability in the scores can be explained by training method
fly.eta2p.group <- 11939.28 / (11939.28+26313.52)
fly.eta2p.group
  # 31%; large effect






## POWER & SAMPLE SIZE
# This is the laborious way to calculate power
n <- 25
siglevel <- 0.05
numgroups <- 5
alphai <- c(-2,-2,-2,-2,8)    
   # notice that sum(alphai^2) is the same as (5-1)*var(c(0,0,0,0,10))
sigma <- 11.67
Phi2 <- n * sum(alphai^2) / (numgroups * sigma^2)
Phi2
 
fcrit <- qf(1 - siglevel, numgroups-1, numgroups*n - numgroups)
fcrit
power <- 1 - pf(fcrit, numgroups-1, numgroups*n - numgroups, numgroups*Phi2)  
  # Notice that the non-centrality parameter
  #   required by R is numgroups*Phi2
power

xpts <- seq(0,20,.01)
plot(xpts,df(xpts,numgroups-1, numgroups*n - numgroups),
     main="F(4,120) [black] and F(4,120,Phi) [red]",type="l",ylab="density")
lines(xpts,df(xpts,numgroups-1, numgroups*n - numgroups, numgroups*Phi2),col="red")
#lines(density(rf(100000,numgroups-1, numgroups*n - numgroups, 2*numgroups*Phi2)),col="green")
abline(v=fcrit,col="green")
abline(h=0,v=0)





# This is the EASY way to calculate power or sample size #

## Give n and leave power out of list of arguments: get power calculated
power.anova.test(groups=5, between.var=var(c(0,0,0,0,10)), within.var=11.67^2, 
                  sig.level=0.05, n=25)

## Give power and leave n out of list of arguments: get n calculated
power.anova.test(groups=5, between.var=var(c(0,0,0,0,10)), within.var=(11.67)^2, 
    sig.level=0.05, power=.869)

power.anova.test(groups=5, between.var=var(c(0,0,0,0,10)), within.var=11.67^2, 
    sig.level=0.05, power=.999)


## Make a power curve (plot n on the x-axis and power on the y-axis)
junk <- power.anova.test(groups=5, between.var=var(c(0,0,0,0,10)), 
         within.var=11.67^2, sig.level=0.05, n=25)
names(junk)
junk$power  


# Power analysis for example in class
ns <- 2:50
powers <- power.anova.test(groups=5, between.var=var(c(50,50,50,50,60)), 
                           within.var=11.67^2, sig.level=0.05, n=ns)$power
plot(ns,powers,type="l",ylim=c(0,1))

# Power analysis...if we expect one of the groups to be 20 units from the other 4 groups
powersc <- power.anova.test(groups=5, between.var=var(c(0,0,0,0,20)), 
                            within.var=11.67^2, sig.level=0.05, n=ns)$power
lines(ns,powersc,col="brown",lty="dotted")

# Power analysis...if we expect alphas to be -5,0,0,0,5
powersd <- power.anova.test(groups=5, between.var=var(c(-5,0,0,0,5)), 
                            within.var=11.67^2, sig.level=0.05, n=ns)$power
lines(ns,powersd,col="green")

# Power analysis...if we expect copulation groups to have means 10 days longer than non-cop groups
powerse <- power.anova.test(groups=5, between.var=var(c(0,0,0,10,10)), 
                            within.var=11.67^2, sig.level=0.05, n=ns)$power
lines(ns,powerse,col="orange",lwd=2)





## MULTIPLE COMPARISONS (using the fruitfly data above)
library(DescTools)
## FRUIT FLY DATA
fly <- read_table("../fruitfly3.txt", 
                  col_names=c("fly","companions","type","lifespan",
                              "thorax","sleeppercent","group")) %>%
  mutate(group=as_factor(group)) %>%
  mutate(group=fct_relevel(group, levels=c("nocomp","preg1","preg8","virgin1","virgin8")))


#fly.lm <- lm(lifespan~group,data=fly)
fly.aov <- aov(lifespan~group,data=fly)    
   # aov is different from lm but does similar calculations
PostHocTest(fly.aov, method="bonferroni")    # This multiples your p-values by the number of comparisons
PostHocTest(fly.aov, method="lsd")
TukeyHSD(fly.aov)
ScheffeTest(fly.aov)

# # BONFERRONI
#   virgin8-virgin1 -18.04 -30.01814  -6.061861 0.00034 ***
# # FISHER'S LSD
#   virgin8-virgin1 -18.04 -26.33266  -9.747342 3.4e-05 ***
# # TUKEY'S HSD
#   virgin8-virgin1 -18.04 -29.64047  -6.439532 0.0003240
# # SCHEFFE'
#   virgin8-virgin1 -18.04 -31.14424  -4.935761 0.0016 ** 
  

# The FDR method of Benjamini and Hochberg ("BH") is typically used
#   when doing LARGE numbers of comparisons (as in genetic testing),
#   but it can also be used when doing all pairwise comparisons among
#   factor levels. Protecting the False Discovery Rate is less stringent
#   than protecting the familywise error rate...but is a good tool for
#   your toolkit.
pairwise.t.test(fly$lifespan,fly$group,p.adjust.method="BH") 





##
## DIFFERENT KINDS OF CONTRASTS
##
fly <- fly %>% 
  mutate(group=as_factor(group)) %>%               # Make sure R knows that group is a factor
  mutate(group=fct_relevel(group, levels=c("nocomp","preg1","preg8","virgin1","virgin8")))  
                                                   # Make sure that R is putting the groups in the order
                                                   # we want; this is important when we start specifying
                                                   # contrasts

fly$group    # Notice that the groups are now in alphanumeric order: nocomp preg1 preg8 virgin1 virgin8

# These customized contrasts make the most sense to me; I added names to each
#    contrast but you don't need to 
contrasts(fly$group) <- cbind(c(1,-.25,-.25,-.25,-.25), #nocomp vs. comp
                              c(0, .5, .5, -.5, -.5),   #preg vs. virgin
                              c(0, .5, -.5, .5, -.5),   #1 vs. 8
                              c(0, .5, -.5, -.5, .5))   #comptype*compnumber interax (ignoring nocomp)
                                                        #  -- This last contrasts allows us to see if
                                                        #     the effect of going from 1 to 8 companions
                                                        #     is DIFFERENT for the different types of 
                                                        #     companions (pregnant vs. virgin)


# Note that the contrast below gives the same p-values for each contrast:
# contrasts(fly$group) <- cbind(c(1, -.25, -.25, -.25, -.25), #nocomp vs. comp
#                               c(0, 1, 1, -1, -1),           #preg vs. virgin
#                               c(0, 1, -1, 1, -1),           #1 vs. 8
#                               c(0, 1, -1, -1, 1)            #comptype*compnumber interax (ignoring nocomp) 
# )

colnames(contrasts(fly$group)) <- c(".nocomp vs. comp",".preg vs. virgin",
                                    ".1 vs. 8",".comptype*compnumber interax")  
                                                 # Adding a "suffix" to add to "group" so that we 
                                                 #  can test the group contrasts in a summary() of
                                                 #  an lm() object
contrasts(fly$group)

# Note that these contrasts are MUTUALLY ORTHOGONAL, so we see, for example:
sum( contrasts(fly$group)[,1] * contrasts(fly$group)[,2] )  
# equals 0; contrast1 and contrast2 are orthogonal...in fact, all 4 are 
# mutually orthog.

anova(lm(lifespan~group,data=fly))
summary(lm(lifespan~group,data=fly))  # Gives t tests for the contrasts
# Because the four contrasts are mutually orthogonal, we can partition or (split) the
#  sum of squares for "group" into 4 pieces. Notice that the SS for group will only be
#  neatly partitioned: 
#    SS_group = SS_contrast1 + SS_contrast2 + SS_contrast3 + SS_contrast4
#  because the contrasts are mutually orthogonal AND the number of contrasts is equal 
#  to the df for group (which is 4). You can partition the SS for group in this case 
#  using the aov() function along with the summary() function with the split option:
summary(aov(lifespan~group,data=fly),split=list( group=list(ncompVcomp=1,pregVvirg=2,
                                                            oneVeight=3,typeXnumberINT=4)))
# Note that summary(aov(...,split=...)) gives SS, MS, and F for each contrast

## You can get the same output with the line below (if you don't care about cute names 
##   for each contrast:
summary(aov(lifespan~group,data=fly),split=list( group=list(1,2,3,4)))



### Other possible choices for contrasts ###

# Suppose we want to compare each group with the mean of the 
#  other groups. Notice that these are NON-ORTHOGONAL contrasts.
contrasts(fly$group) <- cbind(c(-.25,    1, -.25, -.25, -.25),
                              c(-.25, -.25,    1, -.25, -.25),
                              c(-.25, -.25, -.25,    1, -.25),
                              c(-.25, -.25, -.25, -.25,    1))
colnames(contrasts(fly$group)) <- 
  c(".preg1 vs others",".preg8 vs others",".virgin1 vs others",
    ".virgin8 vs others")

# Note that these contrasts are NOT mutually orthogonal, so we see, for example
sum( contrasts(fly$group)[,1] * contrasts(fly$group)[,2] )  
# is NOT equal to 0; contrast1 and contrast2 are NOT orthogonal...in fact, no pair 
#  of contrasts among the 4 are orthogonal

anova(lm(lifespan~group,data=fly))
summary(lm(lifespan~group,data=fly))

# NOTE: with non-orthogonal contrasts, we aren't getting the 
#   comparisons in R that we thought we were. Look at the means 
#   for the groups:
fly %>%
  group_by(group) %>%
  summarize(means=mean(lifespan))

# Instead of the first contrast giving us preg1 vs. others: c(-.25,1,-.25,-.25,-.25), 
#    we get .8*(preg1 - nocomp)???
sum(  tapply(fly$lifespan,fly$group,mean) * c(-0.8,.8,0,0,0)  )

# If that makes no sense to you, join the club!!
#  Takeaway message: We have to VERY careful when we want to use non-orthogonal contrasts in R.

#
# How to create meaningful non-orthogonal contrasts
#

#Suppose we want to compare each group with the mean of the other groups. With 5 groups,
#  we usually limit ourselves to 4 contrasts. Let's compare group 2 vs. others, 
#  group 3 vs. others, group 4 vs. others
mycontrasts <- cbind(c(-.25,    1, -.25, -.25, -.25),
      c(-.25, -.25,    1, -.25, -.25),
      c(-.25, -.25, -.25,    1, -.25),
      c(-.25, -.25, -.25, -.25,    1) )
colnames(mycontrasts) <-  c(".preg1 vs others",".preg8 vs others",".virgin1 vs others",
                            ".virgin8 vs others")

library(MASS)  # needed for nonorthogcontrasts() function below
# Create the nonorthogcontrasts() function by running the code below:
nonorthogcontrasts <- function(X)  # make sure to add the MASS library before running
                  # This function takes a set of non-orthogonal contrasts with 
                  #   with contrasts as columns, and returns a set of adjusted
                  #   contrasts that will allow the user to measure the 
                  #   contrasts of interest.
{
  temp <- cbind(rep(1,nrow(X)),X)
  newcontrasts <- t(ginv(temp))[,-1]
  colnames(newcontrasts) <- colnames(X) 
  return(newcontrasts)
}
# After running the above, you should see that you now have a function in your 
#  R environment. A function is a kind of R object that you can use just like 
#  you use built-in functions like mean() or sd(). Let's see what it does to our 
#  desired contrasts by applying nonorthogcontrasts() to mycontrasts, and then 
#  rounding to 8 digits past the decimal place:
round(nonorthogcontrasts(mycontrasts),8)
   # I know those new contrasts look all wrong, but when we apply this new set of contrasts,
   #   we're "tricking" R into doing the contrasts we care about
contrasts(fly$group) <- nonorthogcontrasts(mycontrasts)  # Use our "fixed-up" contrasts so that we
                                                         # can still test the comparisons implied by
                                                         # mycontrasts
anova(lm(lifespan~group,data=fly))
summary(lm(lifespan~group,data=fly))
   # This gives us the correct comparisons that we wrote out in mycontrasts above
   # NOTE: The "split" option within the aov() function will only work when the contrasts 
   #   are orthogonal, so DON'T run the code using the split option as in the line below...
   #   ...it will not give you what you want:
   #     summary(aov(lifespan~group,data=fly),split=list( group=list(c1=1,c2=2,c3=3,c4=4)))



# POLYNOMIAL CONTRASTS (orthogonal)
#   This kind of contrast is commonly used so I show you them here. 
#   These kinds of contrasts are helpful if your groups represent increasing amounts
#   of something--time, amount of medicine, etc. 
#     Example: if your five groups were 5 dosages of ibuprofen 
#     (0, 100 mg, 200 mg, 300 mg, 400 mg, and 500 mg), you may be interested in seeing 
#     if pain level (the response) changes linearly with dose, or if there is a quadratic
#     response, a cubic response, or a quartic response
#   It makes no sense to use these with the fruitfly data, where groups are NOT of this form.
#   Pretend for a moment that the five fruitfly groups (environments) were as follows: 
#      0, 2, 4, 6, or 8 virgin companions. How does the number of companions affect lifespan? 
#   We could use polynomial contrasts to see if lifespan decreases linearly with number of 
#   companions, or if there is a quadratic effect (a single inflexion point like a parabola), 
#   or a cubic effect (with two inflexion points), or a quartic effect (three inflexion points). 
#   The group means may give us evidence of one or several of these polynomial effects simultaneously.
#   We could check using:

contrasts(fly$group) <- contr.poly(5)  # With 5 groups, this gives us 1st degree (linear), 2nd degree
                                       # (quadratic), 3rd degree (cubic), and 4th degree (quartic) 
                                       # polynomial contrasts. If we had 6 groups, we'd also get a 5th
                                       # degree polynomial contrast, and so on.
colnames(contrasts(fly$group)) <- 
  c(".linear",".quadratic",".cubic",".quartic")    # Giving suffix names for each contrast
contrasts(fly$group)

anova(lm(lifespan~group,data=fly))
summary(lm(lifespan~group,data=fly))
       # If the five groups really represented 0, 2, 4, 6, or 8 virgin companions, the output would
       # indicate that lifespan relationship to the number of companions has both linear and quadratic
       # components
