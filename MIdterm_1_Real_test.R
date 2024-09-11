remove(list=ls())

# This is the R code to check out Midterm 1, Winter 2023.
#
# Problem 3
x_bar <- (10.145-(-2.725))/2
test_stat <- x_bar/3
test_stat

####Problem 5
critical_z <- 30+qnorm(.05)*5/sqrt(15)
pnorm(critical_z,28.5,5/sqrt(15))
#
#
# Problem 7
ssqr_pl <-(26*(.76^2)+33*(.51^2))/59
t_stat <- (3.9-3.7)/sqrt(ssqr_pl*(1/34+1/27))
t_stat
#########
# Problem 12, 13
before <- c(5,9,3,8,3,4,3,8,3,7)
after <- c(6,8,2,3,4,6,4,2,3,5)
t.test(before,after,paired=TRUE,)
#
##  Problem 15

n <- (3.1^2)*(qnorm(.95)+qnorm(.80))^2
n

###
ssqr_pl <-(26*(.76^2)+33*(.51^2))/59
upper <- (12.87-14.76)+qt(.975,21)*sqrt(ssqr_pl*(1/12+1/11))
lower <-  (12.87-14.76)+qt(.025,21)*sqrt(ssqr_pl*(1/12+1/11))
upper
lower
####
#Problem 18, 20, 21
ssqr_pl <-(16*7+14*5)/30

t_stat <- (13-16)/sqrt(ssqr_pl*(1/17+1/15))
t_stat
p_value <- 2*pt(t_stat,30)
p_value
lower <- (13-16)+qt(0.05,30)*sqrt(ssqr_pl*(1/17+1/15))
lower
upper <- (13-16)-qt(0.05,30)*sqrt(ssqr_pl*(1/17+1/15))
upper
#
##### Problems 22 through 25
sports <- read.csv("Gsj72Y00aavf.csv",header=TRUE)
sports
byu <- sports[,1]
utah<- sports[,2]
byu
length(byu)
byu <- byu[-32]
byu <- byu[-31]
byu
sd(byu)
sd(utah)
t.test(byu,utah,paired=FALSE,var.equal=TRUE,alternative="greater")

t.test(byu,conf.level=.99,var.equal=TRUE)
t.test(byu,conf.level=.98,var.equal=TRUE)
t.test(byu,conf.level=.95)
t.test(byu,conf.level=.90)
t.test(utah,conf.level=.99)

x_bar_1 <- mean(byu)
x_bar_2 <- mean(utah)
n_1 <- length(byu)
n_2 <- length(utah)
sd_1 <- sd(byu)
sd_2 <- sd(utah)
s_pool <- ((n_1-1)*sd_1+(n_2-1)*sd_2)/(n_1+n_2-2)
s_sqr_pool <- s_pool*s_pool
t_stat <- (x_bar_1 - x_bar_2) / sqrt(s_sqr_pool*(1/n_1 + 1/n_2))
t_stat
ssqr_pl <-(16*7+14*5)/30
t.test(byu, utah, alternative = "greater", conf.level = .9, var.equal = TRUE)
t.test(byu, utah, alternative = "two.sided", conf.level = .9, var.equal = TRUE)
lower <- (x_bar_1-x_bar_2)+qt(0.05,(n_1+n_2)-2)*sqrt(s_sqr_pool/((n_1+n_2)/2))
lower
upper <- (x_bar_1-x_bar_2)-qt(0.05,(n_1+n_2)-2)*sqrt(s_sqr_pool/((n_1+n_2)/2))
upper

t_stat <- (13-16)/sqrt(ssqr_pl*(1/17+1/15))
t_stat
p_value <- 2*pt(t_stat,30)
p_value
lower <- (13-16)+qt(0.05,30)*sqrt(ssqr_pl*(1/17+1/15))
lower
upper <- (13-16)-qt(0.05,30)*sqrt(ssqr_pl*(1/17+1/15))
upper
#
#Problem 26
n<- 1.96^2*25/4
n
#problem 27
n <- 2*(1.645^2)*25/(1.5^2)
n
####

##Problem 28
x_bar <-(9.0757+3.9243)/2
#x_bar <- 6.5
critical_t <-qt(.95,16)
se <-(9.0757-6.5)/critical_t
#se <-1.4753
me <--2*qt(.05,16)*se
me
lower <-6.5+qt(.05,16)*se
upper <-6.5-qt(.05,16)*se
lower
upper
upper-lower
lower <-6.5+qt(.1,16)*se
upper <-6.5-qt(.1,16)*se
lower
upper
lower <-6.5+qt(.035,16)*se
upper <-6.5-qt(.035,16)*se
lower
upper
lower <-6.5+qt(.025,16)*se
upper <-6.5-qt(.025,16)*se
lower
upper
lower <-6.5+qt(.01,16)*se
upper <-6.5-qt(.01,16)*se
lower
upper
