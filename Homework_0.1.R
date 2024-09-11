#Q1-6
x <- c(28,18,24,30,7,20,49,6,16,35,45,27)
mean(x) #mean = 25.41667
median(x) #median = 25.5
sd(x) #standard deviation = 13.32433
n <- length(x) # = 12
t.test(x,
       +        alternative = c("less"),
       +        mu = 35, paired = FALSE, var.equal = FALSE,
       +        conf.level = 0.95) 
# t = -2.4915, df = 11, p-value = 0.01498
df <- 11
marg_err <- qt(.025,df,lower.tail=FALSE)*sd/sqrt(n)
CI_95_ll <- point_est-marg_err
CI_95_ul <- point_est+marg_err
cat("point estimate =",point_est,"\r\n")
cat("margin of error =",marg_err,"\r\n")
cat("95% CI Lower Limit =",CI_95_ll,"\r\n")
cat("95% CI Upper Limit =",CI_95_ul,"\r\n")

CI_95_ll_mean <- mean-marg_err
CI_95_ul_mean <- mean+marg_err
cat("95% CI Lower Limit =",CI_95_ll_mean,"\r\n")
cat("95% CI Upper Limit =",CI_95_ul_mean,"\r\n")



#Q7-10
x <- c(32, 29, 31, 27, 29, 30, 34, 31, 29, 33)
mean <- mean(x) #mean = 30.5
median <- median(x) #median = 30.5
sd <- sd(x) #standard deviation = 2.12132
n <- length(x) # = 12
t.test(x,
       alternative = c("greater"),
       mu = 29, paired = FALSE, var.equal = FALSE,
       conf.level = 0.99) 
# t = 2.2361, df = 9, p-value = 0.02609
df <- 9
point_est <- mean-29
marg_err <- qt(.005,df,lower.tail=FALSE)*sd/sqrt(n)
CI_99_ll <- point_est-marg_err
CI_99_ul <- point_est+marg_err
cat("point estimate =",point_est,"\r\n")
cat("margin of error =",marg_err,"\r\n")
cat("99% CI Lower Limit =",CI_99_ll,"\r\n")
cat("99% CI Upper Limit =",CI_99_ul,"\r\n")
CI_99_ll_mean <- mean-marg_err
CI_99_ul_mean <- mean+marg_err
cat("99% CI Lower Limit =",CI_99_ll_mean,"\r\n")
cat("99% CI Upper Limit =",CI_99_ul_mean,"\r\n")


#Q11-16
x <- c(22, 24, 19, 21, 18, 23, 20, 21, 26, 24, 19, 21, 25, 21, 23)
mean <- mean(x)
mean # = 21.8
median <- median(x)
median # = 21
sd <- sd(x) 
sd # = 2.336053
n <- length(x)
n
t.test(x,
       alternative = c("two.sided"),
       mu = 23, paired = FALSE, var.equal = FALSE,
       conf.level = 0.90) 
# t = -1.9895, df = 14, p-value = 0.06655
df <- 14
point_est <- mean-23
marg_err <- qt(.05,df,lower.tail=FALSE)*sd/sqrt(n)
CI_90_ll <- point_est-marg_err
CI_90_ul <- point_est+marg_err
cat("point estimate =",point_est,"\r\n")
cat("margin of error =",marg_err,"\r\n")
cat("90% CI Lower Limit =",CI_90_ll,"\r\n")
cat("90% CI Upper Limit =",CI_90_ul,"\r\n")
CI_90_ll_mean <- mean-marg_err
CI_90_ul_mean <- mean+marg_err
cat("90% CI Lower Limit =",CI_90_ll_mean,"\r\n")
cat("90% CI Upper Limit =",CI_90_ul_mean,"\r\n")