#HW 0.2 Q11
z <- (5 - 4.5) / (2.87/sqrt(32))
z # = 0.9855147
#look at the chart
1 - .8389

#HW 0.3
#Q1-2
SE <- 3.12/(sqrt(9))
Variance <- 3.12^2


#Q3-7
mean <- 92.5805
sd <- 0.4673*(sqrt(25))
sd
n <- 25
# t = 3.38, df = ?, p-value = 0.002
#df <- ?
#marg_err <- qt(.025,df,lower.tail=FALSE)*sd/sqrt(n)
marg_err <- mean - 91.6160
CI_95_ll_mean <- mean-marg_err
#91.6160
CI_95_ul_mean <- mean+marg_err
cat("95% CI Lower Limit =",CI_95_ll_mean,"\r\n")
cat("95% CI Upper Limit =",CI_95_ul_mean,"\r\n")
marg_err / (sd/sqrt(n))
qt95 <- marg_err / (sd/sqrt(n))

#Q8
#z-score for a two-sided 99% confidence interval is 2.807
qt99 <- 2.807
marg_err <- qt99*sd/sqrt(n)
CI_99_ll_mean <- mean-marg_err
CI_99_ul_mean <- mean+marg_err
cat("99% CI Lower Limit =",CI_99_ll_mean,"\r\n")
cat("99% CI Upper Limit =",CI_99_ul_mean,"\r\n")

#Q9
p <- 0.002
p/2 # = 0.001

#Q10-13
x <- c(16.03, 16.04, 16.05, 16.05, 16.02, 16.01, 15.96, 15.98, 16.02, 15.99)
y <- c(16.02, 15.97, 15.96, 16.01, 15.99, 16.03, 16.04, 16.02, 16.01, 16.00)
mean_x <- mean(x)
mean_x
mean_y <- mean(y)
mean_y
sd_x <- sd(x)
sd_x
sd_y <- sd(y)
sd_y
n_x <- length(x)
n_x
n_y <- length(y)
n_y
t.test(x,y,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)
t.test(x,y,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)
t <- 0.79894
df <- 18
p_value <- 0.4347

#Q14-15
#Photoresist is a light-sensitive material applied to semiconductor wafers so 
# that the circuit pattern can be imaged onto the wafer. After application, the 
# coated wafers are baked to remove the solvent in the photoresist mixture and 
# to harden the resist. Here are measurements of photoresist thickness (in kA) 
# for eight wafers baked at 95C and eight wafers baked at 100C. Assume that all 
# 16 of the runs were made in random order.

#95 C: 11.176, 7.089, 8.097, 11.739, 11.291, 10.759, 6.467, 8.315
#100 C: 5.263, 6.748, 7.461, 7.015, 8.133, 7.418, 3.772, 8.963

#Is there evidence to support the claim that the higher baking temperature 
# results in wafers with a lower mean photoresist thickness? Use α = 0.05.
# mean(y)-mean(x)<0

x <- c(11.176, 7.089, 8.097, 11.739, 11.291, 10.759, 6.467, 8.315)
y <- c(5.263, 6.748, 7.461, 7.015, 8.133, 7.418, 3.772, 8.963)
mean_x <- mean(x)
mean_x
mean_y <- mean(y)
mean_y
sd_x <- sd(x)
sd_x
sd_y <- sd(y)
sd_y
n_x <- length(x)
n_x
n_y <- length(y)
n_y
t.test(y,x,
       alternative = c("less"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)
t <- -2.6751
p_value <- 0.009059