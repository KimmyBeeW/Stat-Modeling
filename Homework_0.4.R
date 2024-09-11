y <- 9.333
x <- 8.375
sd_y <- 4.945
sd_x <- 1.187
n_y <- 12
n_x <- 8
t <- (y-x) / sqrt((((sd_y)^2)/n_y)+(((sd_x)^2)/n_x))
#pt(q=t, df=18, lower.tail = FALSE)
p_value <- 2*pt(q=t, df=18, lower.tail = FALSE)
# Confidence interval for Two-sample T test
(y-x) + c(-1,1) * qt(0.975, df = 18) * sqrt((1/n_y + 1/n_x))


y <- 16.0
x <- 12.0
sd_y <- 5.0
sd_x <- 3.0
n_y <- 9
n_x <- 4
#t <- (y-x)/sqrt((((sd_y)^2)/n_y)+(((sd_x)^2)/n_x))
#p_value <- pt(q=t, df=11, lower.tail = FALSE)
#p_value
# Calculate pooled standard deviation
pooled_sd <- ((n_y-1)*sd_y^2 + (n_x-1)*sd_x^2) / (n_y+n_x-2)
# Two-sample T test
t <- (y-x) / (sqrt(pooled_sd)*(1/n_y + 1/n_x))
p_value <- pt(q=t, df=11, lower.tail = FALSE)
p_value
# 90 Confidence interval for Two-sample T test
(y-x) + (c(-1,1) * qt(0.95, df = 11) * sqrt((1/n_y + 1/n_x)))


z_1 <- (62-50)/10
z_2 <- (1540-1500)/25

#CHAT GPT's answer
# Given data
y_bar_1 <- 16  # Sample mean for population 1
y_bar_2 <- 12  # Sample mean for population 2
s_1 <- 5      # Sample standard deviation for population 1
s_2 <- 3     # Sample standard deviation for population 2
n_1 <- 9     # Sample size for population 1
n_2 <- 4     # Sample size for population 2

# Calculate the standard error of the difference
SE <- sqrt((s_1^2 / n_1) + (s_2^2 / n_2))

# Set the confidence level
confidence_level <- 0.90

# Calculate the degrees of freedom (assuming unequal variances)
df <- ((s_1^2 / n_1 + s_2^2 / n_2)^2) / (((s_1^2 / n_1)^2 / (n_1 - 1)) + ((s_2^2 / n_2)^2 / (n_2 - 1)))

# Calculate the critical value for a two-tailed test
critical_value <- qt((1 + confidence_level) / 2, df)

# Calculate the margin of error
margin_of_error <- critical_value * SE

# Calculate the confidence interval
confidence_interval <- c(y_bar_1 - y_bar_2 - margin_of_error, y_bar_1 - y_bar_2 + margin_of_error)

# Print the confidence interval
cat("90% Confidence Interval for 𝜇1 - 𝜇2:", confidence_interval, "\n")

# Perform a two-sample t-test
t_stat <- (y_bar_1 - y_bar_2) / sqrt((s_1^2 / n_1) + (s_2^2 / n_2))

# Calculate the degrees of freedom (assuming unequal variances)
df <- ((s_1^2 / n_1 + s_2^2 / n_2)^2) / (((s_1^2 / n_1)^2 / (n_1 - 1)) + ((s_2^2 / n_2)^2 / (n_2 - 1)))

# Calculate the p-value
p_value <- 2 * pt(-abs(t_stat), df)

# Print the p-value
cat("P-value:", p_value, "\n")



#Chat GPT Try #2
# Given data
x_bar_1 <- 9.333   # Sample mean for population 1
x_bar_2 <- 8.375   # Sample mean for population 2
s_1 <- 4.945       # Sample standard deviation for population 1
s_2 <- 1.187       # Sample standard deviation for population 2
n_1 <- 12          # Sample size for population 1
n_2 <- 8           # Sample size for population 2

# Calculate the standard error of the difference
SE <- sqrt((s_1^2 / n_1) + (s_2^2 / n_2))

# Set the confidence level
confidence_level <- 0.95

# Calculate the degrees of freedom (assuming unequal variances)
df <- ((s_1^2 / n_1 + s_2^2 / n_2)^2) / (((s_1^2 / n_1)^2 / (n_1 - 1)) + ((s_2^2 / n_2)^2 / (n_2 - 1)))

# Calculate the critical value for a two-tailed test
critical_value <- qt((1 + confidence_level) / 2, df)

# Calculate the margin of error
margin_of_error <- critical_value * SE

# Calculate the confidence interval
confidence_interval <- c(x_bar_1 - x_bar_2 - margin_of_error, x_bar_1 - x_bar_2 + margin_of_error)

# Print the confidence interval
cat("95% Confidence Interval for 𝜇1 - 𝜇2:", confidence_interval, "\n")




#Bing try #1
x_bar_1 <- 9.333   # Sample mean for population 1
x_bar_2 <- 8.375   # Sample mean for population 2
s_1 <- 4.945       # Sample standard deviation for population 1
s_2 <- 1.187       # Sample standard deviation for population 2
n_1 <- 12          # Sample size for population 1
n_2 <- 8           # Sample size for population 2

# Calculate the confidence interval for the difference in means
x_bar_diff <- x_bar_1 - x_bar_2
t_critical <- qt(0.975, df = n_1 + n_2 - 2)
standard_error <- sqrt((s_1^2 / n_1) + (s_2^2 / n_2))
margin_of_error <- t_critical * standard_error

lower_bound <- x_bar_diff - margin_of_error
upper_bound <- x_bar_diff + margin_of_error

# Print the confidence interval
cat("The 95% confidence interval for the difference in means is [", lower_bound, ",", upper_bound, "].", sep = "")




#Bing's confidence interval for pt 2
# Input values
x_bar_1 <- 16
x_bar_2 <- 12
s_1 <- 5
s_2 <- 3
n_1 <- 9
n_2 <- 4

# Calculate the confidence interval
t_critical <- qt(0.95, df = n_1 + n_2 - 2)
standard_error <- sqrt((s_1^2 / n_1) + (s_2^2 / n_2))
margin_of_error <- t_critical * standard_error

lower_bound <- (x_bar_1 - x_bar_2) - margin_of_error
upper_bound <- (x_bar_1 - x_bar_2) + margin_of_error

# Print the confidence interval
cat("The 90% confidence interval for the difference between the two sample means is [", lower_bound, ", ", upper_bound, "].", sep = "")

