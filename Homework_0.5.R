      # Required libraries
      library(pwr)
      
      # Parameters
      confidence_level <- 0.99
      margin_of_error <- 2
      x_bar <- 25
      s <- 3
      
      # Calculate sample size
      sample_size <- pwr::pwr.norm.test(
        d = margin_of_error/s,
        sig.level = 1-confidence_level,
        power = .99,
        alternative = "two.sided"
        )$n
      
      # Print the sample size
      sample_size
      
      
      # Required libraries
      library(pwr)
      
      # Parameters
      alpha <- 0.01
      power <- 0.80
      difference <- 2
      x_bar <- 25
      s <- 3
      
      # Calculate sample size
      sample_size <- pwr::pwr.t.test(
        n = NULL,
        d = difference / s,
        sig.level = alpha,
        power = power,
        type = "one.sample",
        alternative = "greater"
      )$n
      
      # Print the sample size
      sample_size
      
      
      
      brand_a <- c(12.26,9.72,8.60,6.04,11.35,10.81,9.16,11.56)
      brand_b <- c(15.79,14.60,14.54,16.29,12.49,14.13,18.52,16.41)
      mean_a <- mean(brand_a)
      mean_b <- mean(brand_b)
      sd_a <- sd(brand_a)
      sd_b <- sd(brand_b)
      pooled_sd <- ((7)*sd_a^2 + (7)*sd_b^2) / (8+8-2)
      
      library(pwr)
      alpha <- 0.05
      power <- 0.80
      margin_of_error <- 1
      
      sample_size <- pwr::pwr.t.test(
        n = NULL,
        d = margin_of_error / pooled_sd,
        sig.level = alpha,
        power = power,
        type = "two.sample",
        alternative = "two.sided"
      )$n
      sample_size
      
      
      
      library(pwr)
      alpha <- 0.05
      power <- 0.90
      margin_of_error <- 1
      
      sample_size <- pwr::pwr.t.test(
        n = NULL,
        d = margin_of_error / pooled_sd,
        sig.level = alpha,
        power = power,
        type = "two.sample",
        alternative = "greater"
      )$n
      sample_size
      #AI failed, so let's try and figure it out
      
      
      
      
      #Ta help
      marginSampleSize <- function(sd, conf, ME, samples = 2){
        return(samples * (qnorm((conf+1)/2) * sd / ME)^2)
      }
      
      
      #Questions 4-5
      marginSampleSize(sd = 3, conf = 0.99, ME = 2, samples = 1)
      power.t.test(delta = 2, sd = 3, sig.level = 0.01, power = 0.8, 
                   alternative = "one.sided", type = "one.sample")
      
      #Questions 6-7
      yM <- matrix(
        c(11.56,	16.41,
          9.16,	18.52,
          10.81,	14.13,
          11.35,	12.49,
          6.04,	16.29,
          8.60,	14.54,
          9.72,	14.60,
          12.26,	15.79), 
        ncol = 2, byrow = TRUE)
      y1 <- yM[,1]
      y2 <- yM[,2]
      spool <- sqrt((sum((y1-mean(y1))^2)+sum((y2-mean(y2))^2))/14)
      
      
      marginSampleSize(sd = spool, conf = 0.95, ME = 1, samples = 2)
      power.t.test(delta = 1, sd = spool, sig.level = 0.05, power = 0.9, 
                   alternative = "one.sided", type = "two.sample")
      
      

#My Turn
marginSampleSize <- function(sd, conf, ME, samples = 2){
  return(samples * (qnorm((conf+1)/2) * sd / ME)^2)
}
#4
marginSampleSize(sd = 3, conf = 0.99, ME = 2, samples = 1)
((2.576^2)*3^2)/2^2
#5
power.t.test(n = NULL, delta = 2, sd = 3, sig.level = 0.01, power = .80, 
             type = ("one.sample"),
             alternative = ("one.sided"))
#6
brand_a <- c(12.26,9.72,8.60,6.04,11.35,10.81,9.16,11.56)
brand_b <- c(15.79,14.60,14.54,16.29,12.49,14.13,18.52,16.41)
mean_a <- mean(brand_a)
mean_b <- mean(brand_b)
sd_a <- sd(brand_a)
sd_b <- sd(brand_b)
pooled_sd <- sqrt(((7)*sd_a^2+(7)*sd_b^2)/(8+8-2))
pooled_sd
power.t.test(n=NULL, delta = 1, sd = pooled_sd, sig.level = 0.05, power = .8,
             type = ("two.sample"),
             alternative = ('two.sided'))
marginSampleSize(sd = pooled_sd, conf = 0.95, ME = 1, samples = 2)


#7
power.t.test(n=NULL, delta = 1, sd = pooled_sd, sig.level = 0.05, power = .9,
             type = ("two.sample"),
             alternative = ('one.sided'))
