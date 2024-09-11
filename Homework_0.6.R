#Q1-3
yM <- matrix(
  c(1.186,	1.061,
    1.151,	0.992,
    1.322,	1.063,
    1.339,	1.062,
    1.200,	1.065,
    1.402,	1.178,
    1.365,	1.037,
    1.537,	1.086,
    1.559,  1.052), 
  ncol = 2, byrow = TRUE)
karlsruhe <- yM[,1]
lehigh <- yM[,2]
Karlsruhe_Method <- c(1.186, 1.151,1.322,1.339,1.200,1.402,1.365,1.537,1.559)
Lehigh_Mehod <- c(1.061,0.992,1.063,1.062,1.065,1.178,1.037,1.086,1.052)
spool <- sqrt((sum((karlsruhe-mean(karlsruhe))^2)+sum((lehigh-mean(lehigh))^2))/16)
sdk <- sd(Karlsruhe_Method)
sdl <- sd(Lehigh_Mehod)
Pair(karlsruhe, lehigh)
t.test(karlsruhe, lehigh,
       alternative = c("two.sided"),
       mu = 0, paired = TRUE, var.equal = FALSE,
       conf.level = 0.95)


#Q4-13
student_participation <- matrix(
  c(10,	8,
    3,	4,
    4,	2,
    8,	5,
    8,	7,
    9,	8,
    5,	4,
    7,	5,
    1,  2,
    7,  5), 
  ncol = 2, byrow = TRUE)
before <- student_participation[,1]
after <- student_participation[,2]
spool <- sqrt((sum((before-mean(before))^2)+sum((after-mean(after))^2))/14)
sdb <- sd(before)
sda <- sd(after)
Pair(before, after)
t.test(before, after,
       alternative = c("two.sided"),
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)

#Q6-7
t.test(before, after,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)


