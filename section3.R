library(tidyverse)

# read from file

fly <- read_table("../fruitfly3.txt", 
                  col_names=c("fly","companions","type","lifespan","thorax",
                              "sleeppercent","group"))
# OR #

fly <- read_table( "fly companions type lifespan thorax sleeppercent group
                   1 8 0 35 0.64 22 preg8
                   2 8 0 37 0.68  9 preg8
                   3 8 0 49 0.68 49 preg8
                   4 8 0 46 0.72  1 preg8
                   5 8 0 63 0.72 23 preg8
                   6 8 0 39 0.76 83 preg8
                   7 8 0 46 0.76 23 preg8
                   8 8 0 56 0.76 15 preg8
                   9 8 0 63 0.76  9 preg8
                   10 8 0 65 0.76 81 preg8
                   11 8 0 56 0.80 12 preg8
                   12 8 0 65 0.80 15 preg8
                   13 8 0 70 0.80 37 preg8
                   14 8 0 63 0.84 24 preg8
                   15 8 0 65 0.84 26 preg8
                   16 8 0 70 0.84 17 preg8
                   17 8 0 77 0.84 14 preg8
                   18 8 0 81 0.84 14 preg8
                   19 8 0 86 0.84  6 preg8
                   20 8 0 70 0.88 25 preg8
                   21 8 0 70 0.88 18 preg8
                   22 8 0 77 0.92 26 preg8
                   23 8 0 77 0.92 24 preg8
                   24 8 0 81 0.92 29 preg8
                   25 8 0 77 0.94 27 preg8
                   1 0 9 40 0.64 18 nocomp
                   2 0 9 37 0.70  6 nocomp
                   3 0 9 44 0.72 19 nocomp
                   4 0 9 47 0.72  7 nocomp
                   5 0 9 47 0.72 16 nocomp
                   6 0 9 47 0.76 13 nocomp
                   7 0 9 68 0.78 35 nocomp
                   8 0 9 47 0.80  2 nocomp
                   9 0 9 54 0.84 35 nocomp
                   10 0 9 61 0.84  6 nocomp
                   11 0 9 71 0.84 15 nocomp
                   12 0 9 75 0.84 14 nocomp
                   13 0 9 89 0.84 18 nocomp
                   14 0 9 58 0.88 50 nocomp
                   15 0 9 59 0.88 25 nocomp
                   16 0 9 62 0.88 10 nocomp
                   17 0 9 79 0.88 33 nocomp
                   18 0 9 96 0.88 43 nocomp
                   19 0 9 58 0.92 35 nocomp
                   20 0 9 62 0.92 17 nocomp
                   21 0 9 70 0.92 27 nocomp
                   22 0 9 72 0.92 22 nocomp
                   23 0 9 75 0.92 16 nocomp
                   24 0 9 96 0.92 20 nocomp
                   25 0 9 75 0.94 37 nocomp
                   1 1 0 46 0.64 23 preg1
                   2 1 0 42 0.68  4 preg1
                   3 1 0 65 0.72 20 preg1
                   4 1 0 46 0.76 42 preg1
                   5 1 0 58 0.76  9 preg1
                   6 1 0 42 0.80 32 preg1
                   7 1 0 48 0.80 66 preg1
                   8 1 0 58 0.80 28 preg1
                   9 1 0 50 0.82 10 preg1
                   10 1 0 80 0.82  4 preg1
                   11 1 0 63 0.84 12 preg1
                   12 1 0 65 0.84 17 preg1
                   13 1 0 70 0.84 12 preg1
                   14 1 0 70 0.84 23 preg1
                   15 1 0 72 0.84 40 preg1
                   16 1 0 97 0.84 18 preg1
                   17 1 0 46 0.88 10 preg1
                   18 1 0 56 0.88 38 preg1
                   19 1 0 70 0.88  7 preg1
                   20 1 0 70 0.88 23 preg1
                   21 1 0 72 0.88 36 preg1
                   22 1 0 76 0.88  9 preg1
                   23 1 0 90 0.88 21 preg1
                   24 1 0 76 0.92 62 preg1
                   25 1 0 92 0.92 36 preg1
                   1 1 1 21 0.68 23 virgin1
                   2 1 1 40 0.68 62 virgin1
                   3 1 1 44 0.72 28 virgin1
                   4 1 1 54 0.76 18 virgin1
                   5 1 1 36 0.78 10 virgin1
                   6 1 1 40 0.80 28 virgin1
                   7 1 1 56 0.80 22 virgin1
                   8 1 1 60 0.80 29 virgin1
                   9 1 1 48 0.84 15 virgin1
                   10 1 1 53 0.84 73 virgin1
                   11 1 1 60 0.84 10 virgin1
                   12 1 1 60 0.84  5 virgin1
                   13 1 1 65 0.84 13 virgin1
                   14 1 1 68 0.84 27 virgin1
                   15 1 1 60 0.88 20 virgin1
                   16 1 1 81 0.88 21 virgin1
                   17 1 1 81 0.88 12 virgin1
                   18 1 1 48 0.90 49 virgin1
                   19 1 1 48 0.90 17 virgin1
                   20 1 1 56 0.90 22 virgin1
                   21 1 1 68 0.90 71 virgin1
                   22 1 1 75 0.90 17 virgin1
                   23 1 1 81 0.90 10 virgin1
                   24 1 1 48 0.92 24 virgin1
                   25 1 1 68 0.92 18 virgin1
                   1 8 1 16 0.64 34 virgin8
                   2 8 1 19 0.64  6 virgin8
                   3 8 1 19 0.68  4 virgin8
                   4 8 1 32 0.72 22 virgin8
                   5 8 1 33 0.72 28 virgin8
                   6 8 1 33 0.74 31 virgin8
                   7 8 1 30 0.76 16 virgin8
                   8 8 1 42 0.76 27 virgin8
                   9 8 1 42 0.76  8 virgin8
                   10 8 1 33 0.78 32 virgin8
                   11 8 1 26 0.80 20 virgin8
                   12 8 1 30 0.80 35 virgin8
                   13 8 1 40 0.82 12 virgin8
                   14 8 1 54 0.82 14 virgin8
                   15 8 1 34 0.84 17 virgin8
                   16 8 1 34 0.84 29 virgin8
                   17 8 1 47 0.84 31 virgin8
                   18 8 1 47 0.84  6 virgin8
                   19 8 1 42 0.88 30 virgin8
                   20 8 1 47 0.88 27 virgin8
                   21 8 1 54 0.88 40 virgin8
                   22 8 1 54 0.88 19 virgin8
                   23 8 1 56 0.88  8 virgin8
                   24 8 1 60 0.88  8 virgin8
                   25 8 1 44 0.92 15 virgin8
") 

## Group means & sds
fly %>% 
  group_by(group) %>%
  summarize(means=mean(lifespan),sds=sd(lifespan))

## OR ##
# tapply(fly$lifespan,fly$group,mean)
# tapply(fly$lifespan,fly$group,sd)
# # alternatively could say
# sd(fly$lifespan[fly$group=="nocomp"])
# sd(fly$lifespan[fly$group=="preg1"])
# sd(fly$lifespan[fly$group=="preg8"])
# sd(fly$lifespan[fly$group=="virgin1"])
# sd(fly$lifespan[fly$group=="virgin8"])



## Check the distributions of the groups (are they all normal-ish?)
boxplot(lifespan ~ group, data=fly)
# or #
ggplot(data = fly, mapping = aes(x = group, y = lifespan)) +
  geom_boxplot() 

### Histograms
ggplot(data = fly, mapping = aes(x = lifespan)) +
  geom_histogram(binwidth = 10) +
  facet_grid(. ~ group)

# or #
par(mfrow=c(3,2))  #sets up a 3x2 "grid" in the plotting region...room for 6 plots
hist(fly$lifespan[fly$group=="nocomp"],main="nocomp")
hist(fly$lifespan[fly$group=="preg1"],main="preg1")
hist(fly$lifespan[fly$group=="preg8"],main="preg8")
hist(fly$lifespan[fly$group=="virgin1"],main="virgin1")
hist(fly$lifespan[fly$group=="virgin8"],main="virgin8")
par(mfrow=c(1,1))

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




