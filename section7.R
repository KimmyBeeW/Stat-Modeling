library(tidyverse)

#setwd("C:/Documents and Settings/William Christensen/My Documents/stat230")

#################################################
### AUDITOR TRAINING (Randomize Complete Block CB[1]) ###
#################################################

## Either of the two lines below will access the data ##
audit <- read_csv("../auditor.csv")
audit <- read_table("
score  block	method
73	1	home
81	1	local
92	1	national
76	2	home
78	2	local
89	2	national
75	3	home
76	3	local
87	3	national
74	4	home
77	4	local
90	4	national
76	5	home
71	5	local
88	5	national
73	6	home
75	6	local
86	6	national
68	7	home
72	7	local
88	7	national
64	8	home
74	8	local
82	8	national
65	9	home
73	9	local
81	9	national
62	10	home
69	10	local
78	10	national
")
audit

audit$block <- as_factor(audit$block)
audit$method <- as_factor(audit$method)  # this line not necessary for these data


audit.lm <- lm(score ~ block + method , data=audit)
#audit.lm <- lm(audit$score ~ audit$block + audit$method )

# method means
audit %>%
  group_by(method) %>%
  summarize(mean(score))

# block means
audit %>%
  group_by(block) %>%
  summarize(mean(score))

anova(audit.lm)

# eta^2 and partial eta^2
audit.eta2.method <- 1295 / (433.3667+1295+112.3333)
audit.eta2.method 
  # 70% of the variability in the scores can be explained by training method
audit.eta2p.method <- 1295 / (1295+112.3333)
audit.eta2p.method
  # 92%; very large effect

#################################################
### DENTAL PAIN (CB[2])                       ###
#################################################

dental <- read_table("https://tofu.byu.edu/stat230/dentalpain.txt")
dental <- read_table("relief block codeine acupunc
                     0.0      1      1      1
                     0.6      1      1      2
                     0.5      1      2      1
                     1.2      1      2      2
                     0.3      2      1      1
                     0.7      2      1      2
                     0.6      2      2      1
                     1.3      2      2      2
                     0.4      3      1      1
                     0.8      3      1      2
                     0.8      3      2      1
                     1.6      3      2      2
                     0.4      4      1      1
                     0.9      4      1      2
                     0.7      4      2      1
                     1.5      4      2      2
                     0.6      5      1      1
                     1.5      5      1      2
                     1.0      5      2      1
                     1.9      5      2      2
                     0.9      6      1      1
                     1.6      6      1      2
                     1.4      6      2      1
                     2.3      6      2      2
                     1.0      7      1      1
                     1.7      7      1      2
                     1.8      7      2      1
                     2.1      7      2      2
                     1.2      8      1      1
                     1.6      8      1      2
                     1.7      8      2      1
                     2.4      8      2      2
"                     )
dental$block <- as_factor(dental$block)
dental$codeine <- as_factor(dental$codeine)
dental$acupunc <- as_factor(dental$acupunc)

dental.lm <- lm(relief ~ block + codeine + acupunc + codeine:acupunc, data=dental)

#Codeine factor means
dental %>%
  group_by(codeine) %>%
  summarize(mean(relief))

#Acupuncture factor means
dental %>%
  group_by(acupunc) %>%
  summarize(mean(relief))

#Codeine*Acupuncture group means
dental %>%
  group_by(codeine,acupunc) %>%
  summarize(mean(relief))

#dental %>%
#  group_by(block) %>%
#  summarize(mean(relief))

#Interaction plot
dental %>% 
  group_by(codeine,acupunc) %>% 
  summarise(mygroups = mean(relief)) -> dental.int
dental.int %>% 
  ggplot() +
  aes(x = codeine, y = mygroups, color = acupunc) +
  geom_line(aes(group = acupunc)) +
  geom_point() +
  ylab("mean of relief")

anova(dental.lm)

# eta^2 and partial eta^2
dental.eta2.codeine <- 2.31125 / (5.59875+2.31125+3.38+0.045+0.30375)
dental.eta2.codeine
  # 20% of the variability in the pain relief scores can be explained by codeine status
dental.eta2p.codeine <- 2.31125 / (2.31125+0.30375)
dental.eta2p.codeine
  # 88%; very large effect

dental.eta2.acupunc <- 3.38 / (5.59875+2.31125+3.38+0.045+0.30375)
dental.eta2p.acupunc <- 3.38 / (3.38+0.30375)

dental.eta2.codbyacu <- 0.045 / (5.59875+2.31125+3.38+0.045+0.30375)
dental.eta2p.codbyacu <- 0.045 / (0.045+0.30375)



#################################################
### SP/RM[1;1] -- Diabetic dogs               ###
#################################################
dogs <- read_csv("../dogs.csv")
dogs <- read_csv("http://tofu.byu.edu/stat230/dogs.csv")

dogs$dog <- as_factor(dogs$dog)

## Using "lm": requires some manual fixing for between-subject tests
dogs.lm <- lm(rate ~ operation + dog + method + operation:method, data=dogs)
anova(dogs.lm) # The F test for operation is WRONG
Foperation <- 320.0/63.5
Foperation
poperation <- 1-pf(Foperation,1,8)
poperation

## OR... Using "aov": all tests are correct #
dogs.aov <- aov(rate ~ operation + Error(dog) + method + operation:method, data=dogs)
summary(dogs.aov)

dogs %>% 
  group_by(operation,method) %>% 
  summarise(mygroups = mean(rate)) -> dogs.int
dogs.int %>% 
  ggplot() +
  aes(x = method, y = mygroups, color = operation) +
  geom_line(aes(group = operation)) +
  geom_point() +
  ylab("mean of rate")


## CAUTION! eta^2 and partial eta^2 are a little more complicated when using SP/RM[;] designs.
## (see notes in Section 7 lecture slides)

eta2.operation <- 320 / (320+508+2420+80+680)    # eta^2
eta2.operation
  # 8% of the variability in the lactic acid turnover can be explained by operation status
eta2p.operation <- 320 / (320+508)               # partial eta^2: NOTE that because operation
                                                 #  is a between-block (between-subject) effect,
                                                 #  we use SS(dog) in the denominator here
eta2p.operation
  # 39%; large effect

eta2.method <- 2420 / (320+508+2420+80+680)
eta2.method
  # 60% of the variability in the lactic acid turnover can be explained by method
eta2p.method <- 2420 / (2420+680)
eta2p.method
  # 78%; very large effect

eta2.opbymeth <- 80 / (320+508+2420+80+680)
eta2p.opbymeth <- 80 / (80+680)
eta2.opbymeth
eta2p.opbymeth

#####################################################
### SP/RM[1;2] -- No data here; just example code ###
#####################################################

# Code for SP/RM[1;2] (but no data...so don't try to run this code):  
#      Assuming that the whole plot factor is "seed" and the sub-plot 
#         factors are "fertilizer" and "herbicide" (as on slide 23 of Stat230_sec7.pdf).
#      That is, levels of seed are randomly assigned to farms (blocks) 
#         and levels of the fertilizer and herbicide factors are randomly 
#         assigned to subplots within farms.  
#      Assume that you have a data set called "mydata" with variables "farm", 
#         "seed", "fertilizer", and "herbicide"

# Don't run this code because the dataset "mydata" doesn't exist
#   mydata.aov <- aov(response ~ seed + Error(farm) + fertilizer + herbicide +
#                        seed:fertizer + seed:herbicide + fertilizer:herbicide +
#                        seed:fertilizer:herbicide, data=mydata)
#   summary(mydata.aov)


#####################################################
### SP/RM[2;1] -- No data here; just example code ###
#####################################################

# Code for SP/RM[2;1] (but no data...so don't try to run this code):  
#      Assuming that the whole plot factors are "seed" and "pesticide" 
#         and the sub-plot factor is "fertilizer" (as on slide 24 of Stat230_sec7.pdf).
#      That is, levels of seed and pesticide are randomly assigned
#         to farms (blocks) and levels of the fertilizer factor are 
#         randomly assigned to subplots within farms.  
#      Assume that you have a data set called "mydata" with variables "farm", 
#         "seed", "pesticide", and "fertilizer"

# Don't run this code because the dataset "mydata" doesn't really exist
#   mydata.aov <- aov(response ~ seed + pesticide + seed:pesticide + Error(farm) + 
#                       fertilizer + seed:fertizer + pesticide:fertilizer + 
#                        seed:perticide:fertilizer, data=mydata)
#   summary(mydata.aov)



#####################################################
### LS[1] -- Gasoline blend vs MPG                ###
#####################################################


cars <- read_csv("../mpg.csv")
cars <- read_csv("http://tofu.byu.edu/stat230/mpg.csv")

cars$driver <- as_factor(cars$driver)
cars$model <- as_factor(cars$model)
cars$blend <- as_factor(cars$blend)

## Using "lm": 
cars.lm <- lm(mpg ~ model + driver + blend, data=cars)
anova(cars.lm) 

## OR... Using "aov":  #
cars.aov <- aov(mpg ~ model + driver + blend, data=cars)
summary(cars.aov)

cars %>%
  group_by(blend) %>%
  summarize(means = mean(mpg))


##########################################################
### LS[2] -- Fake data: Heart rate vs. diet & exercise ###
##########################################################
# Code for LS[2] (but no data...so don't try to run this code):  
#      Assuming that you have a data set called "mydata" with "subject" 
#         as rows, "time" as columns, and factors labeled 
#         "diet" & "exercise"(as on slide 24 of Stat230_sec7.pdf).

# Don't run this code because the dataset "mydata" doesn't really exist
#   mydata.lm <- aov(response ~ subject + time + diet + exercise +
#      diet:exercise, data=mydata)
#   summary(mydata.lm)



##############################################################
### Replicated LS[1] -- FEED ADDITIVE vs. MILK PRODUCTION  ###
##############################################################

# Replicated Latin Square LS[1]:  4 different squares (runs of experiment).  
#   Within each square, 3 cows measured each of 3 time periods with 
#   each time period assigned a feed additive (A, B, or C). 

milk <- read_csv("http://tofu.byu.edu/stat230/milk.csv")
milk$square <- as_factor(milk$square)
milk$time <- as_factor(milk$time)
milk$cow <- as_factor(milk$cow)
milk$additive <- as_factor(milk$additive)

# Question: Do we think of this model as comparing 3 levels of time (i.e., 
#   times "1", "2", and "3" are the same for each square), OR comparing 12 
#   levels of time (e.g., the 3 columns of the first square are "Jan", "Feb",
#   "Mar"; the 3 columns of the second square are "Apr", "May",
#   "Jun"; etc.)??

# Model with 12 levels of time and 12 levels of cow (time NESTED in square)
# This first model implies that there are really 12 distinct times with 
#   12 different time effects (i.e., times are NESTED in square). Because  
#   the times within each square are labeled "1" to "3" within each square, we 
#   need to indicate that time "1" within square 1 is different from time "1" 
#   in square 2 or 3. We do this by describing the time (or column) effect 
#   by writing "time:square" which has 12 levels.  
# This model also implies 12 different cows. We could write "cow:square", but 
#   that is unnecessary since we gave each the 12 cows a unique label (1-12).

milk.aov <- aov(FCMpounds ~ square + time:square + cow + additive, data=milk)
# Equivalently, we could write: 
#   milk.aov <- aov(FCMpounds ~ square + time:square + cow:square + additive, data=milk)
summary(milk.aov)

# Model with 3 levels of time and 12 levels of cow (time CROSSED with square)
# This second model implies that there are really just 3 distinct times. That
#   is, time "1" is the same effect in any square (e.g., regardless of square, 
#   time "1" is always the January effect, time "2" is the February effect, 
#   and time "3" is always the March effect. Because the times within each 
#   square are labeled "1" to "3", we simply refer to the time effect in the 
#   model using "time".  
# This model also implies 12 different cows. We could write "cow:square", but 
#   that is unnecessary since we gave each the 12 cows a unique label (1-12).

milk.aov <- aov(FCMpounds ~ square + time + cow + additive, data=milk)
# Equivalently, we could write: 
#   milk.aov <- aov(FCMpounds ~ square + time + cow:square + additive, data=milk)
summary(milk.aov)

# Which model looks like a better fit to the data?



  




