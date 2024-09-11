library(tidyverse)

#################################################
### FABRIC WEAR DATA (balanced two-way ANOVA) ###
#################################################

wear <- read_csv("../wear.csv")  #note that this file has columns separated by commas
# OR #
wear <- read_table("
prop  filler	wear1	wear2	wear3
1	1	155	169	151
1	1	173	152	141
1	2	137	82	77
1	2	160	82	83
2	1	198	187	176
2	1	177	196	167
2	2	129	94	78
2	2	98	89	48
3	1	235	225	166
3	1	229	270	183
3	2	155	76	92
3	2	132	105	67
")
# OR #
wear <- read_csv("
prop,filler,wear1,wear2,wear3
1,1,155,169,151
1,1,173,152,141
1,2,137,82,77
1,2,160,82,83
2,1,198,187,176
2,1,177,196,167
2,2,129,94,78
2,2,98,89,48
3,1,235,225,166
3,1,229,270,183
3,2,155,76,92
3,2,132,105,67
")

wear$prop <- as_factor(wear$prop)
wear$filler <- as_factor(wear$filler)


# Interaction plots ...quick and ugly #
interaction.plot(wear$prop,wear$filler,wear$wear1)
interaction.plot(wear$filler,wear$prop,wear$wear1)

# OR ...prettier using ggplot #
wear %>% 
  group_by(prop, filler) %>% 
  summarise(mygroups = mean(wear1)) -> wear.int  # Stores the treatment means in wear.int
wear.int %>%     
  ggplot() +
  aes(x = prop, y = mygroups, color = filler) +
  ylab("wear (mg)") +
  geom_line(aes(group = filler)) +
  geom_point()    # I like having prop on the x-axis because filler has 
                  # only two levels (so only two lines on the plot)
wear.int %>% 
  ggplot() +
  aes(x = filler, y = mygroups, color = prop) +
  ylab("wear (mg)") +
  geom_line(aes(group = prop)) +
  geom_point()    # Three lines (one for each proportion...less helpful (IMO))


wear.lm <- lm(wear1 ~ prop + filler + prop:filler, data=wear)
 # OR you could use the line below, creating the same object
 #  wear.lm <- lm(wear$wear1 ~ wear$prop + wear$filler + wear$prop:wear$filler)
anova(wear.lm)

# prop means
wear %>%
  group_by(prop) %>%
  summarize(mean(wear1))
# filler means
wear %>%
  group_by(filler) %>%
  summarize(mean(wear1))
# prop*filler (treatment) means
wear %>%
  group_by(prop,filler) %>%
  summarize(mean(wear1))

# or #
#tapply(wear$wear1, wear$prop, mean)
#tapply(wear$wear1, wear$filler, mean)
#tapply(wear$wear1, wear$prop:wear$filler, mean)


# eta^2 and partial eta^2
wear.eta2.prop <- 3217.167 / (3217.167+10561.333+2987.167+1410)
wear.eta2.prop
  # 18% of the variability in the wear1 can be explained by proportion
wear.eta2p.prop <- 3217.167 / (3217.167+1410)
wear.eta2p.prop
  # 70%; very large effect

wear.eta2.filler <- 10561.333 / (3217.167+10561.333+2987.167+1410)
wear.eta2.filler
  # 58% of the variability in the wear1 can be explained by filler
wear.eta2p.filler <- 10561.333 / (10561.333+1410)
wear.eta2p.filler
  # 88%; very large effect

wear.eta2.propbyfiller <- 2987.167 / (3217.167+10561.333+2987.167+1410)
wear.eta2.propbyfiller
  # 16% of the variability in the wear1 can be explained by filler
wear.eta2p.propbyfiller <- 2987.167 / (10561.333+1410)
wear.eta2p.propbyfiller
  # 25%; large effect


#######################################################
### CANCER SURVIVAL TIMES (unbalanced BF[2] design) ###
#######################################################

library(car)    # So we can use the Anova() function [different
                # from anova()] to get Type II and Type III SS

# use log(days) as the response variable (see HW #8)
cancer <- read_table("../cancer.txt")
  # note-- gender: 1=male, 2=female

# or #
cancer <- read_table("
type gender age days
stomach 2 61  124
stomach 1 69   42
stomach 2 62   25
stomach 2 66   45
stomach 1 63  412
stomach 1 79   51
stomach 1 76 1112
stomach 1 54   46
stomach 1 62  103
stomach 1 46  146
stomach 1 57  340
stomach 2 59  396
bronchus 1 74   81
bronchus 1 74  461
bronchus 1 66   20
bronchus 1 52  450
bronchus 2 48  246
bronchus 2 64  166
bronchus 1 70   63
bronchus 1 77   64
bronchus 1 71  155
bronchus 1 39  151
bronchus 1 70  166
bronchus 1 70   37
bronchus 1 55  223
bronchus 1 74  138
bronchus 1 69   72
bronchus 1 73  245
colon 2 76  248
colon 2 58  377
colon 1 49  189
colon 1 69 1843
colon 2 70  180
colon 2 68  537
colon 1 50  519
colon 2 74  455
colon 1 66  406
colon 2 76  365
colon 2 56  942
colon 2 74  372
colon 1 58  163
colon 2 60  101
colon 1 77   20
colon 1 38  283
rectum 2 56  185
rectum 2 75  479
rectum 2 57  875
rectum 1 56  115
rectum 1 68  362
rectum 1 54  241
rectum 1 59 2175
bladder 1 93 4288
bladder 2 70 3658
bladder 2 77   51
bladder 2 72  278
bladder 1 44  548
kidney 2 71  205
kidney 2 63  538
kidney 2 51  203
kidney 1 53  296
kidney 1 57  870
kidney 1 73  331
kidney 1 69 1685
")
cancer$gender <- as_factor(cancer$gender)
cancer$type <- as_factor(cancer$type)
cancer$gender <- recode_factor(cancer$gender, "1"="male", 
                               "2"="female")

# Sample sizes for groups
table(cancer$gender,cancer$type)


# Interaction plot
interaction.plot(cancer$type,cancer$gender,log(cancer$days))
  # OR #
cancer %>% 
  group_by(type, gender) %>% 
  summarise(mygroups = mean(log(days))) -> cancer.int
cancer.int %>% 
  ggplot() +
  aes(x = type, y = mygroups, color = gender) +
  geom_line(aes(group = gender)) +
  geom_point() +
  ylab("mean of log(survival days)")


# boxplots by gender*type groups
ggplot(data = cancer, mapping = aes(x = interaction(gender,type), y = log(days), 
                                    color=gender)) +
  geom_boxplot() 

# OR, equivalently:
ggplot(data = cancer, mapping = aes(x = type:gender, y = log(days), 
                                    color=gender)) +
  geom_boxplot() 


# Fit the model for log(days) of survival time
cancer.lm <- lm(log(days)~type + gender + type:gender, data=cancer )

# For Type I SS, the order of type and gender will affect the results BECAUSE the
# data are NOT balanced
anova(cancer.lm)  # here type goes first and gender second

cancer.lm <- lm(log(days)~gender + type + gender:type, data=cancer )
anova(cancer.lm)  # here gender goes first and type goes second...different results!


# To get Type III SS to work out properly, assign contrasts to the factors in the 
#   model. The following will work:
cancer.lm <- lm(log(days)~gender + type + gender:type, data=cancer, 
                contrasts=list(gender=contr.poly, type=contr.poly) )
## OR ##
# contrasts(cancer$type) <- contr.poly
# contrasts(cancer$gender) <- contr.poly
# cancer.lm <- lm(log(days)~gender + type + gender:type, data=cancer)

# For Type III SS
Anova(cancer.lm,type=3)  # Type III SS. Note the capital "A" in "Anova"--this is a 
                         # different function than "anova".  Note if you don't like
                         # how much "Anova" rounds the SS and other columns, you can 
                         # use print(Anova(cancer.lm,type=3),digits=8)
# For Type II SS
Anova(cancer.lm,type=2)  # Type II SS are useful when you want to evaluate the 
                         # importance of ALL of the main effects before looking
                         # at any interactions. I.e., significance of type in the 
                         # presence of gender (but before the interaction), 
                         # significance of gender in the presence of type (but 
                         # before the interaction), then the interaction last.

# Notes on eta^2 when using Type II or Type III SS
#   If you are calculating eta^2, you want to make sure you're comparing the
#   factor's SS to the Corrected Total Sum of Squares (SSCT). When data are balanced, 
#   or when using Type I SS, SSCT is just the sum of all effects including the error 
#   term but NOT including the grand mean.  When data are unbalanced, these sums of 
#   squares do not add up to the SSCT. Also, use caution when interpreting partial eta^2
#   values when using Type II or Type III SS. 
SSCT <- sum( (log(cancer$days) - mean(log(cancer$days)))^2 )
## The above is the same as SS_{Observed} - SS_{Mean}:
# SSCT <- sum( log(cancer$days)^2)  - mean(log(cancer$days))^2 * nrow(cancer)

SSCT
  # Note that for Type I SS, SSCT is just the sum of SSfiller, SSprop, SSinterax, & SSE
  # That is:
  #          SSCT <- sum(anova(cancer.lm)$`Sum Sq`)

# eta^2 values should then be calculated by taking the SS for each 
#   factor and dividing by SSCT
# For example, using Type II SS, 
cancer.eta2.cancertype <- 21.859 / SSCT
cancer.eta2.cancertype
  # 25.1% of the variability in log(survival days) can be explained by cancer type 
cancer.eta2p.cancertype <- 21.859 / (21.859+59.73788)
cancer.eta2p.cancertype
  # 26.8%; large effect

cancer.eta2.gender <- 0.397 / SSCT
cancer.eta2.gender
  # 0.46% of the variability in log(survival days) can be explained by gender 
cancer.eta2p.gender <- 0.397 / (0.397+59.73788)
cancer.eta2p.gender
  # 0.66%; very small effect

cancer.eta2.typebygen <- 4.65 / SSCT
cancer.eta2.typebygen
  # 5.4% of the variability in log(survival days) can be explained by gender 
cancer.eta2p.typebygen <- 4.65 / (4.65+59.73788)
cancer.eta2p.typebygen
  # 7.2%; small effect

# Model eta^2 (i.e., % of variability explained by all of the effects NOT 
#   including the grand mean), can be obtained by summing the effects' Type I SS values
#   divided by SSCT, or by taking SS_{Model} / SSCT = (SSCT - SSE) / (SSCT) 
cancer.eta2.overall <- (SSCT - 59.738) / SSCT
cancer.eta2.overall
  # 31.2% of the variability in log(survival days) can be collectively explained 
  #   by the cancer type, gender, and the type*gender interaction
  
  

########################################
### reminder about balanced data:    ###
### Type I, II, & III are equivalent ###
########################################

# Using the wear data (which is balanced)
  wear.lm <- lm(wear1 ~ prop + filler + prop:filler, data=wear, 
                contrasts=list(prop=contr.poly, filler=contr.poly))
  anova(wear.lm)
  Anova(wear.lm,type=2)
  Anova(wear.lm,type=3)
  
  
##############################
###  BF[3]                 ###
##############################

# Bacteria are cultured in medical laboratories to identify them so patients 
# can be treated correctly.  The tryptone dataset contains measurements of 
# bacteria counts following the culturing of five strains of Staphylococcus 
# aureus.  There are many strains of Staphylococcus aureus; five were used 
# by the experimenter.  They are identified by numbers in the data because 
# their names are too complicated to be useful as identifiers.  The dataset 
# also contains the time of incubation, temperature of incubation and 
# concentration of tryptone, a nutrient.  The protocols for culturing this 
# bacteria, set the time at 24 hours, the temperature at 35 degrees and the 
# tryptone concentration at 1.0%.  The question is whether the conditions 
# recommended in the protocols for the culturing of these strains are 
# optimal.  The task is to find the incubation time, temperature and 
# tryptone concentration that optimises the growth of this Bacterium.

tryptone <- read_table("../tryptone.txt")
tryptone$Time <- as_factor(tryptone$Time)
tryptone$Temp <- as_factor(tryptone$Temp)
tryptone$Conc <- as_factor(tryptone$Conc)

tryptone.lm <- lm(Count1 ~ Time + Temp + Conc + Time:Temp + Time:Conc + 
                    Temp:Conc, data=tryptone )
anova(tryptone.lm)   # Type I SS is the default
tryptone.lm <- lm(Count1 ~ Time + Temp + Conc + Time:Temp + Time:Conc + 
                    Temp:Conc, data=tryptone )
Anova(tryptone.lm, type=2)   # Why does Type 2 SS give the same results as Type I SS?



