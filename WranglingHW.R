library(tidyverse)
library(car)  # So we can calculate Type II and Type III sums of squares

apps <- read_csv("data/AppleStore.csv")

# Number 57
# (a) What percent of apps are free in each of the cont_rating groups (i.e., the age content ratings)? Sort the list by 
# these percentages by the size of the cont_rating groups (largest to smallest)?
apps %>% 
  group_by(cont_rating) %>%
  summarize(percentfree = 100*mean(price == 0), ngroup = n()) %>%
  arrange(desc(ngroup)) #%>% 
 # select(-ngroup)

# OR #
apps %>% 
  add_count(cont_rating,name="nn") %>% 
  group_by(cont_rating) %>%
  summarize(percentfree = 100*mean(price == 0), ngroup = mean(nn)) %>%
  arrange(desc(ngroup))


# (b) Give side-by-side histograms for the user_rating distribution of apps for each of the 
# cont_rating groups.
apps %>% 
  group_by(cont_rating) %>%
  ggplot(mapping=aes(x=user_rating)) +
    geom_histogram(binwidth=0.5,center=5.0) +
    facet_grid(. ~ cont_rating)

# (c) Give side-by-side histograms for the size_bites distribution of apps for each of the
# top 8 most common prime_genre groups. Recreate the same plot for the log(size_bytes)
# distribution. Which is more promising as a response variable for ANOVA: size_bytes or
# log(size_bytes)?

apps %>% 
  add_count(prime_genre,name="nn") %>%  
  filter(dense_rank(-nn) <= 8) %>%
  group_by(prime_genre) %>%
  ggplot(mapping=aes(x=size_bytes)) +
  geom_histogram(bins=15) +
  facet_grid(. ~ prime_genre)

apps %>% 
  add_count(prime_genre,name="nn") %>%  
  filter(dense_rank(-nn) <= 8) %>%
  mutate(logsizebytes=log(size_bytes)) %>%
  group_by(prime_genre) %>%
  ggplot(mapping=aes(x=logsizebytes)) +
  geom_histogram(bins=15) +
  facet_grid(. ~ prime_genre)

# OR #
apps %>% 
  add_count(prime_genre,name="nn") %>%  
  filter(dense_rank(-nn) <= 8) %>%
  group_by(prime_genre) %>%
  ggplot(mapping=aes(x=log(size_bytes))) +
  geom_histogram(bins=15) +
  facet_grid(. ~ prime_genre)

# apps %>% 
#   group_by(cont_rating) %>%
#   ggplot(mapping=aes(x=user_rating)) +
#   stat_count(mapping = aes(x=user_rating, y=100*(..prop..) )) +
#   #geom_histogram(binwidth=0.5,center=5.0,aes(y = 100*(..count..)/sum(..count..))) +
#   facet_grid(. ~ cont_rating) #+
#   ylab("percent") + scale_y_continuous(labels = percent_format())

# Number 60
# Using your EDA work from number 57(c), carry out an ANOVA comparing the means of
# log(size_bytes) for each of the top 8 prime_genres of apps. Give a complete ANOVA
# table and write a summary paragraph describing your findings. (Make sure you discuss
# the p-value for the F test and describe how practically important/useful the
# genre is for understanding differences in app size. As always, give all code.

apps5 <- apps %>% 
  add_count(prime_genre,name="nn") %>%  
  filter(dense_rank(-nn) <= 8) %>%
  select(prime_genre,size_bytes)

## log(size) ~ pricegroup 
apps5.lm <- lm(log(size_bytes) ~ prime_genre , data=apps5)
summary(apps5.lm)
anova(apps5.lm)


# Number 69
# Evaluate whether pricegroup (either free, cheap, or expensive), prime_genre, or rating group
# effects the log(size_bytes) of an app. Also consider all the two-way interactions
# between the main effects. Define the rating group factor as follows: "poorfair" for
# apps with ratings in the 0 to 3.0 range, "good" for apps with ratings in the 3.5 to 4.0
# range, and "excellent" for apps in the 4.5 to 5.0 range. Define price group as follows:
# "cheap" is for apps in the $.01 to $3 range, "expensive" is for apps $3 or more, and "free" is...
# well... free. Include only on the top 10 most common genres in your analysis.

apps62 <- apps %>% 
  mutate(pricegroup=cut(price,
                        breaks=c(0,.001,3,Inf),right=FALSE,
                        labels=c("free","cheap","expensive"))) %>%
  mutate(ratinggroup=cut(user_rating,
                         breaks=c(0,3.1,4.1,5.1),right=FALSE,
                         labels=c("poorfair","good","excellent"))) %>%
  add_count(prime_genre,name="nn") %>%  
  filter(dense_rank(-nn) <= 10) %>%
  select(pricegroup,prime_genre,ratinggroup,size_bytes)
apps62.lm <- lm(log(size_bytes) ~ pricegroup + prime_genre + ratinggroup + 
                pricegroup:prime_genre + pricegroup:ratinggroup + 
                prime_genre:ratinggroup + prime_genre:ratinggroup:pricegroup , 
                data=apps62)
summary(apps62.lm)
anova(apps62.lm)
Anova(apps62.lm,type="II")     # Gives us F statistics and p-values for Type II or III SS



# Number 86
# Among the apps that are  most popular (more than 500 ratings), are the following
# significant predictors of the user rating:
#  - the genre of the app (consider only the top 8 genres)
#  - the size of the app (small, med, or large)
#  - version level (current version vs. past versions)
# In addition to each of the 3 main effects above, consider all 3 of the two-way interactions and the three-way interaction
# Use the following definition for
# appsize: small=less than 50,000,000 bytes (50 megabytes), med=between 50,000,000 and 100,000,000 bytes, and
# large= greater than 100,000,000 bytes

appsversion <- apps %>%
  filter(rating_count_tot > 500) %>%
  #gather(user_rating,user_rating_ver,key="version",value="rating") %>%
  pivot_longer(c(user_rating,user_rating_ver),names_to="version",values_to="rating") %>%
  add_count(prime_genre,name="nn") %>%    
  filter(dense_rank(-nn) <= 8) %>%
  mutate(id=as_factor(id)) %>%
  mutate(appsize=cut(size_bytes,  
                        breaks=c(0,50000000,100000000,Inf),right=FALSE, 
                        labels=c("small","med","large"))) %>%
  select(id,rating,version,prime_genre,appsize)

## RM[2,1] rating ~ genre + appsize + version
appsversion.lm <- aov(rating ~ prime_genre + appsize + prime_genre:appsize +
                        Error(id) + version + version:prime_genre + version:appsize + version:prime_genre:appsize, data=appsversion)
summary(appsversion.lm)

