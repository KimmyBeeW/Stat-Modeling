library(tidyverse)
library(car)  # So we can calculate Type II and Type III sums of squares

# Apple App Store Data from https://www.kaggle.com/ramamet4/app-store-apple-data-set-10k-apps?select=AppleStore.csv
# The ever-changing mobile landscape is a challenging space to navigate.
#  To get more people to download your app, you need to make sure they can
#  easily find your app. Mobile app analytics is a great way to understand the
#  existing strategy to drive growth and retention of future user.
#  With million of apps around nowadays, the following data set has become very
#  key to getting top trending apps in iOS app store. This data set contains more
#  than 7000 Apple iOS mobile application details.
# appleStore.csv
# 
# VARIABLES
# "id" : App ID
# "track_name": App Name
# "size_bytes": Size (in Bytes)
# "currency": Currency Type
# "price": Price amount
# "ratingcounttot": User Rating counts (for all version)
# "ratingcountver": User Rating counts (for current version)
# "user_rating" : Average User Rating value (for all version)
# "userratingver": Average User Rating value (for current version)
# "ver" : Latest version code
# "cont_rating": Content Rating
# "prime_genre": Primary Genre
# "sup_devices.num": Number of supporting devices
# "ipadSc_urls.num": Number of screenshots showed for display
# "lang.num": Number of supported languages
# "vpp_lic": Vpp Device Based Licensing Enabled

apps <- read_csv("AppleStore.csv")
apps <- read_csv("http://tofu.byu.edu/stat230/AppleStore.csv")
apps


# Some exploratory data analysis (EDA):

# Q1. What are the most common genres ranked from 1 (most common) to 10 (least common)
#    and how many apps are in each of the top 10?

# Method 1
apps %>% 
  add_count(prime_genre,name="nn") %>%          # Add a count for each genre
  filter(dense_rank(-nn) <= 10) %>% # dense_rank(nn) assigns the highest rank
                                   #  to the highest number. To assign rank "1"
                                   #  to the highest number, we rank "-nn" and 
                                   #  filter out anything not in the top 10
  group_by(prime_genre) %>%        # put apps in groups by genre
  summarize(ngen=n()) %>%          # use summarize to create a new variable called
                                   #   ngen containing groupsizes for genres
  arrange(desc(ngen))                # arrange the genres in descending order of ngen
  
# Method 2 
apps %>% 
  group_by(prime_genre) %>%        # put apps in groups by genre
  summarize(ngen=n()) %>%          # use summarize to create a new variable called
                                   #   ngen containing groupsizes for genres
  arrange(desc(ngen)) %>%          # arrange the genres in descending order of ngen
  slice_head(n=10)                 # We have arranged ALL the genres in the descending 
                                   #   order, but we just want the top 10

# Q2. Similar to the above question, but list the PERCENT for each genre 
#   instead of the count for each genre
apps %>% 
  group_by(prime_genre) %>%        # put apps in groups by genre
  summarize(ngen=n()) %>%          # use summarize to create a new variable called
                                   #   ngen containing groupsizes for genres
  mutate(pgen=ngen/sum(ngen)*100) %>%  # create percent for group by taking the group sizes 
                                       #   divided by the sum of the group sizes * 100
  arrange(desc(ngen)) %>%          # arrange the genres in descending order of ngen
  select(prime_genre,pgen) %>%     # keep only the genres and pgen (percent) in the data set
  slice_head(n=10)                 # We have arranged ALL the genres in the descending 
                                   #   order, but we just want the top 10



# Q3. Create a histogram for the size in MB (bytes/(2^20)) for all apps that cost $.01 or more
apps %>%
  filter(price > 0) %>%                   # keep only the apps with price > 0
  mutate(sizeMB = size_bytes/(2^20)) %>%  # create a new variable ("sizeMB") with size in MB
  ggplot(mapping=aes(x=sizeMB)) +         # plot with sizeMB on the x-axis
    geom_histogram(bins=15)               #   ...and the plot is a histogram with 15 bins

# Q4. Create side-by-side histograms for the size in MB (bytes/(2^20)) for free apps and apps
#   that cost $.01 or more
apps %>%
  mutate(sizeMB = size_bytes/(2^20)) %>%
  mutate(pricegroup = cut(price, breaks=c(0,0.001,Inf),
                          right=FALSE,    # right=FALSE makes the price groups: [0,0.001), and [0.001,Inf).
                                          #   that is, intervals are closed on the left and open on the right;
                                          # right=TRUE would make the price groups: (0,0.001] and (0.001,Inf].
                                          #   that is, intervals would be open on the left and closed on the right;
                          labels=c("Free","NotFree"))) %>%
    # create a new variable ("pricegroup") with two levels: "Free" and "NotFree";
    #   an app has pricegroup equal to "Free" if the app has a price in [0, .001), 
    #   and an app has pricegroup equal to "NotFree" if the app has a price in (0.001, Inf]
  ggplot(mapping=aes(x=sizeMB)) + 
    geom_histogram(bins=15) +
    facet_grid(. ~ pricegroup)            # give a separate histogram for each level of pricegroup

 

#  Q5. Within each genre, find the percent of apps that are available in at least 3 
#    languages. Print the genres sorted by that percentage, listing only the top twenty.
#  To calculate percentages, we use a "logical vector" constructed by comparing lang.num 
#    with 3, which gives a TRUE for apps with >=3 languages and FALSE otherwise. Taking 
#    the mean of a logical vector turns the vector into 1's and 0's, so the mean is the
#    proportion with lang.num>=3

apps %>% 
  group_by(prime_genre) %>%        # put apps in groups by genre
  summarize(perctri=mean(lang.num>=3)*100) %>%  # use summarize to create a new variable called
                                   #   perctri containing the percent in genre that 
                                   #   have >= 3 languages
  arrange(desc(perctri)) %>%       # arrange the genres in descending order of pertri
  slice_head(n=20)                 # We have arranged ALL the genres in the descending 
                                   #   order, but we just want the top 20




# Q6. Question that needs data wrangling: Is the user rating for an app significantly
# affected by:
#  - the popularity of the app (low amount of downloads, medium amount, or high amount), 
#  - the genre of the app
#  - the popularity by genre interaction
# Focus only on the top 5 most common genres and use the following definition for 
# popularity: low=less than 300 downloads, med=at least 300 but less than 3000, 
# high=3000 or more downloads
  
## Create new dataset for analysis
apps5 <- apps %>% 
  mutate(popularity=cut(rating_count_tot,      # mutate compute and append new columns
                        breaks=c(0,300,3000,Inf), right=FALSE,
                                                           # split the data set into 
                                                           # groups with ratings in [0,300), 
                                                           # [300,3000), [3000,Inf)
                        labels=c("low","med","high"))) %>%  # give names to the three groups
  add_count(prime_genre,name="nn") %>%          # Add a count for each genre
  filter(dense_rank(-nn) <= 5) %>%             # dense_rank(nn) assigns the highest rank
                                               #  to the highest number. To assign rank "1"
                                               #  to the highest number, we rank "-nn" and 
                                               # filter out anything not in the top 5
  select(user_rating,prime_genre,popularity) %>%  # keep only the listed columns in the new data "apps5"
  mutate(prime_genre=as_factor(prime_genre))   # Because prime_genre is a character variable and we want
                                               #  it to be a factor with 5 levels 

# Model A (BF[1]): Can prime_genre explain user_rating?

apps5.lmA <- lm(user_rating ~ prime_genre, data=apps5)
summary(apps5.lmA)             # Gives us R^2 values -- same as eta^2 on slide 3.31
anova(apps5.lmA)               # Gives us F statistics and p-values 

apps5 %>% 
  group_by(prime_genre) %>%
  summarize(mean(user_rating))                   # means for genres

# Model B (BF[1]): Can popularity group explain user_rating?

apps5.lmB <- lm(user_rating ~ popularity, data=apps5)
summary(apps5.lmB)             # Gives us R^2 values -- same as eta^2 on slide 3.31
anova(apps5.lmB)               # Gives us F statistics and p-values 

apps5 %>% 
  group_by(popularity) %>%
  summarize(mean(user_rating))                   # means for popularity groups

# Model C (BF[2]): Can prime_genre and popularity group and the prime_genre:popularity group 
#                    interaction explain user_rating?

apps5.lm <- lm(user_rating ~ prime_genre + popularity + prime_genre:popularity, data=apps5)
summary(apps5.lm)             # Gives us R^2 values -- same as eta^2 on slide 3.31
anova(apps5.lm)               # Gives us F statistics and p-values for Type I SS
Anova(apps5.lm,type="II")     # Gives us F statistics and p-values for Type II or III SS

apps5 %>% 
  group_by(prime_genre:popularity) %>%
  summarize(mean(user_rating))                   # means for genre*popularity groups




# Q7. Question that needs data wrangling: Among the apps that are large in size 
# (more than 100 MB) and among the 5 most common genres, are the following 
# significant predictors of the user rating:
#  - the genre of the app
#  - the popularity group of the app (low, med, or high)
#  - version level (current version vs. past versions)
#  - all 3 of the two-way interactions and the three-way interaction
# Use the following definition for 
# popularity: low=less than 300 downloads, med=at least 300 but less than 3000, 
# high=3000 or more downloads

##  Note: I use appid instead of appname because there are two apps with the same name!

appsversion <- apps %>%
  filter(size_bytes > 100*10^6) %>%
  # gather(user_rating,user_rating_ver,key="version",value="rating") %>%  
    # Note that gather() is an older function that has been replaced by pivot_longer()
  pivot_longer(c(user_rating,user_rating_ver),names_to="version",values_to="rating") %>%
  add_count(prime_genre,name="nn") %>%    # Count up how many apps are in each genre and add 
                                          #   a count to each row to indicate how common each 
                                          #   app's genre is.
  filter(dense_rank(-nn) <= 5) %>%    # Keeps only the top 5 (most common) genres
  mutate(id=as_factor(id)) %>%            # Make sure R treats "id" as a factor
  mutate(popularity=cut(rating_count_tot,   # See above for explanations about how cut() is used
                        breaks=c(0,300,3000,Inf),right=FALSE, 
                        labels=c("low","med","high"))) %>%
  select(id,rating,version,prime_genre,popularity)

## RM[2,1] rating ~ genre + popularity + version
appsversion.lm <- aov(rating ~ prime_genre + popularity + prime_genre:popularity +
                        Error(id) + version + version:prime_genre + version:popularity + version:prime_genre:popularity, data=appsversion)
summary(appsversion.lm)      # Gives the anova table after using "aov"
options(digits=9)           # default is 7
summary(appsversion.lm)      # Gives the anova table after using "aov", using more digits


