rm( list = ls())

# packages to call
library(tidyverse)
library(stringr)
library(xtable)
library(tidyr)
library(ggplot2)
library(dplyr)
library(kableExtra)

############# GET DATA ##############
# import csvs from github repo
my_url_git <- 'https://raw.githubusercontent.com/hushva/DA2_Final_project/main/data/'
imdb_rating <- read_csv(paste0( my_url_git , 'raw/movies_initial.csv' ) )
extra <- read_csv(paste0( my_url_git , 'raw/IMDb_movies.csv' ) )


############# CLEAN DATA ##############
# Prepare the key column for the join
# remove letters
extra2 <- extra %>% 
  mutate ( imdb_title_id = str_remove(extra$imdb_title_id, "^tt" ) ) 
# by changing the column type, zeros disappear from the beginning
extra2$imdb_title_id <- as.numeric( as.character( extra2$imdb_title_id ) )
# extra2$imdb_title_id <- as.numeric( extra2$imdb_title_id )

sapply( extra2, class )

# Check for missing observations in the most important variables
m <- imdb_rating %>% filter( !complete.cases( imdb_rating$imdbRating ) )
imdb_rating <- imdb_rating %>% filter( complete.cases( imdb_rating$imdbRating ) )

g <- imdb_rating %>% filter( !complete.cases( imdb_rating$genre) )
imdb_rating <- imdb_rating %>% filter( complete.cases( imdb_rating$genre ) )

# Remove the columns not relevant in the analysis
col_remove1 <- c("rating", "released", "director", "writer", "cast", "poster", "plot", "fullplot", "language", "lastupdated")
imdb_rating_drop <- imdb_rating %>% 
  select(- one_of(col_remove1))

h <- imdb_rating_drop %>% filter( !complete.cases( imdb_rating_drop$year) )
imdb_rating_drop <- imdb_rating_drop %>% filter( complete.cases( imdb_rating_drop$year ) )

# Filter for the observations that are labeled as a movie, since not interested in series
imdb_rating_drop <- imdb_rating_drop %>% filter( type == "movie")

# To figure whether metacritic variable could be used in the analysis
j <- imdb_rating %>% filter( !complete.cases( imdb_rating$metacritic ) )


# Check for extreme values
# all HISTOGRAMS
imdb_rating_drop %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# imdbRating: close to normal distribution, bit skewed to the left, signaling that he mean is well above 5, it is around 7.
# similar with metacritic, which has a better looking bell-shape, the distribution of values are wider, values are more spread, greater variability
# year is strongly right skewed but that is understandable since there are way more movies made in modern times than in the beginning of the 20th century.
#   there was a big jump around years 1925 and 2000.
# imdbVotes would make sense to transform for the analysis part, but since it is already incorporated into the imdbRating variable (how it is calculated)
#   it would be a bad conditioning variable in the visual analysis, so it won't be used


# Summary statistics table
imdb_rating_stat <- imdb_rating_drop %>%
  keep(is.numeric) %>%
  summary( imdb_rating_drop)

xtb1 <- xtable(imdb_rating_stat, type= "latex", caption = "Table 1: Summary statistics")
print(xtb1, comment = FALSE, include.rownames=FALSE)

rm(g,h,i,j,m)

# Extract the genres and then keep only the primary one (assumption: first genre is the most typical)
imdb_rating_drop2 <-
  separate(imdb_rating_drop, col = genre, into = c("genre1", "genre2"), sep = ",")

imdb_rating_drop2 <- subset(imdb_rating_drop2, select=-c(genre2,type))

rm(f) 
ggplot(imdb_rating_drop2,aes(genre1)) +
  geom_bar(stat = "count")

extra2 <- subset(extra2, select=c(imdb_title_id,production_company,budget,usa_gross_income,worlwide_gross_income,reviews_from_users,reviews_from_critics))


colnames(extra2)[colnames(extra2) == "imdb_title_id"] <- "imdbID" # Rename column


data_join <- left_join(imdb_rating_drop2,extra2,by = c("imdbID" = "imdbID"))

# Get rid of duplicates
data_unique <- distinct(data_join, imdbID, .keep_all = TRUE)


data_unique <- data_unique %>% filter( imdbVotes >= 25000)
ggplot(data_unique,aes(genre1)) +
  geom_bar(stat = "count")

# drop more columns
data_unique <- subset(data_unique, select=-c(production_company,budget,usa_gross_income))

# delete dollar sign
data_unique <- data_unique %>%
  mutate ( worlwide_gross_income = str_replace_all(data_unique$worlwide_gross_income, "[^[:alnum:]]","") )
data_unique$worlwide_gross_income <- as.numeric( as.character( data_unique$worlwide_gross_income ) )

ggplot(data_unique,aes(worlwide_gross_income/10000)) +
  geom_density()


data_unique <- data_unique %>% transmute( 
                                          title = title,
                                          year = year,
                                          genre = genre1,
                                          metacritic = metacritic,
                                          rating = imdbRating,
                                          country = country,
                                          wordwide_gross_income = worlwide_gross_income,
                                          user_review = reviews_from_users,
                                          critics_review = reviews_from_critics
                                          )



# Write to clean folder
write.csv(data_unique, "clean/movies.csv", row.names = F)


############# ANALYSIS PART ##############

rm( list = ls())

library(tidyverse)
# For scaling ggplots
require(scales)
# Estimate piecewise linear splines
#install.packages("lspline")
library(lspline)
# Estimate robust SE
#install.packages("estimatr")
library(estimatr)
# Compare models with robust SE
#install.packages("texreg")
library(texreg)
# For different themes
#install.packages(ggthemes)
library(ggthemes)
library(AER)
require(scales)
library(lspline)
library(estimatr)
library(texreg)

my_url_git <- 'https://raw.githubusercontent.com/hushva/DA2_Final_project/main/data/'
movies <- read_csv(paste0( my_url_git , 'clean/movies.csv' ) )

# Re-iterated research question:
#   Does the genre choice of a director yield to certain imdb ratings for movies younger than 1920?


# Descriptives
movies %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# there are some extreme values for user_review, namely 8232, 6938 and 5392.But these are within the top 15 movies in a row, so it is possible, they might
# have a huge fan base. There is no need to drop these values.
# gross income is strongly left skewed and there will be a need for log transformation

sum1 <- summary( movies )
knitr::kable(sum1)

##### Model setup
# 
# Outcome variable:      rating  - weighted average user rating on IMDB.
# Parameter of interest: genre - the supposedly primary genre of the movie

# How imdb rating is calculated:
#   weighted rating (WR) = (v ÷ (v+m)) × R + (m ÷ (v+m)) × C
# Where:
#   R = average for the movie (mean) = (Rating)
#   v = number of votes for the movie = (votes)
#   m = minimum votes required to be listed in the Top 250 (currently 25,000)
#   C = the mean vote across the whole report

# Thinking about potential confounders:
# - availability of dubbing
# - screening in how many countries (how worldwide the screening was)
# - lot harder to acquire old films  - not digitized
# - motion picture content rating system
# - leading actor/actress in the movie is famous/well known

# Check the main parameter of interests and potential confounders:

# rating
ggplot( movies , aes(x = rating)) +
  geom_histogram(binwidth = 0.5,fill='navyblue') +
  labs(x = "Weighted average imdb rating by users") 

# Create a dummy variable for genre:
movies <- fastDummies::dummy_cols(movies, select_columns = c("genre"), remove_first_dummy = TRUE)
# it removed the first dummy variable created from each column. This is done to avoid multicollinearity
# in a multiple regression model caused by included all dummy variables.

## Check some scatter plots with loess ## 

# Create a general function to check the pattern
chck_sp <- function(x_var){
  ggplot( movies , aes(x = x_var, y = rating)) +
    geom_point() +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(y = "Weighted average of imdb ratings") 
}

### Our main interest: genre:
chck_sp(movies$genre_Adventure)
# It seems there is a different average (and sd) for 0 and 1

chck_sp(movies$genre_Animation)
# relatively high average, between ~5.0 and 8.7

chck_sp(movies$genre_Biography)
# Averages mainly between ~6.2 and 8.7

chck_sp(movies$genre_Comedy)
# Averages all around the values --> this is not surprising since matter of someone's humor is very individual

chck_sp(movies$genre_Crime)
# Bit wider rangethan the previous ones, but gets up to high ratings, in general above 5

chck_sp(movies$genre_Documentary)
# Few documentaries, very narrow average rating range, but in general in a high range (~6.3 to 8.25)

# ...

# genre - rating boxplot

movies$genre = as.factor(movies$genre)
is.factor(movies$genre)

ggplot( movies , aes(x = genre, y = rating) ) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Weighted average of imdb ratings")
# we see genres with very few observations, but high rating

## critics review
chck_sp( movies$critics_review)
# Taking the log is not helping, huge variation for lower number of critics reviews, not very linear
# Metacritic also gives films a score out of 100, based on published critics’ reviews.Check whether these two are correlated.

ggplot( movies , aes(x = critics_review, y = metacritic)) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Metacritic ratings")
# Doesn't show strong correlation

## users review
chck_sp(movies$user_review)
chck_sp(log (movies$user_review))
# taking the log looks better! Probably non-linear.
# check it with polynomial:
ggplot( movies , aes(x = log(user_review), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )+
  labs(y = "Weighted average of imdb ratings")

# check with PLS:
ggplot( movies , aes(x = log(user_review), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ lspline(x,c(4.5,7)) , method = lm , color = 'red' )
# not much difference

# with the user-critics ratio:
chck_sp(log ( movies$ucratio ) )

ggplot( movies , aes(x = log(movies$ucratio), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )+
  labs(y = "Weighted average of imdb ratings")

# check with PLS:
ggplot( movies , aes(x = log(movies$ucratio), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ lspline(x,c(-0.7,2.8)) , method = lm , color = 'red' )
# not much difference

# variable: gross income (usd)
chck_sp(movies$wordwide_gross_income)
chck_sp(log (movies$wordwide_gross_income))
# needs to take log and it is most likely non-linear relationship, can check with PLS (knots: 10,18) or quadratic?
ggplot( movies , aes(x = log(wordwide_gross_income), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )

# check with PLS:
ggplot( movies , aes(x = log(wordwide_gross_income), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ lspline(x,c(10,18)) , method = lm , color = 'red' )


## variable: metacritic
chck_sp(movies$metacritic)
# no need to take log, pretty much linear, should be added to the model

chck_sp(movies$year)
# doesn't seem relevant, it signals, that older movies have high scores in general, less variation

# Think about weightening: there is no natural candidate for this...


####
# 5) Comparing explanatory variables 
#
# Check the correlations
#

numeric_df <- keep( movies , is.numeric )
cT <- cor(numeric_df , use = "complete.obs")

# Check for highly correlated values:
sum( cT >= 0.4 & cT != 1 ) / 2

# Find the correlations which are higher than 0.8
id_cr <- which( cT >= 0.4 & cT != 1 )
pair_names <- expand.grid( variable.names(numeric_df) , variable.names(numeric_df) )

# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr ] )
high_corr

# Results:
#   - only those which are 
#       a) possible outcomes, 
#       b) not inteded to include in the main regression

# rating - metacritic has the highest correlation, but rating is our y variable.

# worldwide gross income and user review have the second highest correlation: 0.48.
# Since correlations are not strong between these pairs we can rule out multicollinearity.

# Remove the un-needed variables
rm( numeric_df, id_cr, pair_names )


mod1 <- lm(genre ~ user_review * critics_review, data = movies)
plot(mod1)

ggplot(movies, aes(x = critics_review, y = user_review, color = )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

require(ggthemes)
library(interplot)
library(sjPlot)
library(sjmisc)
library(jtools)

theme_set(theme_sjplot())

# interaction: if parallel line: no interaction occurs
# Nonparallel lines
# An interaction occurs. The more nonparallel the lines are, the greater the strength of the interaction.
intera1 <- lm(rating ~ user_review * critics_review, data = movies)
summary(intera1)
summ(intera1)
plot_model(intera1, type = "pred", terms = c("user_review", "critics_review"))
# interaction term is not significant, there might be some interaction, since the lines' slope change.
# They are almost parallel, though.
# When number of reviews from critics raise from low to medium there is a sight change (drop) in the output rating.

intera2 <- lm(rating ~ metacritic* critics_review, data = movies)
summary(intera2)
plot_model(intera2, type = "pred", terms = c("metacritic", "critics_review"))
# interaction term is not significant

intera3 <- lm(rating ~ metacritic* user_review, data = movies)
summary(intera3)
plot_model(intera3, type = "pred", terms = c("metacritic", "user_review"))

intera4 <- lm(rating ~  metacritic * wordwide_gross_income, data = movies)
summary(intera4)
plot_model(intera4, type = "pred", terms = c("metacritic", "wordwide_gross_income"))

intera5 <- lm(rating ~  user_review * wordwide_gross_income, data = movies)
summary(intera5)
plot_model(intera5, type = "pred", terms = c("user_review", "wordwide_gross_income"))


#####
# 6) Modelling
#
# Start from simple to complicated

#
# Main regression: rating = b0 + b1*genre
#   reg1: NO controls, simple linear (factored genre)
#   With controls:
#   Use reg1 and control for:
#
#   reg2: log / PLS (knots: 10, 18) gross_income
# 
#   reg3: 
#   reg4: reg3 + log (user_review)
#   reg5: reg4 + metacritic

# create ratio of user/critic_review
movies <- movies %>% 
  mutate ( ucratio = user_review/critics_review) 

reg1 <- lm_robust( rating ~ genre , data = movies )
summary( reg1 )

reg2 <- lm_robust( rating ~ genre + lspline( wordwide_gross_income , c(10,18) ) , data = movies )
summary( reg2 )

reg3 <- lm_robust( rating ~ wordwide_gross_income, data = movies )
summary( reg3 )

reg4 <- lm_robust( rating ~ wordwide_gross_income + genre, data = movies )
summary( reg4 )
