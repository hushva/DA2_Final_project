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


# Check for missing observations in the most important variables
# m <- imdb_rating %>% filter( !complete.cases( imdb_rating$imdbRating ) )
imdb_rating <- imdb_rating %>% filter( complete.cases( imdb_rating$imdbRating ) )

# g <- imdb_rating %>% filter( !complete.cases( imdb_rating$genre) )
imdb_rating <- imdb_rating %>% filter( complete.cases( imdb_rating$genre ) )

# Remove the columns not relevant in the analysis
col_remove1 <- c("rating", "released", "director", "writer", "cast", "poster", "plot", "fullplot", "language", "lastupdated")
imdb_rating_drop <- imdb_rating %>% 
  dplyr::select(- one_of(col_remove1))

# h <- imdb_rating_drop %>% filter( !complete.cases( imdb_rating_drop$year) )
# imdb_rating_drop <- imdb_rating_drop %>% filter( complete.cases( imdb_rating_drop$year ) )

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

SummStats <- imdb_rating_drop %>%
  keep(is.numeric) %>%
  summary( imdb_rating_drop)
SummStats %>% kable()

rm(g,h,i,j,m)

# Extract the genres and then keep only the primary one (assumption: first genre is the most typical)
imdb_rating_drop2 <-
  separate(imdb_rating_drop, col = genre, into = c("genre1", "genre2"), sep = ",")

imdb_rating_drop2 <- subset(imdb_rating_drop2, select=-c(genre2,type))

rm(f) 
ggplot(imdb_rating_drop2,aes(genre1)) +
  geom_bar(stat = "count")

extra2 <- subset(extra2, select=c(imdb_title_id,production_company,duration,budget,usa_gross_income,worlwide_gross_income,reviews_from_users,reviews_from_critics))


colnames(extra2)[colnames(extra2) == "imdb_title_id"] <- "imdbID" # Rename column


data_join <- left_join(imdb_rating_drop2,extra2,by = c("imdbID" = "imdbID"))

# Get rid of duplicates
data_unique <- distinct(data_join, imdbID, .keep_all = TRUE)


data_unique <- data_unique %>% filter( imdbVotes >= 25000)
ggplot(data_unique,aes(genre1)) +
  geom_bar(stat = "count")

# drop more columns
data_unique <- subset(data_unique, select=-c(production_company,usa_gross_income))

# delete dollar sign
data_unique <- data_unique %>%
  mutate ( worlwide_gross_income = str_replace_all(data_unique$worlwide_gross_income, "[^[:alnum:]]","") )
data_unique$worlwide_gross_income <- as.numeric( as.character( data_unique$worlwide_gross_income ) )

ggplot(data_unique,aes(log( worlwide_gross_income/1000000))) +
  geom_density()


data_unique <- data_unique %>% transmute( 
                                          title = title,
                                          year = year,
                                          genre = genre1,
                                          duration = duration,
                                          metacritic = metacritic,
                                          rating = imdbRating,
                                          votes = imdbVotes,
                                          worldwide_gross_income = worlwide_gross_income/1000000,
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

rating_hist <- movies %>% ggplot() +
  geom_histogram(aes(x = rating), bins = 45, fill = "darkorange", col="darkgrey", alpha= .7) +
  labs(title="Distribution of IMDB rating", x = "Weighted average of movie ratings") +
  scale_x_continuous(limits=c(1,10)) +
  theme_minimal()

duration_hist <- movies %>% ggplot() +
  geom_histogram(aes(x = duration), bins = 25, fill = "darkorange", col="darkorange", alpha= .7) +
  # labs(title="Distribution of movie length", x = "Movie duration (min)") +
   scale_x_continuous(limits=c(60,330)) +
theme_minimal()

movies %>% ggplot() +
  geom_histogram(aes(x = metacritic), bins = 30, fill = "darkorange", col="darkorange", alpha= .7) +
  labs(title="Distribution of metacritic scores", x = "Metacritic score") +
  scale_x_continuous(limits=c(1,100)) +
  theme_minimal()


genre_dist <- movies %>% group_by(genre) %>% 
  mutate(count_name = n())

ggplot(genre_dist,aes(x = reorder(genre, count_name, y = count_name))) +
    geom_bar(fill = "darkorange", color = "darkorange",stat = "count",alpha = .7, width = 0.7 ) +
    coord_flip() +
    labs(title="Number of various genres", x = "Genre") +
    ylab("") +
    theme_minimal()

movies %>% ggplot() +
  geom_histogram(aes(x = votes), bins = 45, fill = "darkorange", col="darkorange", alpha= .7) +
  labs(title="Distribution of number of votes", x = "Votes") +
  ylab("") +
  theme_minimal()

movies %>% ggplot() +
  geom_histogram(aes(x = worldwide_gross_income), bins = 50, fill = "darkorange", col="darkorange", alpha= .7) +
  labs(title="Distribution of movie gross income", x = "Gross income in millions (USD)") +
  ylab("") +
  theme_minimal()

movies %>% ggplot() +
  geom_histogram(aes(x = user_review), bins = 50, fill = "darkorange", col="darkorange", alpha= .7) +
  labs(title="Distribution of user reviews", x = "Number of user reviews") +
  ylab("") +
  theme_minimal()

movies %>% ggplot() +
  geom_histogram(aes(x = critics_review), bins = 50,  alpha= .7) +
  labs(title="Distribution of critics reviews", x = "Number of critics reviews") +
  ylab("") +
  theme_minimal()

movies %>% ggplot(aes(critics_review)) + geom_histogram() +
  scale_fill_manual(values = wes_palette("GrandBudapest2", n = 1) )
  
# create ratio of user/critic_review
genre_dist <- genre_dist %>% 
  mutate ( ucratio = user_review/critics_review) 

  
#Create statistics
ratings_stat <- movies %>% 
  summarise(
    Variable  = "IMDB ratings",
    mean     = round(mean(rating), 2),
    median   = round(median(rating),2),
    std      = round(sd(rating), 2),
    min      = round(min(rating),2),
    max      = round(max(rating),2),
    skew     = round(moments::skewness(rating), 2),
    numObs   = sum( !is.na( rating ) ) )


duration_stat <- movies %>% 
  summarise(
    Variable  = "Movie length",
    mean     = round(mean(duration), 2),
    median   = round(median(duration), 2),
    std      = round(sd(duration), 2),
    min      = round(min(duration), 2),
    max      = round(max(duration), 2),
    skew     = round(moments::skewness(duration), 2),
    numObs   = sum( !is.na( duration ) ) )

# create log transformation graphs
votes_ln_Hist <- movies %>% ggplot() +
  geom_histogram(aes(x = log(votes)), fill = "brown1",  alpha = 0.3) +
  labs(x = "Ln of Confirmed Deaths (1.000s)",
       y = "")

movies %>% ggplot() +
  geom_histogram(aes(x = log(votes)), fill = "brown1",  alpha = 0.7) +
  labs(x = "Ln of number of votes",
       y = "") +
  theme_minimal()

movies %>% ggplot() +
  geom_histogram(aes(x = log(duration)), fill = "brown1",  alpha = 0.7) +
  labs(x = "Ln of movie length",
       y = "") +
  theme_minimal()

movies %>% ggplot() +
  geom_histogram(aes(x = log(worldwide_gross_income)), fill = "brown1",  alpha = 0.7) +
  labs(x = "Ln of gross movie income (in millions)",
       y = "") +
  theme_minimal()

movies %>% ggplot() +
  geom_histogram(aes(x = log(user_review)), fill = "brown1",  alpha = 0.7) +
  labs(x = "Ln of number of user reviews",
       y = "") +
theme_minimal()


##### Model setup
# 
# Outcome variable:      rating  - weighted average user rating on IMDB.
# Parameter of interest: genre - the supposedly primary genre of the movie


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
    labs(y = "Weighted average of imdb ratings") +
    theme_minimal()
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
movies$genre <- factor(df$genre, levels = movies$genre)


chck_sp( movies$genre)

ggplot( movies , aes(x = forcats::fct_rev(reorder(genre,genre)), y = rating) ) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  coord_flip() +
  labs(y = "Weighted average of imdb ratings") +
  xlab("") +
  theme_minimal()
# we see genres with very few observations, but high rating



## critics review
chck_sp( movies$critics_review)
chck_sp( log(movies$critics_review) )
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
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )+
  labs(y = "Weighted average of imdb ratings")

# check with PLS:
ggplot( movies , aes(x = log(user_review), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ lspline(x,c(4.5,7)) , method = lm , color = 'red' )
# not much difference

# with the user-critics ratio:
chck_sp(log ( genre_dist$ucratio ) )
chck_sp(genre_dist$ucratio )
ggplot( genre_dist , aes(x = log(genre_dist$ucratio), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )+
  labs(y = "Weighted average of imdb ratings")

# check with PLS:
ggplot( genre_dist , aes(x = log(genre_dist$ucratio), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ lspline(x,c(-0.7,2.8)) , method = lm , color = 'red' )
# not much difference

# variable: gross income (usd)
chck_sp(movies$worldwide_gross_income)
chck_sp(log (movies$worldwide_gross_income))
# needs to take log and it is most likely non-linear relationship, can check with PLS (knots: 10,18) or quadratic?
ggplot( movies , aes(x = log(worldwide_gross_income), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ poly(x,3) , method = lm , color = 'red' )

# check with PLS:
ggplot( movies , aes(x = log(worldwide_gross_income), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ lspline(x,c(-5,4.8)) , method = lm , color = 'red' )

# variable 
chck_sp( movies$duration )
chck_sp( log( movies$duration) )
ggplot( movies , aes(x = log(duration), y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

ggplot( movies , aes(x = duration, y = rating)) +
  geom_point() +
  geom_smooth( formula = y ~ lspline(x,c(4.3,5.3)) , method = lm , color = 'red' )

## variable: metacritic

ggplot( movies , aes(x = metacritic, y = rating)) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(x= "metacritic score", y = "Weighted average of imdb ratings")
# no need to take log, pretty much linear, should be added to the model

chck_sp(movies$year)
# doesn't seem relevant, it signals, that older movies have high scores in general, less variation

# Think about weightening: genres need to be weighted as some occur only a few time, others a lot (Action)


####
# 5) Comparing explanatory variables 
#
# Check the correlations
#

numeric_df <- keep( genre_dist , is.numeric )
cT <- cor(numeric_df , use = "complete.obs")

# Check for highly correlated values:
sum( cT >= 0.5 & cT != 1 ) / 2

# Find the correlations which are higher than 0.8
id_cr <- which( cT >= 0.5 & cT != 1 )
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
# When number of reviews from critics raise from low to medium there is a slight change (drop) in the output rating.

# Votes and user review
intera2 <- lm(rating ~ user_review * votes, data =genre_dist)
summary(intera2)
plot_model(intera2, type = "pred", terms = c("user_review", "votes"))
# interaction term is not significant

intera3 <- lm(rating ~ ucratio * user_review, data = genre_dist)
summary(intera3)
plot_model(intera3, type = "pred", terms = c("ucratio", "user_review"))

intera4 <- lm(rating ~  votes* worldwide_gross_income, data = genre_dist)
summary(intera4)
plot_model(intera4, type = "pred", terms = c("votes", "worldwide_gross_income"))

intera5 <- lm(rating ~  votes * ucratio, data = genre_dist)
summary(intera5)
plot_model(intera5, type = "pred", terms = c("votes", "ucratio"))


#####
# 6) Modelling
#
# Start from simple to complicated

#
# main variable of interest is genre

# main regression: rating = b0 + b1 * genre
#   reg1: NO controls, simple linear
#   reg2: NO controls, weighted for genre
# Use reg2 and control for:
#   reg3: log duration
#   reg4: reg3 + log gross income
#   reg5: reg4 + metacritic
#   reg6: reg5 + log ucratio

#### reg1: no control, simple linear regression
reg1 <- lm_robust(rating ~ genre  , data = genre_dist )
summary( reg1 )
# R-squared:0.1542
# Coefficient estimate is XXXX
coef

reg2 <- lm_robust(rating ~ genre  , data = genre_dist, weights = count_name )
summary( reg1 )
# R-squared:0.1542
# Coefficient estimate is XXXX

reg3 <- lm_robust(rating ~ genre + log(duration) , data = genre_dist, weights = count_name )
summary( reg3 )
# R-squared:0.2
# Coefficient estimate is XXXX

reg4 <- lm_robust(rating ~ genre + log(duration) + log(worldwide_gross_income), data = genre_dist, weights = count_name )
summary( reg4 )
# R-squared:0.2344
# Coefficient estimate is XXXX

reg5 <- lm_robust(rating ~ genre + log(duration) + log(worldwide_gross_income) + metacritic, data = genre_dist, weights = count_name )
summary( reg5 )
# R-squared:0.5779
# Coefficient estimate is XXXX

reg6 <- lm_robust(rating ~ genre + log(duration) + log(worldwide_gross_income) + metacritic + log(ucratio), data = genre_dist, weights = count_name )
summary( reg6 )
# R-squared:0.5851
# Coefficient estimate is XXXX

# Extra for reg6:
reg61 <- lm_robust(rating ~ genre + log(duration) + log(worldwide_gross_income) + metacritic + lspline (ucratio, 10.54), data = genre_dist, weights = count_name )
summary( reg61 )
# R-squared:0.5858
# Coefficient estimate is XXXX
# very small difference

reg71 <- lm_robust(rating ~ genre + log(duration) + lspline(log(worldwide_gross_income),c(-5,5)) + metacritic + log(ucratio), data = genre_dist, weights = count_name )
summary( reg71 )
# R-squared:0.5904
# Coefficient estimate is XXXX

# what is the optimal knot pont?
library(segmented)
reg6_lm_seg <- lm( rating ~ ucratio , data = genre_dist )
fit_seg_ucratio <- segmented( reg6_lm_seg , seg.Z = ~ucratio, psi = list( ucratio = 5 ) )
summary(fit_seg_ucratio)
# output: 10.54


## Alternatives

#### reg1: no control, simple linear regression
reg7 <- lm_robust(rating ~ log(worldwide_gross_income)  , data = genre_dist )
summary( reg7 )
# R-squared:0.031
# Coefficient estimate is 0.08

reg8 <- lm_robust(rating ~ metacritic  , data = genre_dist )
summary( reg8 )
# R-squared:0.5543
# Coefficient estimate is 0.04

reg9 <- lm_robust(rating ~ metacritic + log(worldwide_gross_income)  , data = genre_dist )
summary( reg9 )
# R-squared:0.5543
# Coefficient estimate is 0.04

reg10 <- lm_robust(rating ~ metacritic + log(worldwide_gross_income) + log(duration) , data = genre_dist, weights = count_name )
summary( reg10 )
# R-squared:0.5721
# Coefficient estimate is 0.04

reg11 <- lm_robust(rating ~ metacritic + log(worldwide_gross_income) + log(duration) + genre , data = genre_dist, weights = count_name )
summary( reg11 )
# R-squared:0.5779
# Coefficient estimate is 0.04

# Summarize findings:
data_out <- "/Users/ilike/Documents/CEU/Courses/2020_Fall/Mandatory/DA2/Final_project/DA2_Final_project/out/"
html_sum <- htmlreg( list(reg2 , reg3 , reg4 , reg5, reg6,reg71),
         type = 'html',
         custom.header = list("Weigthed average ratings for movies"=1:6),
         custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
         custom.coef.names = c("Intercept","genre(w)","log(duration)",
                               "log(gross_income)","metacritic",
                               "log(ucratio)"),
         omit.coef = "Intercept|duration|gross_income|metacritic|ucratio",
       #  reorder.coef = c(2:4,1),
         file = paste0( data_out ,'MovieRatings_model.html'), include.ci = FALSE,
         single.row = FALSE, siunitx = TRUE,
         custom.gof.rows = list( "duration" = c("NO","YES","YES","YES","YES","YES"),
                                 "gross income" = c("NO","NO","YES","YES","YES","YES"),
                                 "metacritic" = c("NO","NO","NO","YES","YES","YES"),
                                 "ucratio" = c("NO","NO","NO","NO","YES","YES")))

