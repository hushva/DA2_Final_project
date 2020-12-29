rm( list = ls())

# packages to call
library(tidyverse)
library(stringr)
library(xtable)
library(tidyr)
library(ggplot2)
library(dplyr)


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
# by changing the column type, zeros diappear from the beginning
extra2$imdb_title_id <- as.numeric( as.character( extra2$imdb_title_id ) )
extra2$imdb_title_id <- as.numeric( extra2$imdb_title_id )

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

#
h <- imdb_rating_drop %>% filter( !complete.cases( imdb_rating_drop$year) )
imdb_rating_drop <- imdb_rating_drop %>% filter( complete.cases( imdb_rating_drop$year ) )

# Filter for the observations that are labeled as a movie, since not interested in series
imdb_rating_drop <- imdb_rating_drop %>% filter( type == "movie")

# To figure whether metacritic variable could be used in the analysis
j <- imdb_rating %>% filter( !complete.cases( imdb_rating$metacritic ) )

sapply( imdb_rating_drop, class )

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

# data_join <- data_join %>% filter( year >= 1920) use year or number of voters?

# Write to clean folder
write.csv(share, paste0(dir, "clean/share-health.csv"), row.names = F)

