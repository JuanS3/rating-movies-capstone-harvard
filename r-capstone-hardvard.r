# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #
# Autor     <- Sebastian Martinez                                              #
# GitHub    <- github.com/JuanS3                                               #
# WebPage   <- data-forest.co                                                  #
# Twittet   <- @JuanS3bas                                                      #
# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #

# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")

if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")


# Only for re-run
# rm(edx, validation)
# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #
# special functions                                                            #
# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #


# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


# Data set of rating movies by user and date
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))


# convert the values of column 'timestamp' to date values
ratings <- ratings %>% 
    mutate(timestamp = substr(as.POSIXlt(x = timestamp, origin = '1970-01-01'), 1, 10) )

# Data set of movies by movie name and genres
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

rm(dl)

movielens <- left_join(ratings, movies, by = "movieId")

rm(ratings, movies)

# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #
# Preparing Data                                                               #
# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #
# ADD NEW COLUMNS                                                              #
# The new columns has the same information that 'genres' column & 'timestamp'  #
# column, id est, it is split the information from one to many                 #
# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #
 
# add columns of each genre
genres_list <- movielens %>%
    select(genres) %>% 
    separate_rows(genres, sep = '\\|')

genres_list <- unique(genres_list$genres)

for (c in genres_list) {
    if (c == '(no genres listed)') {
        movielens['Others'] <- TRUE
    } else{
        movielens[c] <- ifelse(grepl(c, movielens$genres),TRUE,FALSE)
    }
}

rm(c)

# add columns of year, month and day

movielens <- movielens %>% 
    mutate(rating_year  = as.numeric(substr(timestamp,1,4)),
           rating_month = as.numeric(substr(timestamp,6,7)),
           rating_day   = as.numeric(substr(timestamp,9,10)),
           # this is necessary because R take the language of the pc
           day_name = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[as.POSIXlt(timestamp)$wday + 1],
           movie_year = as.numeric(substr(title, str_length(title)-4, str_length(title)-1)),
           title = substr(title, 1, str_length(title)-7))

movielens[,c('timestamp','genres')] <- NULL


# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #


# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>%
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(test_index, temp, movielens, removed)

# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #
# Data Analysis                                                                #
# ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ #
# mean of ratings by year of movie premiere
edx %>% 
    group_by(movie_year) %>% 
    summarize(mean_rating = mean(rating)) %>% 
    ggplot(aes(movie_year,
               mean_rating)) +
    geom_line()
    ggtitle('Mean reating by year of movie premiere') +
    xlab('Year of movie premiere') +
    ylab('Mean rating')

