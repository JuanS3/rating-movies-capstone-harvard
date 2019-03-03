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
# mean of ratings per year by movies premiere and by users ratings
graph_myear <- edx %>% 
    group_by(movie_year) %>% 
    summarize(mean_rating = mean(rating)) 
colnames(graph_myear) <- c('year', 'movie_rating')

graph_ryear <- edx %>% 
    group_by(rating_year) %>% 
    summarize(mean_rating = mean(rating))
colnames(graph_ryear) <- c('year', 'user_rating')

graph_year <- left_join(graph_myear, graph_ryear, 'year')

rm(graph_myear, graph_ryear)

graph_year %>% 
    ggplot(aes(year)) +
    # mean of ratings per year by movies premiere
    geom_line(aes(y = movie_rating, colour = 'movie premiere')) +
    geom_point(aes(y = movie_rating, colour = 'movie premiere')) +
    geom_label(aes(y = movie_rating,
                  label = round(movie_rating, digits = 1),
                  fontface = 'bold'),
             nudge_y = 0.004,
             colour = 'white',
             fill = 'red',
             alpha = 0.7) +
    # mean of ratings per year by users ratings
    geom_line(aes(y = user_rating,  colour = 'user rating')) +
    geom_point(aes(y = user_rating, colour = 'user rating')) +
    geom_label(aes(y = user_rating,
                   label = round(user_rating, digits = 1),
                   fontface = 'bold'),
               nudge_y = 0.004,
               colour = 'white',
               fill = '#009d8b',
               alpha = 0.7) +

    scale_y_log10() +
    ggtitle('Mean reating per year of the movie premiere and user rating per year') +
    xlab('Year') +
    ylab('Mean rating')

# Total users ratings per year
edx %>%
    group_by(rating_year) %>%
    summarize(total_ratings = n()) %>%
    ggplot(aes(rating_year, total_ratings)) +
    geom_freqpoly(stat = 'identity', colour = '#00007f') +
    geom_area(color = "#00007f", fill = "#07b5ff", alpha = 0.7) +
    geom_label(aes(label = total_ratings,
                   fontface = 'bold'),
               colour = 'white',
               fill = '#00007f',
               alpha = 0.6,
               nudge_y = 35000) +
    xlab('Year') +
    ylab('Total ratings')
