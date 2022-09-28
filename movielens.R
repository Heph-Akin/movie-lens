##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)


table(edx$rating)
length(unique(edx$movieId))

n_distinct(edx$userId)


edx %>%
  filter(rating != 0)%>%
  group_by(genres) %>%
  arrange(genres)


edx %>%
  mutate(Drama = str_detect(genres, "Drama")) %>%
  count(Drama)


edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Rating count
rating_count <- edx %>% group_by(rating) %>%
  mutate(rating = rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) 



#The most frequently given ratings
ggplot(rating_count, mapping = aes(factor(rating), count)) +
  geom_col(aes(fill = count)) +
  labs(title = "Frequency of Ratings") +
  scale_y_continuous(name = "Rating Count",labels = scales::comma) +
  xlab("Rating") +
  guides(fill=guide_legend(title="Rating Count"))
  


colnames(edx)

table(is.na(edx))

length(unique(edx$userId))



# Genre combination count
genre_counts <-
  table(str_count(edx$genres, '\\|') + 1
        - str_count(edx$genres, 'no genres'))
par(cex = 0.7)
barplot(genre_counts, xlab = 'Number of genres', ylab = 'Count', main = 'Genres per movie')
barplot(genre_counts, xlab = 'Number of genres', ylab = 'Count',
        main = 'Genres per movie')


dummy_columns(edx[1:6,], select_columns = c('G'), split = "|")



genre_combinations <- edx %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#  Highest Average Ratings
average_per_movie <- edx %>%
  select(movieId, title, genres, rating) %>%
  group_by(movieId, title) %>%
  summarise(average_rating = mean(rating), count = n()) %>%
  arrange(desc(average_rating))

head(average)
tail(average)

  
average %>%
  ggplot(aes(count, average_rating)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlab("Number of Ratings per Movie") +
  ylab("Average Ratings per Movie")

# ratings per user
average_per_user <- edx %>%
  group_by(userId) %>%
  summarise(average_rating = mean(rating), count = n()) %>%
  arrange(desc(count))

average_per_user %>%
  ggplot(aes(count, average_rating)) +
  geom_point(aes()) +
  geom_smooth(method = lm) +
  xlab("Number of Ratings per User") +
  ylab("Average Ratings per User") 



year(as.POSIXct(edx$timestamp[1:3], origin = "1970-01-01"))

as.Date(as.POSIXct("2013-01-01 07:00",tz="Hongkong"),tz="Hongkong")

#Time
date <- edx %>%
  mutate(date = year(as.POSIXct(timestamp, origin = "1970-01-01"))) %>%
  group_by(movieId, title) %>%
  summarise(count = n(), average_rating = mean(rating), date = date)
  


date %>%
  ggplot(aes(date, average_rating)) +
  geom_point() +
  #expand_limits(y = 0) +
  geom_smooth(method = lm) +
  xlab("Year") +
  ylab("Average Rating") +
  theme(axis.text.x = element_text(angle = 90, size = 10)) 

month <- edx %>%
  mutate(month = month(as.POSIXct(timestamp, origin = "1970-01-01"))) %>%
  group_by(month) %>%
  summarise(count = n(), average_rating = mean(rating))

month %>%
  ggplot(aes(month, average_rating)) +
  geom_point()


# Adding columns with insights from the EDA

# Function to get mode 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Most frequently given ratings by each user
mode_user_rating <- edx %>%
  group_by(userId)%>%
  summarise(mode_user_rating = getmode(rating))

edx <- left_join(edx, mode_user_rating, by = "userId")


# Most frequently given ratings for each movie
mode_movie_rating <-  edx %>%
  group_by(movieId) %>%
  summarise(mode_movie_rating = getmode(rating))

edx <- left_join(edx, mode_movie_rating, by = "movieId")

# Number of ratings the user has given
rating_count <- edx %>%
  group_by(userId) %>%
  summarise(rating_count = n())

edx <- left_join(edx, rating_count, by = "userId")


# Average rating per user
average_rating_user <- edx %>%
  group_by(userId) %>%
  summarise(average_rating_user = mean(rating)) 

edx <- left_join(edx, average_rating_user, by = "userId")


# Average rating per movie
average_rating_movie <- edx %>%
  group_by(movieId) %>%
  summarise(average_rating_movie = mean(rating)) 

edx <- left_join(edx, average_rating_movie, by = "movieId")


# RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}




## Simple Prediction based on Mean Rating
#mean on the training set
mu <- mean(edx_train$rating)

naive_rmse <- RMSE(edx_test$rating, mu)



#Encoding Categorical Variables
edx$title <- unclass(as.factor(edx$title))
edx$genres <- unclass(as.factor(edx$genres))



# Partitioning edx data into training and test sets
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
edx_train <- edx[-test_index,]
edx_test <- edx[test_index,]


# Linear Regression
lm_one <- lm(rating ~ userId+movieId+timestamp+genres, data = edx_train)

#Predictions with the linear model
lm_pred <- predict(lm, newdata = edx_test)

# RMSE of the linear model
lm_RMSE <- RMSE(edx_test$rating, lm_pred)

# Linear Regression with insights from the EDA
lm_new <- lm(rating ~ userId+movieId+genres+mode_user_rating+mode_movie_rating+rating_count+average_rating_user+average_rating_movie, data = edx_train)

#Predictions with the new linear model
lm_new_pred <- predict(lm_new, newdata = edx_test)

# RMSE of the new linear model
lm_new_RMSE <- RMSE(edx_test$rating, lm_new_pred)

# Getting the equation for the new model
equatiomatic::extract_eq(lm_new)



# Adding EDA Features to validation data set
# Most frequently given ratings by each user
validation_mur <- validation %>%
  group_by(userId)%>%
  summarise(mode_user_rating = getmode(rating))

validation <- left_join(validation, validation_mur, by = "userId")


# Most frequently given ratings for each movie
validation_mmr <-  validation %>%
  group_by(movieId) %>%
  summarise(mode_movie_rating = getmode(rating))

validation <- left_join(validation, validation_mmr, by = "movieId")

# Number of ratings the user has given
validation_rc <- validation %>%
  group_by(userId) %>%
  summarise(rating_count = n())

validation <- left_join(validation, rating_count, by = "userId")


# Average rating per user
validation_aru <- validation %>%
  group_by(userId) %>%
  summarise(average_rating_user = mean(rating)) 

validation <- left_join(validation, validation_aru, by = "userId")


# Average rating per movie
validation_arm <- validation %>%
  group_by(movieId) %>%
  summarise(average_rating_movie = mean(rating)) 

validation <- left_join(validation, validation_arm, by = "movieId")

#Encoding Categorical Variables for Validation Dataset
validation$genres <- unclass(as.factor(validation$genres))


# Validation of the Final model
validation_pred <- predict(lm_new, newdata = validation)

validation_RMSE <- RMSE(validation$rating, validation_pred)