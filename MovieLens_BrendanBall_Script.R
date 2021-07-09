# Title: "MovieLens Project"
# Author: "Brendan Ball"
# Date: "7/9/2021"

# Introduction

# Installation of packages if needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Use the library commands
library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Download the temporary file (If no file is present)
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Create a movies dataframe
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), 
                          "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# If using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

# If using R 3.5 or earlier, use `set.seed(1)`
suppressWarnings(set.seed(1, sample.kind="Rounding")) 

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

# Determine the class of the edx data
class(edx)

# Glimpse the data set of "edx"
glimpse(edx)

# Inspect the result summaries of the results
summary(edx)

# Results

# Explore top individual genres 
genre_order <- edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Create a table to list the top genres and the 
table_genre <- data.table(genre_order, rownames = FALSE, filter="top")
table_genre <- table_genre[,c("genres","count")]  
table_genre

# Rank the Top 10 Movie Titles
ranked_titles <- edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(10,count) %>%
  arrange(desc(count))

# Plot a bar graph of the top ranked titles
ranked_titles %>% 
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity', fill="dark green", color = "black") + 
  coord_flip(y=c(0, 35000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.2, size=3) +
  labs(title="Top 10 Movies Based on Ratings" , caption = "[1]")

# Output the Top 10 Titles, and its Corresponding Titles and Count
kable(head(edx %>% group_by(title,genres) %>%
             summarize(count = n()) %>%
             top_n(10,count) %>%
             arrange(desc(count)) ,
           10)) %>%
  kable_styling(bootstrap_options = "basic", full_width = NULL , 
                position ="center") %>%
  column_spec(1, bold = T) %>%
  column_spec(2) %>%
  column_spec(3)

# Number of User Ratings by UserId
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "orange", color = "black", bins = 10) +
  scale_x_log10() +
  ggtitle("Number of Movies Ratings by UserId") +
  xlab("SemilogX of User Id") +
  ylab("Number of Ratings")

# Generate a group between whole star and half star results
stars_grouping <-  ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 | 
                             edx$rating == 4 | edx$rating == 5) ,
                          "whole star", 
                          "half star") 

# Create a data frame to use for the plot generation
stars_ratings <- data.frame(edx$rating, stars_grouping)

# Generating a Histogram comparing the distribution
ggplot(stars_ratings, aes(x= edx.rating, fill = stars_grouping)) +
  geom_histogram( binwidth = 0.5, color = "black") +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5)) +
  scale_fill_manual(values = c("half star"="blue", "whole star"="orange")) +
  labs(x = "Rating (Out of 5)", y = "Number of Ratings (count)", 
       caption = '[1]') +
  ggtitle("Distribution of Whole and Half Star Ratings")

# Generate a plot of the timestamp, with x axis being year, y being ratings
edx %>% 
  mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) +
  ggtitle("Time Stamp of the Ratings by Year")+
  labs(x = "Year", y = "Ratings (Out of 5)", caption = "[1]") +
  ylim(1, 5) + 
  geom_hline(yintercept=3, linetype="dashed", 
             color = "red", size=1) +
  geom_hline(yintercept=4, linetype="dashed", 
             color = "red", size=1)

# Define the RMSE Function
RMSE <- function(ratings_T, ratings_P){
  sqrt(mean((ratings_T - ratings_P)^2))
}

# Observing the RMSE based on lambdas value
lambdas <- seq(0,5,.5)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- edx %>%
    left_join(b_i, by='movieId') %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() +l))
  
  ratings_P <- edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i +  b_u) %>% .$pred
  
  return(RMSE(ratings_P, edx$rating))
})

qplot(lambdas, rmses) + xlab("Lambdas") + ylab("RMSEs")

# Using the which.min() command to determine the lambdas value
print(c("The RMSEs value is:", round(min(rmses), 5)))
print(c("The lambdas (l) value is:", lambdas[which.min(rmses)]))

# Use the Model on the Validation Data for Confirmation
mu <- mean(validation$rating)
l <- 0.5
b_i <- validation %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + l))

b_u <- validation %>%
  left_join(b_i, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() +l))

ratings_P <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE(ratings_P, validation$rating)

# References
# 1. F. Maxwell Harper and Joseph A. Konstan. 2015. The MovieLens Datasets: 
#  History and Context. ACM Transactions on Interactive Intelligent Systems 
# (TiiS) 5, 4, Article 19 (December 2015), 19 pages. DOI=http://dx.doi.org/10.1145/2827872

















