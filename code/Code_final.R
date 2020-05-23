####################################################################
# Advanced Programming Languages Group Project
# Movie Recommendation System
# Team Members: Lorenzo Dei Tos, Federico Farhanghi, Valerie Großrieder,
#               Roman Kastl, Nils Kaufmann, Marko Kosutov
####################################################################

# The base code for our project comes from Data Flair
# https://data-flair.training/blogs/data-science-r-movie-recommendation/

####################################################################
# Part 1: Data Preparations
####################################################################

# install packages
install.packages('plotly')
install.packages("ggthemes")
install.packages("scales")
install.packages('datetime')
install.packages("recommenderlab")
install.packages("ggplot2")
install.packages("data.table")
install.packages("reshape2")
install.packages("stringr")
install.packages("tree")
install.packages("class")
install.packages("plyr")
install.packages("formattable")

# In a first step, we load the libraries needed for our analysis
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(stringr)
library(tree)
library(class)
library(plotly)
library(ggthemes)
library(datetime)
library(plyr)
library(scales)
library(formattable)

# load raw data from csv files
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
movie_data
rating_data <- read.csv("ratings.csv")
rating_data

# Our basic assumption is that users like movies when they assign them a rating of 3.5 or better
rating_data$like <- ifelse(rating_data$rating >= 3.5, 1, 0)
rating_data$timestamp <- as.date(rating_data$timestamp)

# We split the original title column into title and year and assign movies from 1970 or older the label "classic"
movie_data$year <- as.numeric(str_sub(movie_data$title,-5,-2))
movie_data$title <- str_sub(movie_data$title, end = -7)
movie_data$classic <- ifelse(movie_data$year <= 1970, 1, 0)

# Next, we split the movie genre column into several columns
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]',
                                        type.convert=TRUE),
                              stringsAsFactors=FALSE)
colnames(movie_genre2) <- c(1:10)

# The genre list is created to label the genres
list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

# Moreover, we create an empty (zero) matrix for all 10325 movies (+5 additional lines) and 18 genres
genre_mat1 <- matrix(0,10330,18)

# The first line of the genre matrix is filled with the genre names and the columns are labeled accordingly
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

# We loop through all elements of the movie genre matrix and assign value 1 to each genre of the movies
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
    genre_mat1[index+1,gen_col] <- 1
  }
  }

# Finally, we remove the first row (which was the genre list) and convert characters to integers to be able to
# perform calculations with the binary variables
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE)
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

# Now we can combine the movie raw data with the genre matrix and get a large movie data frame
movie_data <- as.data.frame(cbind(movie_data,genre_mat2))

# This data frame we combine with the rating data to get an aggregated data frame with all information
all_data <- as.data.frame(merge(rating_data, movie_data, by = "movieId"))

# We can now count how often each movie has been reviewed and and calculate average ratings
no_ratings <- as.data.frame(table(all_data$movieId))
colnames(no_ratings) <- c("movieId", "reviews")
avg_ratings <- aggregate(. ~ movieId, data=all_data[,c("movieId","rating")],mean)
colnames(avg_ratings) <- c("movieId","avg.rating")

# This information we add to the movie data and aggregated data frame
movie_data <- merge(avg_ratings,movie_data, by = "movieId")
movie_data <- merge(no_ratings, movie_data, by = "movieId")
all_data <- merge(all_data, avg_ratings, by = "movieId")
all_data <- merge(all_data, no_ratings, by = "movieId")

# We are of course also interested in the rating behavior of individual users as ultimately our goal 
# is to make recommendations to them. Therefore we create a table containing the number of reviews as well as
# their favorite genre
user_data <- as.data.frame(table(all_data$userId))
colnames(user_data) <- c("userId", "reviews")
user_data$topuser <- ifelse(user_data$reviews >= quantile(user_data$reviews, 0.90), 1,0)
user_genres <- aggregate(. ~ userId, data=all_data[,c(2,10:27)], sum)
user_genres$favorite_genre <- colnames(user_genres[,-1])[apply(user_genres[,-1],1,which.max)]
user_data <- merge(user_data, user_genres, by = "userId")

# Also, we compute the average rating assigned by the user to each genre and identify the best rated genre
genre_mat3 <- all_data$rating * all_data[,c(10:27)]
genre_mat3$userId <- all_data$userId
user_ratings <- aggregate(. ~ userId, data=genre_mat3, sum)
user_ratings <- merge(user_data, user_ratings, by = "userId")
user_avg_ratings <- user_ratings[,c(23:40)]/user_ratings[,c(4:21)]
user_avg_ratings[is.na(user_avg_ratings)] <- 0
colnames(user_avg_ratings) <- list_genre
user_avg_ratings$best_rated_genre <- colnames(user_avg_ratings)[apply(user_avg_ratings,1,which.max)]
user_avg_ratings$userId <- user_ratings$userId

# We combine the information on the user in a user data frame
user_data <- merge(user_data, user_avg_ratings[,c("userId","best_rated_genre")], by = "userId")

# the most important information on the user is added to the aggregated data frame
user_data_consolidated <- merge(user_data[,c("userId","topuser","favorite_genre")], 
                                user_avg_ratings[,c("userId","best_rated_genre")], by = "userId")
all_data <- merge(all_data, user_data_consolidated, by = "userId")

# We conclude our data preparation by adding columns that indicate whether a movie rated belongs
# to the user's favorite or best rated genre
all_data$is_best_rated_genre <- 0
  for (i in 1:nrow(all_data)){
  current_genre <- all_data[i,"best_rated_genre"]
  all_data[i,"is_best_rated_genre"] <- all_data[i,current_genre]
}

all_data$is_favorite_genre <- 0
for (i in 1:nrow(all_data)){
  current_genre <- all_data[i,"favorite_genre"]
  all_data[i,"is_favorite_genre"] <- all_data[i,current_genre]
}

all_data$likes_genre <- ifelse(all_data$is_favorite_genre == 1 | all_data$is_best_rated_genre == 1,
                               1,0)

####################################################################
# Part 2: Data Analysis
####################################################################

# Before we start with our movie recommendation algorithm, we will perform a detailed analysis of the dataset

# DESCRIPTION OF DATA: 
# We start to get a glimpse of the structure our dataset
str(all_data) 
dim(all_data)
head(all_data)
# Following, we compute three main values of interest: total ratings, total movies and total users and report them in a table
total_ratings <- nrow(rating_data)
total_movies <- nrow(movie_data)
total_users <- nrow(user_data)

descriptives <- as.data.frame(x = c(total_ratings, total_movies,total_users), row.names = c('Total ratings', 'Total movies', 'Total users'))
colnames(descriptives) <- 'Values'

formattable(descriptives, align = 'l')

# Next, we might be interested in understanding which years the movies in our dataset are from.
# To visualize this, we produce a line chart that shows in which years the largest number of the movies in our dataset are concentrated 

movies_by_year <- count(movie_data,'year')
plot(movies_by_year, xlab = "Year", ylab = "Movies in this year", main = "Distribution of movies by year")

graph_distr_year <- ggplot(movies_by_year, aes(x = year, y = freq)) + geom_area(fill = 'dodgerblue2', alpha = 0.5) + geom_line(color = "midnightblue") + ggtitle("Distribution of movies by year") + xlab("Year") + ylab("Movies in this year") + theme_clean()
graph_distr_year

# INTERPRETATION
# The chart shows that the bulk of the movies in our sample seems to be concentrated between the late 90s and early 2000s. This suggests that most of the movies in our sample are relatively recent.

# Description of ratings
# Let us now have a look at the distribution of the ratings (mean, median , std. deviation, frequency). The
# avg. rating of 3.5 is used to separate movies the users liked from those they didn't like. The rating table 
# also shows that roughly 60% of all ratings are in the range [3,4] and only 1 in 7 movies gets a perfect rating
mean(rating_data$rating)
median(rating_data$rating)
sd(rating_data$rating)
rating_table <- as.data.frame(table(rating_data$rating))
rating_table$fraction <- round(((rating_table$Freq/total_ratings)*100), digits = 4)
colnames(rating_table) <- c('rate', 'freq', 'fraction')
rating_table

# We plot a line chart that makes it easier to visualize the most prevalent ratings in the sample
graph_ratings <- ggplot(rating_table, aes(x = rate, y = fraction, group = 1)) + geom_line(size= 1, color = 'dodgerblue2') + geom_point(shape = 23, color = 'dodgerblue2', fill = 'white', size = 3) + 
  theme_clean() + xlab("Rate") + ylab('Percentage') + ggtitle('Rating prevalence', subtitle = 'Mid/high ratings seem to be prevalent in the sample')

graph_ratings

# INTERPRETATION: 
# The line chart shows that, in our sample, movie ratings of 3 and 4 seem to be prevalent. This means that most of the movies reviewed received a mid/high rating from viewers.
# Notice that very high ratings and very low ones occur much less often. 


# We now know the distribution of the ratings in the dataset. But which movies are the most popular in terms of 
# a) number of ratings and b) their average rating? To find this out we sort our movie data by reviews and average
# rating. Note that for b) we only consider representative movies with at least 20 reviews

# a) What are the most reviewed movies? 
# We create a subset of our data that we call 'top_20_reviewed' to capture the 20 most reviewed movies
top_20_reviewed <- movie_data[,c("title", "reviews")]
# We also aggregate to this subset the information from the 'like' column from our dataset, which captures wheter the rating of the movie review was above 3.5. For each title,
# we consider the sum of the positive reviews
like_data <- all_data[, c("title","like")]
like_data <- aggregate(like ~ title,like_data, sum)

top_20_reviewed <- merge(top_20_reviewed, like_data, by = 'title')
top_20_reviewed$like <- ((top_20_reviewed$like/top_20_reviewed$reviews)*100) #  we consider the number of positive reviews in % of the total reviews

top_20_reviewed <- top_20_reviewed[order(-top_20_reviewed$reviews),] # we apply a decreasing order depending on the number of reviews
top_20_reviewed <- head(top_20_reviewed, n=20) #  we only consider the first 20 entries
top_20_reviewed$title <- str_wrap(top_20_reviewed$title, width = 50) #  We wrap the text of the longest movie titles to make them fit better in the graph

# We now plot a bar chart that shows on the x-axis the number of reviews and on the y-axis the title of the movie. We fill the bars with a gradient that shows number of positive reviews
# as a % of the total reviews
graph_top_ten_titles <- (ggplot(data = top_20_reviewed, aes(x = reorder(title, reviews), y = reviews, fill = like) ) +    # print bar chart
                           geom_bar(stat = 'identity') + theme_clean() + ggtitle("Most reviewed movies", subtitle = 'Top 20 movies by number of reviews') + 
                           xlab("Movie title") + ylab("Number of reviews") + coord_flip() + 
                           theme(axis.text.y = element_text(angle = 360, lineheight = 0.8, size = 10)))
graph_top_ten_titles

# INTERPRETATION: 
# This chart depicts a list of the most reviewd movies. We can see that the top 5 movies by number of reviews are "Pulp Fiction", "Forrest Gump", "The Shawshank Redemption", "Jarassic Park" and "The Silence of the Lambs". 
# These movies have received a number of reviews in excess of 250. The gradient fill of the bars shows the number of positive reviews given to the movie i.e., those reviews that rated the movie above 3.5/5 as a %
# of the total number of reviews. As we move from the top (most reviewed) to the bottom of the graph, we see the gradient turning progressively darker, indicating a lower number of positive reviews. 
# The movie with the lowest number of positive reviews, among the top reviewed ones, seems to have been "Batman". 

# b) What movies have the highest average rating?
a <- which(movie_data$reviews >= 20) # consider only the movies with a number of reviews greater than 20
relevant_movies <- movie_data[a,]
relevant_movies
movie_ranking <- relevant_movies[order(relevant_movies$avg.rating, decreasing = TRUE),]
top_20_ranked_movies <- head(movie_ranking, n=29)
top_20_ranked_movies$avg.rating <- round(top_20_ranked_movies$avg.rating, digits = 2) # round the average rating to 2 decimals
top_20_ranked_movies <- top_20_ranked_movies[, c('title', "avg.rating")]
colnames(top_20_ranked_movies) <- c('Title', 'Average rating')

# We produce a table to visualize the ranking of the best rated movies
formattable(top_20_ranked_movies, align = c('l', 'l'))

# INTERPRETATION
# The table reports the top 20 movies by average rating, considering only representative movies with at least 20 reviews. Notice that all of the reported movies have an average rating in excess of 4,
# with the highest rating being 4.48. It is also interesting to see that the titles appearing on this table look quite different from the list of most reviewed movies. While we have seen that movies with 
# with a high number of reviews also typically have a high number of positive ratings, these do not seem to be necessarily the top rated ones. 

# However, we might be interested to know how ratings are related the number of reviews. To do so, we can perform regression analysis to establish their relationship. 
reviews_popularity <- lm(avg.rating ~ reviews, data = relevant_movies)
summary(reviews_popularity) # we print a summary table with the results of the regression

plot(relevant_movies$reviews,relevant_movies$avg.rating, xlab = "Number of Reviews",
     ylab = "Avg. Rating", main = "Relationship between reviews and ratings", col = 'dodgerblue2') # we plot the average rating against the number of reviews

abline(reviews_popularity, col = 'red') # we add a regression line to the plot

# INTERPRETATION
# As we can see from the graph, movies with a high rating are reviewed more often. This seems to indicate that recommendations by other viewers have an impact on what movies we watch. 

# Having looked at the ratings and reviews at a movie level we take a step back and turn to the ratings of movie genres.
# To do this, we create a lollipop chart depicting the most rated genres in our dataset.
genre_popularity <- as.data.frame(sapply(all_data[,c(10:27)], sum))
genre_popularity <- as.data.frame(cbind(list_genre, genre_popularity))
colnames(genre_popularity) <- c('genre', 'reviews')
genre_popularity$genre <- reorder(genre_popularity$genre, genre_popularity$reviews) # apply a decreasing order to the genre column

graph_top_genres <- ggplot(genre_popularity, aes(x = genre, y = reviews)) + geom_segment(aes(x=genre, xend=genre, y=0, yend=reviews), 
                                                                                         color = 'dodgerblue2') + geom_point(size=5, color = 'dodgerblue2') + ylab('No. of reviews') + xlab('Genre') + 
  theme_clean() + theme(panel.background = element_blank(), axis.ticks.y = element_blank(), axis.line.y = 
                          element_blank(), axis.text.y = element_text(angle = 360, lineheight = 0.8, size = 9)) + 
  ggtitle("Most reviewed genres") + coord_flip()

graph_top_genres

# INTERPRETATION
# We find that Drama, Comedy and Action are the most reviewed genres in the dataset. 

####################################################################
# Part 3: Decision Tree Algorithm
####################################################################

# Decision Tree Algorithm

# We start our analysis with a simple decision tree algorithm. The goal of this algorithm is to identify 
# movie characteristics that are helpful for making recommendations to new / unknown users

# We first select the features and movies that shall be considered by the decision tree algorithm
b <- which(all_data$reviews >=25)
tree_data <- all_data[b,c(5,9,28,29,30,33,34,35)]

# In the next step the tree is created and we obtain the summary of the decision tree characteristics
movie_tree <- tree(like~.,tree_data)
summary(movie_tree)

# Finally, we plot the decision tree
plot(movie_tree)
text(movie_tree)

cat("Interpretation: the algorithm only uses the average rating as a criterion,
indicating that this criterion dominates other criteria. Thus, whether
the movie belongs to the user's favorite genre, whether it is an old movie
and the number of reviews as an indicator for popularity seem to be a far 
less important factors for whether a user will like a movie than the
assessment by the community. If a movie has an average rating of e.g. > 3.87,
the probability that a given user will like it is about 83.2%. Therefore a 
simple approach would be to recommend the best rated movies, regardless of
the genre etc.")

####################################################################
# Part 4: Movie Recommendation
####################################################################

# Our goal in this section is to recommend movies to given users based on their movie reviews
# We use a kNN algorithm and a logistic regression to make predictions. Then we compare our
# recommendations to the ones that are based on the recommenderlab package

# KNN algorithm

# Our kNN algorithm predicts whether a user will like a movie that he/she has not watched yet by 
# identifying the k most similar movies (with respect to features like the average rating by the
# community and the number of reviews) which the user has already watched. It computes the "like"
# probability as the fraction of the k movies the user liked. We recommend those movies with the 
# highest like probability

# Here we define the user for whom the recommendation is made
userId <- 1

# We only consider movies with at least 20 reviews in our algorithm
knn_data <- relevant_movies

# We identify movies the user has already watched
watched_movies <- all_data[which(all_data$userId == userId),"movieId"]
knn_data$watched <- ifelse(is.element(knn_data$movieId,watched_movies), 1, 0)

# Also, we need to identify the movies user liked
liked_movies <- all_data[which(all_data$userId == userId & all_data$like == 1),"movieId"]
knn_data$like <- ifelse(is.element(knn_data$movieId,liked_movies), 1, 0)

# Moreover, we add two features (favorite and best rated genre) which are used as additional criteria
# by the algorithm to better identify similar movies
favorite_genre <- user_data[which(user_data$userId == userId),"favorite_genre"]
best_rated_genre <- user_data[which(user_data$userId == userId),"best_rated_genre"]
knn_data$is_favorite_genre <- ifelse(knn_data[,favorite_genre], 1, 0)
knn_data$is_best_rated_genre <- ifelse(knn_data[,best_rated_genre], 1, 0)

# Before we run our algorithm we the split the data into a training (rated movies) and a prediction data 
# set (movies not rated). In addition, we specify the features that shall be included in the analysis
data.train <- knn_data[which(knn_data$watched == 1),
                       c("like","avg.rating","reviews","is_best_rated_genre","is_favorite_genre")]
data.predict <- knn_data[which(knn_data$watched == 0),
                      c("like","avg.rating","reviews","is_best_rated_genre","is_favorite_genre")]

# We now run the knn algorithm and store the "like" probabilities
knn_output <- knn(data.train[, colnames(data.train) != 'like'],
                  data.predict[, colnames(data.predict) != 'like'],
                  data.train[, 'like'],k=round(sqrt(nrow(data.train))) ,prob=TRUE)
knn_output
knn_probabilities <- attributes(knn_output)$prob

# We combine the titles with the probabilities from knn, sort them and recommend the movies with the highest prob.
recommendation <- as.data.frame(cbind(knn_data[which(knn_data$watched == 0),"title"], knn_probabilities))
colnames(recommendation) <- c("title","probability")
recommendation <- recommendation[order(recommendation$probability, decreasing = TRUE),]
recommended_titles <- recommendation[1:10,"title"]
recommended_titles

# Logistic Regression

# Like in the knn algorithm we compute the likelihood that a user will enjoy the movies he/she
# has not watched yet, however, this time we use a logistic regression

# Perform a logistic regression on the training data (reviewed movies) to obtain the coefficients for the 3 features
logistic_regression <- glm(like~avg.rating + reviews + is_favorite_genre,data=data.train,family=binomial)
summary(logistic_regression)

# Then we predict the "like" probabilities for the movies not watched thus far
glm.probs <- as.data.frame(predict(logistic_regression,data.predict,type ="response"))

# Again, we combine the data, sort it and make our recommendation based on the highest like probabilities
log_reg_recommendations <- cbind(knn_data[which(knn_data$watched == 0),"title"],glm.probs)
colnames(log_reg_recommendations) <- c("title","probability")
log_reg_recommendations <- log_reg_recommendations[order(log_reg_recommendations$probability, decreasing = TRUE),]
head(log_reg_recommendations)

# Predictions based on the recommenderlab library

# First we create a rating matrix based on the rating data with userIds as rows and movieIds as columns  
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) # remove userIds

# The rating matrix is converted into a recommenderlab sparse matrix, so it becomes useable for the recommendation system
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix

# There are several options for creating a recommendation system. The following lines give an overview of those options
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)
lapply(recommendation_model, "[[", "description")

# We will make use of the IBCF ("Item Based Collaborative Filtering Approach")
# In contrast to the approaches used before, this model makes suggestions to users based on the preferences of other users
recommendation_model$IBCF_realRatingMatrix$parameters

# Thus, we need to establish the similarity between users based on our rating matrix
# Here we consider 4 users and make use of the cosine operator, then visualize the similarities
similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")

# Similarly we find similarities between movies
movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies similarity")

# The rating matrix also allows us to create visualizations of the dataset
# e.g. we can create a heatmap of ratings for the first 25 rows and columns
image(ratingMatrix[1:20, 1:25], axes = FALSE, main = "Heatmap of the first 25 rows and 25 columns")

# Let us prepare the data for our movie recommendation system
# We only use representative movies and users with at least 20 ratings
movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 20,
                              colCounts(ratingMatrix) > 20]
movie_ratings
# This reduces the data from 668 users and 10325 movies to 649 users and 1261 reviews

# In the following we have a look only at the top 2% of those users and their ratings of the 2% of moview with the most reviews
minimum_movies<- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
                    colCounts(movie_ratings) > minimum_users],
      main = "Heatmap of the top users and movies")

# we now normalize the data to avoid biases
normalized_ratings <- recommenderlab::normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)

image(normalized_ratings[rowCounts(normalized_ratings) > minimum_movies,
                         colCounts(normalized_ratings) > minimum_users],
      main = "Normalized Ratings of the Top Users")

# finally we binarize the data to allow our recommendation system to work more efficiently
# then we visualize good rated movies (>3.5) in a heatmap
binary_minimum_movies <- quantile(rowCounts(movie_ratings), 0.95)
binary_minimum_users <- quantile(colCounts(movie_ratings), 0.95)
good_rated_films <- binarize(movie_ratings, minRating = 3.5)
image(good_rated_films[rowCounts(movie_ratings) > binary_minimum_movies,
                       colCounts(movie_ratings) > binary_minimum_users],
      main = "Heatmap of the top users and movies")

# For our IBCF algorithm we shuffle the data and split into training (80%) and testing data (20%) 
sampled_data <- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

# Let us have a look at the parameters of the recommendation system
# Furthermore, we identify the k = 30 most similar items and store their numbers 
recommendation_system <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters
recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)

# We obtain the recommen_model using the getModel function and show its class as well as its dimension
model_info <- getModel(recommen_model)
class(model_info$sim)
dim(model_info$sim)

# Then we visualize the similarity between the top 20 items 
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heatmap of the first rows and columns")

# In the next step of the project, we will carry out the sum of rows and columns with the
# similarity of the objects above 0.
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)
sum_cols <- colSums(model_info$sim > 0)

# The top recommendation variable contains the number of items to recommend to each user
top_recommendations <- 10

# The predict function identifies similar items and ranks them using ratings as weights
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

# We now make a recommendation to the first user
user1 <- predicted_recommendations@items[[1]]
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movieId == movies_user1[index])$title)
}
movies_user2

# finally we display the recommendation matrix for each user
recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) 
recommendation_matrix[,1:4]

####################################################################
# Part 5: Perceptron algorithm
####################################################################

# The goal of this algorithm is to perform linear separation and find a line in the two-dimensional
# space based on the avg. movie rating and number of reviews (as a measure of popularity)
# separating those movies that users liked from those they did not like

' Acknowledgement: The algorithm is based on a code from Prof. Horlemann (Machine Learning 
course) which was originally applied to a breast cancer dataset and adjusted for this
movie recommendation project.'

# First we prepare the data and store the names of the features (predictors)
Data <- knn_data[which(knn_data$watched == 1),c("like","avg.rating","reviews")]
features <- c(names(Data)[2], names(Data)[3])

# Then we change the value of the binary variable "like" from [0,1] to [-1,1] i.e. the format needed by Perceptron
Data$like <- ifelse(Data$like == 0, -1, 1)

# Next, we reshuffle the dataset to give it a random order and speed up learning 
set.seed(123)
resh <- sample(1:nrow(Data))
Data <- Data[resh,]

# Moreover, we standardize the predictor values to avoid bias 
standFun <- function(x){
  out = (x - mean(x))/sd(x)
  return(out)
}

Data[[features[1]]] <- standFun(Data[[features[1]]])
Data[[features[2]]] <- standFun(Data[[features[2]]])

# In the following we define the parameters needed for our algorithm and initialize weights
eta <- 0.1
decCrit <- 0
n <- nrow(Data)
w <- rep(0, length(features) + 1)

# The iterative learning starts here
# Note: i is the running index for both the iteration and the index for cases/observations
for (i in 1:n){
  # We get the x and y values of the i-th observation...
  xi <- as.numeric(Data[features][i, ])  
  yi = Data$like[i]                      
  
  # ... and calculate the index value of the i-th observation
  index <- w[1] + w[2]*xi[1] + w[3]*xi[2]
  
  # Then, we make a prediction for whether the user likes or dislikes a movie, based on the index value
  pred <- ifelse(index>=decCrit, 1, -1)  
  
  # bookkeeping: The current values of the index, prediction, and weights are entered into the data frame
  Data$index_i[i] <- index
  Data$prediction_i[i] <- pred
  Data$bias_i[i] <- w[1]
  Data$w_avg_rating_i[i] <- w[2]
  Data$w_reviews_i[i] <- w[3]
  
  # This is the actual learning process. Here we update the weights of the algorithm
  update <- eta*(yi - pred)      # The updating factor
  w[-1] <- w[-1] + update * xi   # updating the weights for the two features
  w[1] <- w[1] + update          # updating the constant/bias
  
  # bookkeeping: Again, we store information (error)
  error <- yi - pred  
  Data$error_i[i] <- error
  Data$update_i[i] <- update
}

# Now we can make predictions for all i based on the weights from the last iteration
Data$lastIndex <- w[1] + w[2]*Data[[features[1]]] + w[3]*Data[[features[2]]]
Data$lastPrediction <- ifelse(Data$lastIndex >= decCrit, 1, -1)
Data$lastError <- Data$like - Data$lastPrediction

# Also we can calculate the fraction of misclassified cases (in % of the total sample)...
misclas <- round(sum(Data$lastError!=0)/nrow(Data)*100, digits = 1)

# ... false positives (in % of negatives) ...
x <- ifelse(Data$lastPrediction == 1 & Data$like == -1, 1, 0)
falPos <- round(sum(x)/length(Data$like[Data$like == -1])*100, digits = 1)

# and false negatives (in % of positives)
x <- ifelse(Data$lastPrediction == -1 & Data$like == 1, 1, 0)
falNeg <- round(sum(x)/length(Data$like[Data$like == 1])*100, digits = 1)

# Let use have a look at the results
decCrit
summary(Data$index_i)
misclas
falPos
falNeg

# Plotting Section (Note that it may take a couple of seconds before the graph is fully displayed)

# We first define the colors for plotting
posCol <- "blue"     # color for data points with label +1 
negCol <- "red"    # color for data points with label -1 in data
posColbg <- "#9ed9f7"  # Background color for area predicted +1
negColbg <- "#f79999"  # Background color for area predicted -1

# We use an empty plot as "canvas", to add the rest
plot(Data[[features[1]]], Data[[features[2]]], type = "n",
     xlab = features[1], ylab = features[2], cex = 1,
     main = paste0("Perceptron Learning: Errors with critical index value of ", decCrit),
     cex.main = 1.1)

# xlims and ylims can then be obtained from the empty plot
xlims <- par("usr")[1:2]; ylims = par("usr")[3:4]

# The grid of (x,y) combinations that is used for coloring prediction areas is defined next
x <- seq(xlims[1], xlims[2], 0.01)
y <- -w[1]/w[3] - w[2]/w[3]*x

xgrid <- seq(xlims[1], xlims[2], (xlims[2]-xlims[1])/300)
ygrid <- seq(ylims[1], ylims[2], (ylims[2]-ylims[1])/300)
nx <- length(xgrid); ny = length(ygrid)

xgrid <- rep(xgrid, times = ny)
ygrid <- rep(ygrid, each = nx)

# toPaint is the data frame that is used for coloring the prediction areas
toPaint <- data.frame(xgrid = xgrid, ygrid = ygrid) 

# Here we add background colors for the prediction areas onto canvas
dataptsize <- 0.7
bgptsize <- 0.5

toPaint$index <- w[1] + w[2]*xgrid + w[3]*ygrid
toPaint$prediction <- ifelse(toPaint$index >=decCrit, 1, -1 )

# Finally, for the plot we:
# First, add pbackground coloring for areas that are predicted to be "positive"
points(toPaint$xgrid[toPaint$prediction == 1], toPaint$ygrid[toPaint$prediction == 1],
       pch=c(16), col=posColbg, cex =bgptsize)

# Add packground coloring for areas that are predicted to be "negative"
points(toPaint$xgrid[toPaint$prediction == -1], toPaint$ygrid[toPaint$prediction == -1],
       pch=c(16), col=negColbg, cex =bgptsize)

# Define the "positive" data points
points(Data[[features[1]]][Data$like == 1], Data[[features[2]]][Data$like == 1], 
       pch=16, col=posCol, cex = dataptsize)

# Define the "negative" data points
points(Data[[features[1]]][Data$like == -1], Data[[features[2]]][Data$like == -1], 
       pch=16, col=negCol,  cex = dataptsize)

# and add the decision border
xx <- seq(xlims[1], xlims[2], 0.01)
yy <- decCrit/w[3] - w[1]/w[3] - w[2]/w[3]*xx
lines(xx,yy, lty = 2, col = "black", lwd = 2)

mtext(line = -1.5, 
      paste0(misclas, "\u0025 misspecified cases"), col = "white")
mtext(line = -3, 
      paste0(falPos,"\u0025 false positives -- ",
             falNeg,"\u0025 false negatives"), 
      col = "white")
