library(sqldf)
library(recommenderlab)
library(reshape2)
library(ggplot2)
setwd("C:/Users/amber/Desktop/Recommendation Engine/ml-latest-small")

dir()
#load movie data
movies<-read.csv("movies.csv")
str(movies)
# remove useless variables 
movies<-movies[,c(1:7)]
#load rating data
ratings<-read.csv("ratings.csv")
str(ratings)
head(movies)
head(ratings)
# combine the ratings and movie datasets 
mr<-sqldf("select A.userId, A.movieId, A.rating, 
                      B.title, B.genre_1, B.genre_2, B.genre_3, 
                      B.genre_4, B.genre_5 from ratings A 
                      join movies B on A.movieId=B.movieId")
nrow(mr)
names(mr)
# only keep 20,000 rows to shorten train time. 
# In production ready recommendation engine this step would be skipped
data<-mr[c(1:20000),c(1, 4, 5:9, 3)]
head(data)
# cast data into correct format as follows: 
#          t1  t2   t3   t4
# user1    3   4    2    5
# user2    1   6    5
# user3    4   4    2    5

#t1,t2...=movie titles
#values are ratings 
g<-acast(data, userId~title)
class(g)
data<-as.matrix(g)

# convert to realRatingMatrix: recommenderlab sparse-matrix
data<-as(data, "realRatingMatrix")

# view the matrix in other ways that are easier to read
as(data, "list")# view as list
as(data, "matrix")#view as matrix
head(as(data, "data.frame"))#view as data frame
# view image of ratings
image(data, main = "Raw Ratings")       

#*******USER BASED COLLABORATIVE FILTERING**************
# build a user based collaborative filtering recommendation engine 
rec_user=Recommender(data[1:nrow(data)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
# view details of the model
print(rec_user)
names(getModel(rec_user))
getModel(rec_user)$nn

# predict the top ten recommendations for each user
topten<-predict(rec_user, data[1:nrow(data)], type="topNList", n=10)
# view top ten recomendations in list format
as(topten, "list")
# predict the ratings for each movie for each user
recom<-predict(rec_user, data[1:nrow(data)], type="ratings")
# view predictions as a list
as(recom, "list")
# show ratings for first 10 items
as(recom, "matrix")[,1:10]
# see prediction for user 5 for item 3
as.integer(as(recom, "matrix")[5,3])
# convert the recommendations for a list
rec_list<-as(recom,"list")
head(summary(rec_list))
# See user 2, item 2
rec_list[[2]][2]
# convert list to a data frame for only user 1
u1<-as.data.frame(rec_list[[1]])
attributes(u1)
class(u1)
u1$id<-row.names(u1)
u1
u1[u1$id==3952,1]

# function get recommendation for specific user
predict_movie <- function(userId) {
  
  # get the customer profile from the customer-item table
  data <- data[userId,]
  data   <- as(data, "realRatingMatrix")
  
  # perform the prediction
  recom <- predict(rec_user, data,n=10)
  
  return(as(recom,"list"))
}
# get top ten recommendations for user 1
predict_movie(1)
# recommended movies:  
#[1] "Moonstruck (1987)"                                                             
#[2] "Star Wars: Episode V - The Empire Strikes Back (1980)"                         
#[3] "Lord of the Rings: The Fellowship of the Ring, The (2001)"                     
#[4] "Lord of the Rings: The Return of the King, The (2003)"                         
#[5] "Lord of the Rings: The Two Towers, The (2002)"                                 
#[6] "Emma (1996)"                                                                   
#[7] "Ronin (1998)"                                                                  
#[8] "Field of Dreams (1989)"                                                        
#[9] "Raiders of the Lost Ark (Indiana Jones and the Raiders of the Lost Ark) (1981)"
#[10] "Shawshank Redemption, The (1994)" 
user_rec<-predict_movie(1)


#********ITEM BASED COLLABORITIVE FILTERING*************
# create recommendation engine using the item based collaborative filtering method
rec_item=Recommender(data[1:nrow(data)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
# view details of the model
print(rec_item)
names(getModel(rec_item))
getModel(rec_item)$nn

# predict the top ten recommendations for each user
topten<-predict(rec_item, data[1:nrow(data)], type="topNList", n=10)
# view top ten recomendations in list format
as(topten, "list")
# predict the ratings for each movie for each user
recom<-predict(rec_item, data[1:nrow(data)], type="ratings")
# view predictions as a list
as(recom, "list")
# show ratings for first 10 items
as(recom, "matrix")[,1:10]
# see prediction for user 5 for item 3
as.integer(as(recom, "matrix")[5,3])
# convert the recommendations for a list
rec_list<-as(recom,"list")
head(summary(rec_list))
# See user 2, item 2
rec_list[[2]][2]
# convert list to a data frame for only user 1
u1<-as.data.frame(rec_list[[1]])
attributes(u1)
class(u1)
u1$id<-row.names(u1)
u1
u1[u1$id==3952,1]

# function get recommendation for specific user
predict_movie <- function(userId) {
  
  # get the customer profile from the customer-item table
  data <- data[userId,]
  data   <- as(data, "realRatingMatrix")
  
  # perform the prediction
  recom <- predict(rec_item, data,n=10)
  
  return(as(recom,"list"))
}
# get top ten recommendations for user 1
predict_movie(1)
item_rec<-predict_movie(1)


#*******POPULAR FILTERING*******************************
rec_pop=Recommender(data[1:nrow(data)],method="POPULAR")
# view details of the model
print(rec_pop)
names(getModel(rec_pop))
getModel(rec_pop)$nn

# predict the top ten recommendations for each user
topten<-predict(rec_pop, data[1:nrow(data)], type="topNList", n=10)
# view top ten recomendations in list format
as(topten, "list")
# predict the ratings for each movie for each user
recom<-predict(rec_pop, data[1:nrow(data)], type="ratings")
# view predictions as a list
as(recom, "list")
# show ratings for first 10 items
as(recom, "matrix")[,1:10]
# see prediction for user 5 for item 3
as.integer(as(recom, "matrix")[5,3])
# convert the recommendations for a list
rec_list<-as(recom,"list")
head(summary(rec_list))
# See user 2, item 2
rec_list[[2]][2]
# convert list to a data frame for only user 1
u1<-as.data.frame(rec_list[[1]])
attributes(u1)
class(u1)
u1$id<-row.names(u1)
u1
u1[u1$id==3952,1]

# function get recommendation for specific user
predict_movie <- function(userId) {
  
  # get the customer profile from the customer-item table
  data <- data[userId,]
  data   <- as(data, "realRatingMatrix")
  
  # perform the prediction
  recom <- predict(rec_pop, data,n=10)
  
  return(as(recom,"list"))
}
# get top ten recommendations for user 1
predict_movie(1)
pop_rec <-predict_movie(1)


# compare the recommendations for all three methods
user_rec
item_rec
pop_rec
mr[mr$userId==1 & mr$rating>3,c(3:9)]
