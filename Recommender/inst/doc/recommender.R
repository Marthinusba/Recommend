## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(tidyverse)
library(digest)
library(Recommender)
library(NNLM)
load("book_ratings.Rdata")

## ------------------------------------------------------------------------
book_info<-book_info[!duplicated(book_info$Book.Title),] #remove duplicates
ratings_user<-inner_join(book_info,book_ratings) #create database of user ratings and book information
 #create matrix of movie ratings per user
read_book <- ratings_user %>%
  mutate(seen = ifelse(!is.na(Book.Rating),1,0)) %>% 
  select(User.ID, Book.Title, seen)%>%
  spread(key = Book.Title, value = seen)
read_book<- read_book  %>%  mutate_all(funs(replace(.,is.na(.),0))) #replac NA's with 0
sorted_my_users <- as.character(unlist(read_book[,1]))
read_books<- as.matrix(read_book[,!(names(read_book) %in% c("User.ID"))])



## ------------------------------------------------------------------------
# get item similarity for certain user

recommend(read_books,13,type = 'item')


## ------------------------------------------------------------------------
new_user <- data.frame(User.ID =300000, ISBN= c("0440234743", "0971880107", "0345417623"), Book.Rating = as.integer(c(2, 5, 3)))

new_book_ratings<-inner_join(new_user,book_info)
new_ratings_user<-rbind(new_book_ratings,ratings_user)

new_read_book <- new_ratings_user %>%
  mutate(seen = ifelse(!is.na(Book.Rating),1,0)) %>% 
  select(User.ID, Book.Title, seen)%>%
  spread(key = Book.Title, value = seen)
new_read_book<- new_read_book  %>%  mutate_all(funs(replace(.,is.na(.),0))) #replac NA's with 0
new_sorted_my_users <- as.character(unlist(new_read_book[,1]))
new_read_books<- as.matrix(new_read_book[,!(names(new_read_book) %in% c("User.ID"))])

recommend(new_read_books,149,type='item')

## ------------------------------------------------------------------------
# get user similarity for certain user
recommend(read_books,13,type = 'user',raw_data = read_book)
#raw_data is a data table without the user id removes to cross reference the books against the users.

## ------------------------------------------------------------------------
# get item similarity for certain user
recommend(read_books,13,type = 'item2')



## ------------------------------------------------------------------------

read_books_fac <- ratings_user %>% 
  select(User.ID, Book.Title, Book.Rating)%>%
  spread(key = Book.Title, value = Book.Rating)%>%
 mutate_all(funs(replace(.,is.na(.),0)))

read_books_fac<- as.matrix(read_books_fac[,!(names(read_books_fac) %in% c("User.ID"))])

set.seed(123)

k <-35;
init <- list(W = matrix(runif(nrow(read_books_fac)*k), ncol = k),
    H = matrix(runif(ncol(read_books_fac)*k), nrow = k))

nnmf(read_books_fac,k, init = init, max.iter = 10000,method = 'scd',loss = 'mkl')



predicted_ratings <- init[['W']] %*% init[['H']]
matrix_fac<-as.matrix(rbind(round(predicted_ratings[1,], 1), as.numeric(read_books_fac[15,])))
matrix_fac
predicted_ratings<-as.matrix(predicted_ratings)
colnames(predicted_ratings)<-colnames(read_books)
rownames(predicted_ratings)<-rownames(read_books)



## ------------------------------------------------------------------------
data<-predicted_ratings
criteria <- 13
 cosine_sim <- function(a, b){crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))} #function to calculate similarity
    item_similarities_fac = matrix(0, nrow=ncol(data),ncol=ncol(data)) #create blank place holder matrix
     item_matrice_fac<-matrix(rep(data[,criteria],times=ncol(data) ),ncol = ncol(data),nrow = nrow(data),byrow = TRUE) #fil matrix with one item ratings
     item_similarities_fac <- cosine_sim(item_matrice_fac,data) #calculate the similarities between user matrix and the rest of the database ratings
     item_similarities_fac<-as.matrix(diag(item_similarities_fac)) #the diagonal values of matrix is the similarity bewteen chosen item and rest of the items

     colnames(item_similarities_fac) <- 'user score' #column name of the similarity vecetor
     rownames(item_similarities_fac) <- colnames(data) #row names of the similarity vector

  item_similarities_fac<-as.data.frame(item_similarities_fac)
   item_similarities_fac%>%mutate(title =colnames(data))%>% arrange(desc(item_similarities_fac$`user score`)) %>%head(10)

