Recommend
================
Marthinus Basson
8/22/2018

## R Markdown

Problem: Given a database of books and the ratings by users of books
they have read, create a recommendation of 10 books to a new or existing
user. The recommendation should be based on item-based collabrative
filtering and user based colabrative filtering.

Load the required packages and the database retaining the user info and
book info. Recommender package contain the functions used fot
collabrative
    filtering.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.2.0
    ## ✔ readr   1.1.1     ✔ forcats 0.2.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(digest)
library(Recommender)
library(NNLM)
```

    ## Warning: package 'NNLM' was built under R version 3.4.4

``` r
load("book_ratings.Rdata")
```

\#\#\#Item-based colabrartive filtering\#\#\#

First we need to create a matrix of the ratings by each user. There is
duplicate entries for some books in the database of book information, so
first the duplicates needs to removed. Then a the data table of book
info and user info are joined together. Then a matrix is created with
all the ratings from users againts each book read. The ratings are
changed with 1 if it has a rating or a 0 if the user has not read the
book. The user id column is removed from the matrix to have it only
contain the
ratings.

``` r
book_info<-book_info[!duplicated(book_info$Book.Title),] #remove duplicates
ratings_user<-inner_join(book_info,book_ratings) #create database of user ratings and book information
```

    ## Joining, by = "ISBN"

``` r
 #create matrix of mbook ratings per user
read_book <- ratings_user %>%
  mutate(seen = ifelse(!is.na(Book.Rating),1,0)) %>% 
  select(User.ID, Book.Title, seen)%>%
  spread(key = Book.Title, value = seen)
read_book<- read_book  %>%  mutate_all(funs(replace(.,is.na(.),0))) #replac NA's with 0
sorted_my_users <- as.character(unlist(read_book[,1]))
read_books<- as.matrix(read_book[,!(names(read_book) %in% c("User.ID"))])
```

The follwing will recommend a list of movies according to the exisitng
user based on item based recommendation. The item recommdation was done
by creating a empty matrix the same size as the user matrix. The
similarity score is calcaulted between the user and the book rating
matrix using the cosine equation. The resulting matrix contains the
score bewteen each book and how similar it is to the chosen user in its
diagonal row. These values are extracted and the resulting score are
ranked in descending order. The top 10 books, which the user has not
read, are then displayed.

``` r
# get item similarity for certain user

recommend(read_books,13,type = 'item')
```

    ##                                                               title
    ## 1           the perfect storm : a true story of men against the sea
    ## 2                                                    kiss the girls
    ## 3                                         the lovely bones: a novel
    ## 4                                                      the brethren
    ## 5                                                     the testament
    ## 6                                            the kitchen god's wife
    ## 7                                                  while i was gone
    ## 8  harry potter and the sorcerer's stone (harry potter (paperback))
    ## 9                                           the secret life of bees
    ## 10                                                      good in bed
    ##         score
    ## 1  0.05394706
    ## 2  0.04790039
    ## 3  0.04717541
    ## 4  0.04710842
    ## 5  0.04605263
    ## 6  0.04401268
    ## 7  0.04330127
    ## 8  0.04319772
    ## 9  0.04131119
    ## 10 0.04124844

This is to calculate for a new user. First the new user is added to the
database and the recommendation is calculated for new
user.

``` r
new_user <- data.frame(User.ID =300000, ISBN= c("0440234743", "0971880107", "0345417623"), Book.Rating = as.integer(c(2, 5, 3)))

new_book_ratings<-inner_join(new_user,book_info)
```

    ## Joining, by = "ISBN"

    ## Warning: Column `ISBN` joining factor and character vector, coercing into
    ## character vector

``` r
new_ratings_user<-rbind(new_book_ratings,ratings_user)

new_read_book <- new_ratings_user %>%
  mutate(seen = ifelse(!is.na(Book.Rating),1,0)) %>% 
  select(User.ID, Book.Title, seen)%>%
  spread(key = Book.Title, value = seen)
new_read_book<- new_read_book  %>%  mutate_all(funs(replace(.,is.na(.),0))) #replac NA's with 0
new_sorted_my_users <- as.character(unlist(new_read_book[,1]))
new_read_books<- as.matrix(new_read_book[,!(names(new_read_book) %in% c("User.ID"))])

recommend(new_read_books,149,type='item')
```

    ##                                              title      score
    ## 1                                      wild animus 0.14470109
    ## 2                        the lovely bones: a novel 0.12459887
    ## 3                       interview with the vampire 0.10859807
    ## 4                                the da vinci code 0.10715324
    ## 5                              angels &amp; demons 0.10223104
    ## 6                                         the firm 0.09397618
    ## 7  divine secrets of the ya-ya sisterhood: a novel 0.09105593
    ## 8                                the joy luck club 0.08946481
    ## 9                    the poisonwood bible: a novel 0.08872961
    ## 10                           house of sand and fog 0.08862601

The follwing function will return an approximate list of recommended
books for selected user, using user based \[This is not completely
correct list of recommendations, but with some work it can produce the
correct list\]. The recommender is based on the item recommender system.
It extracts the top books that are similar to that of the chosen user,
and then recommends the books those users have read and removes that
books that have been read by chosen user.

``` r
# get user similarity for certain user
recommend(read_books,13,type = 'user',raw_data = read_book)
```

    ##  [1] "the book of ruth (oprah's book club (paperback))"          
    ##  [2] "wild animus"                                               
    ##  [3] "bridget jones's diary"                                     
    ##  [4] "the nanny diaries: a novel"                                
    ##  [5] "the catcher in the rye"                                    
    ##  [6] "the queen of the damned (vampire chronicles (paperback))"  
    ##  [7] "harry potter and the chamber of secrets (book 2)"          
    ##  [8] "the bad beginning (a series of unfortunate events, book 1)"
    ##  [9] "bel canto: a novel"                                        
    ## [10] "the lovely bones: a novel"

``` r
#raw_data is a data table without the user id removes to cross reference the books against the users.
```

This function is just a different way of using item based recommendation
just for interest sake.

``` r
# get item similarity for certain user
recommend(read_books,13,type = 'item2')
```

    ##  [1] "the lovely bones: a novel"                      
    ##  [2] "the da vinci code"                              
    ##  [3] "divine secrets of the ya-ya sisterhood: a novel"
    ##  [4] "angels &amp; demons"                            
    ##  [5] "the firm"                                       
    ##  [6] "house of sand and fog"                          
    ##  [7] "the secret life of bees"                        
    ##  [8] "snow falling on cedars"                         
    ##  [9] "the bridges of madison county"                  
    ## [10] "summer sisters"                                 
    ## [11] "good in bed"

\#\#\#Matrix factorisation\#\#\# This is used to check the accuracy of
the suggested ratings for books that was not rated.

``` r
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
```

    ## Non-negative matrix factorization:
    ##    Algorithm: Sequential coordinate-wise descent
    ##         Loss: Mean Kullback-Leibler divergence
    ##          MSE: 0.6245891
    ##          MKL: 0.1679827
    ##       Target: 0.1679827
    ##    Rel. tol.: 2.31e-15
    ## Total epochs: 201
    ## # Interation: 201
    ## Running time:
    ##    user  system elapsed 
    ## 192.624   0.821 193.995

``` r
predicted_ratings <- init[['W']] %*% init[['H']]
matrix_fac<-as.matrix(rbind(round(predicted_ratings[1,], 1), as.numeric(read_books_fac[15,])))
matrix_fac
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
    ## [1,]  9.8  7.9  8.6  7.5  9.5 10.7  9.2  8.8  9.2   9.3   9.4   9.7     8
    ## [2,]  0.0  0.0  0.0  0.0  0.0  0.0  7.0  7.0  0.0   0.0   0.0   0.0     0
    ##      [,14] [,15] [,16] [,17] [,18] [,19] [,20] [,21] [,22] [,23] [,24]
    ## [1,]   7.9   9.7   9.6   7.4   8.7  10.3   8.7   8.9   8.4  10.1   8.2
    ## [2,]   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0
    ##      [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35]
    ## [1,]   8.3  10.3   8.3   9.3   8.3   8.7   8.5   7.8   8.6   9.2   8.8
    ## [2,]   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0
    ##      [,36] [,37] [,38] [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46]
    ## [1,]   7.7   8.7   8.5   9.7  11.1   9.3   8.3   8.4    11   8.7     8
    ## [2,]   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0     0   0.0     0
    ##      [,47] [,48] [,49] [,50] [,51] [,52] [,53] [,54] [,55] [,56] [,57]
    ## [1,]   9.6   9.4   9.5  11.4   8.3   9.8   8.8   7.9   8.9  10.3  10.6
    ## [2,]   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0
    ##      [,58] [,59] [,60] [,61] [,62] [,63] [,64] [,65] [,66] [,67] [,68]
    ## [1,]   8.5     9   8.5   9.8   7.4     9   8.5   9.8   9.6   9.4   8.6
    ## [2,]   0.0     0   0.0   9.0   0.0     0   0.0   0.0   0.0   0.0   0.0
    ##      [,69] [,70] [,71] [,72] [,73] [,74] [,75] [,76] [,77] [,78] [,79]
    ## [1,]   8.7   9.6   9.9   9.3   8.9   9.1   9.9   8.9   9.3   8.7     7
    ## [2,]   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0   0.0     0
    ##      [,80] [,81] [,82] [,83] [,84] [,85] [,86] [,87] [,88] [,89] [,90]
    ## [1,]   7.8   9.3   8.9     9   8.9   9.3   9.6   9.4  10.3  10.4   7.9
    ## [2,]   0.0   0.0   0.0     0   0.0   0.0   0.0   0.0   0.0  10.0   0.0
    ##      [,91] [,92] [,93] [,94] [,95] [,96] [,97] [,98] [,99] [,100] [,101]
    ## [1,]  10.8     9  10.3     9   9.7  11.5  10.5   8.2   8.4    9.6    9.5
    ## [2,]   0.0     0   0.0     0   0.0   0.0   0.0   0.0   0.0    0.0    0.0
    ##      [,102] [,103] [,104] [,105] [,106] [,107] [,108] [,109] [,110] [,111]
    ## [1,]    9.3    8.8   10.7    9.1    9.7    8.5    8.7    8.1    9.9    9.4
    ## [2,]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0
    ##      [,112] [,113] [,114] [,115] [,116] [,117] [,118] [,119] [,120] [,121]
    ## [1,]    9.3    9.2    8.6    8.9   10.2    9.6    8.5    9.5      9    7.8
    ## [2,]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0      0    9.0
    ##      [,122] [,123] [,124] [,125] [,126] [,127] [,128] [,129] [,130] [,131]
    ## [1,]   10.1    8.1    8.8    8.7   11.1    8.6   10.3    8.1    9.3    9.5
    ## [2,]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0
    ##      [,132] [,133] [,134] [,135] [,136] [,137] [,138] [,139] [,140] [,141]
    ## [1,]    9.6    8.8    8.8   10.2    8.1    8.8    8.5    9.5    9.5    8.9
    ## [2,]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0
    ##      [,142] [,143] [,144] [,145] [,146] [,147] [,148] [,149]
    ## [1,]   10.4    7.7    8.2    7.1   10.4    7.4   10.1    9.8
    ## [2,]    0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0

``` r
predicted_ratings<-as.matrix(predicted_ratings)
colnames(predicted_ratings)<-colnames(read_books)
rownames(predicted_ratings)<-rownames(read_books)
```

The nect chunk of code it to use the matrix factorization to recommend
books to a chosen user.

``` r
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
   item_similarities_fac%>%mutate(title =colnames(data))%>% arrange(desc(item_similarities_fac$`user score`)) %>%head(10) %>%head(10)
```

    ##    user score                                                    title
    ## 1   0.9876023                                    house of sand and fog
    ## 2   0.9875452   a is for alibi (kinsey millhone mysteries (paperback))
    ## 3   0.9874854               full house (janet evanovich's full series)
    ## 4   0.9874606 confessions of a shopaholic (summer display opportunity)
    ## 5   0.9874435                                          the beach house
    ## 6   0.9873553                                    bridget jones's diary
    ## 7   0.9873545                                 white oleander : a novel
    ## 8   0.9873487                                              wild animus
    ## 9   0.9873422                                               the reader
    ## 10  0.9873378                    she's come undone (oprah's book club)

Code for function recommender which is saved in seperate .R file.
