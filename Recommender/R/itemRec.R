#' Recommender for books
#'The package recommends a list of books according to user, or item.
#' @param data database of book and user ratings
#' @param criteria user wanting the recommended books
#' @param typetype of recommeder system
#'
#' @return the top 10 books like to be liked by user.
#' @export
#'
#' @examples
#' recommend(book_ratings, 13, type = 'item')
#' recommend(book_ratings, 23, type = 'user')
recommend<- function(data,criteria,type = '', raw_data = NULL){

  cosine_sim <- function(a, b){crossprod(a, b) / sqrt(crossprod(a) * crossprod(b))} #function to calculate similarity

#if the type of similarity is item
   if(type == 'item')
     {

     item_similarities = matrix(0, nrow=ncol(data),ncol=ncol(data)) #create blank place holder matrix
     item_matrice<-matrix(rep(data[,criteria],times=ncol(data) ),ncol = ncol(data),nrow = nrow(data),byrow = TRUE) #fil matrix with one item ratings
     item_similarities <- cosine_sim(item_matrice,data) #calculate the similarities between user matrix and the rest of the database ratings
     item_similarities<-as.matrix(diag(item_similarities)) #the diagonal values of matrix is the similarity bewteen chosen item and rest of the items

     colnames(item_similarities) <- 'user score' #column name of the similarity vecetor
     rownames(item_similarities) <- colnames(data) #row names of the similarity vector

     user_seen <- row.names(item_similarities)[data[criteria,] == TRUE] #get the movie the chosen user has seen

     item_seen<-unlist(item_similarities[user_seen,])
     item_similarities<-as.data.frame(item_similarities)
    #create a data frame with user scores and if the user has seen the movie
     item_score<- data.frame(title = colnames(data),
                             score = item_similarities$`user score`,
                             seen = ifelse(item_seen==item_similarities[,1],1,0))
    #recommend top 10 similar movies the user has not seen
    recommend<- item_score %>% filter(seen == 0) %>%arrange(desc(score)) %>%select(-seen)%>% head(10)




   }
  #if the chosen type is user
  if(type == 'user'){
    user_similarities = matrix(0, nrow=nrow(data),ncol=nrow(data)) #create blank holder matrix

    user_matrice<-matrix(rep(data[criteria,],times=nrow(data) ),ncol = nrow(data),nrow = ncol(data),byrow = TRUE) #fill matrix with one user rating
    user_similarities <- cosine_sim(user_matrice,t(data)) #calculate the similarities between the user and the rest of the user ratings
    user_similarities<-as.matrix(diag(user_similarities)) #diagonal values are the calcualted similarities between users

    colnames(user_similarities) <- 'user score'
    rownames(user_similarities) <- rownames(data)

    user_seenit <- row.names(user_similarities)[data[,criteria] == TRUE] #get the movie the chosen user has seen

    user_seent<-unlist(user_similarities[user_seenit,])
    user_similarities<-as.data.frame(user_similarities)

    user_score<- data.frame(title = sorted_my_users,
                            score =(user_similarities$`user score`))

#eliminate user has seen- not entirely correct but will work
    user_scored<-  user_score %>%
      filter(score != 1) %>%
      arrange(desc(score))

    last<- as.data.frame(user_scored%>%head(10)%>%select(title))

    colnames(last)<-'User.ID'
    last<-as_data_frame(last)
    lasted<-merge(raw_data,last,by = 'User.ID')#create table of the movies and similar users
    lasted<- apply(lasted, 1, function(i) paste(names(i[i != 0 ]))) #keep movies only which was rated
    lasted<-as.matrix(lasted) #create as matrix
    lasted<- as.matrix(lasted[-1,]) #remove user.id column
#recommend the movies as user similarities
    recommend<-lasted[!duplicated(lasted[])]%>%head(10)

  }
  #if type is k-nearest
  if(type == 'item2'){


  cosineSim <- function(matrix1,matrix2) {

    apply(matrix1,2,function(x){

      apply(matrix2,2,cosine_sim,x)
    })
  }


  sim_num<-crossprod(read_books)
  diagnl<-sqrt(diag(sim_num))
  sim_dem<-diagnl%*%t(diagnl)
  similarity<-sim_num/sim_dem
  similarity<-as.data.frame(similarity)

  similarity[1:6, 1:4]

  diag(similarity)<-0
  sim_neighbours = matrix(NA, nrow=ncol(similarity),ncol=11,dimnames=list(colnames(similarity)))

  for(i in 1:ncol(read_books)) { # loop through songs
    sim_neighbours[i,] <- (t(head(n=11,rownames(similarity[order(similarity[,i],decreasing=TRUE),][i]))))
  }


  recommend<-sim_neighbours[149, 1:11]
}
    return(recommend)

  if(type == ''){
    print('No type selected')
  }



}

#userRec <- function(data,)
