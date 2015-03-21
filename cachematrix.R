setwd("~/Desktop/Coursera/R_Programming") #setting work directory to course folder

#This function creates a special matrix and caches its inverse using the function solve
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

#This function computes the inverse of the special matrix described in the anterior function (makeCacheMatrix).
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data") # a returning message saying the data was retrieved from the cache
      return(m)
    }
    matrix<-x$get ()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
invmatrix <- makeCacheMatrix( matrix(c(5,10,10,5), nrow = 2, ncol = 2) ) #it creates a 2x2 matrix 
class(invmatrix) # it gives the class of the object invmatrix, which is a list!
summary(invmatrix);
invmatrix$get(); # gives the values for each row/column of the matrix
cacheSolve(invmatrix) # calculate the inverse of the matrix
cacheSolve(invmatrix) # When the function is requested for the second time (if the matrix is still the original one), we get the cached value with the message "getting cached data"