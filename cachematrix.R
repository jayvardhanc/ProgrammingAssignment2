## Functions makeCacheMatrix and cacheSolve are used to store a Matrix and cache its inverse  
## 

## makeCacheMatrix contains the list of functions which set and return the values of the matrix and its inverse respectively
## it takes a matrix as an argument
## Creating an object which is an instance of makeCacheMatrix eg. a<-makeCacheMatrix(your matrix here) allows to subset 
## the main function and use the functions stored in the list

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## cacheSolve takes an instance of the makeCacheMatrix to get the stored matrix and calculates its inverse
## The inverse is stored using the setinverse function inside makeCacheMatrix. As a result, if it finds that the inverse 
## is already calulated then it skips the computation and returns the stored value

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  library(MASS)
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- ginv(matrix)
  x$setinverse(inv)
  inv
}
