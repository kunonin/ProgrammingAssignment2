## This library computes the inverse of a matrix. Unlike using the 
## inverse function directly, it creates a cache to avoid computing
## twice the inverse of the same matrix

## This function creates the cache for a given matrix

makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  set <- function(matr){
      x <<- matr
      cached <<-NULL
  }
  get <- function() x
  set_inverse <- function(inverse) cached <<- inverse
  get_inverse <- function() cached
  list(set=set,get=get,set_inverse=set_inverse, get_inverse=get_inverse)
  
}


## Computes the inverse of a matrix, which should be created using
## makeCacheMatrix. If the inverse has been previously computed, it 
## does not compute the inverse again, but uses the cached data.

cacheSolve <- function(x, ...) {
        inverse=x$get_inverse()
        if (!is.null(inverse)){
          message("getting cached data")
          return(inverse)
        }
        matr<-x$get()
        inverse<-solve(matr,...)
        x$set_inverse(inverse)
        inverse
}
