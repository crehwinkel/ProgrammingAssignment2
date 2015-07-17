
## Two functions - First function creates an object to cache the inverse of the matrix.
##                 The second function gets the cached inverse of the matrix, or
##                   computes it and then caches the inverse of the matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  invmat <- NULL  #set the inverse matrix to NULL
  
  set<- function (y){
    x <<- y
    invmat <<- NULL
  }
  get <- function() x    # return the matrix
  
  setInverse <- function(inv) invmat <- inv
  
  getInverse <- function() invmat   #return the inverse of the matrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  imat <- x$getInverse()    #imat is the inverse of the matrix
  if(!is.null(imat)){
       message("getting cached data")
       return(imat)
  }
  ## if imat was null, then call get the matrix, call solve to calc the inverse
  ##  and save the inverse of the matrix in the cache
  mat <- x$get()
  imat <- solve(mat, ...)
  x$setInverse(imat)
  imat
}
