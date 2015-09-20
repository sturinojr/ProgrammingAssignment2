## The following functions creates a matrix object that 
## caches the inverse operation done on a matrix

##makeCacheMatrix
##Create a matrix that will act as the cache of the inverse
##It will contain the following operations
##getCache - Determines if the cache is set for an operation
##setCache - If an operation is not flagged as complete, this will set it.
##getInv - get the inverse of that operation
##setInv - set the inverse of that operation

makeCacheMatrix <- function(x = matrix()) {
 
  currInv = NULL 
  
  setCache = function(cacheInfo){
    x <<- cacheInfo
    currInv <<- NULL
  }
  
  get <- function() x
    
  getInv <- function() currInv
  
  setInv <- function(currInverse){
    currInv <<- currInverse
  }
  
  list(get=get, setCache=setCache, getInv=getInv, setInv=setInv)
}


## cacheSolve 
## Receives data from makeCacheMatrix and if it is not already
## in the cache it will calculate the inverse, set it in cache,
## and return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  currInv = x$getInv()
  
  if(!is.null(currInv)){
    ##the inverse is already cached. no need to calculate it
    return(currInv)
  }
  else{
    ##The inverse is not in cache
    ##calculate the inverse, and set it in cache
    data <- x$get()
    currInv <- solve(data, ...)
    x$setInv(currInv)
    return(currInv)
  }
}
