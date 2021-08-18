## calculate inverse of a matrix and cache; recall if calculated previously
## initiate by running cached <- makeCachedMatrix(someInvertibleMatrix)

## define get/set functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## calculate/recall inverse

cacheSolve <- function(x, ...) {
  ## check if the entered matrix is the same as the cached & that inverse !=NULL
  if (identical(x,cached$get()) && !(is.null(cached$getinverse()))){
    i <- cached$getinverse()
    message("getting cached data")
    return(i)
  }
  else {
    ##replace matrix in cached
    cached$set(x)
    ##calculate inverse from cached matrix
    data <- cached$get()
    i <- solve(data, ...)
    cached$setinverse(i)
    i
  } 
}
