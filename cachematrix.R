##The first function, makeCacheMatrix creates a special "matrix", which a list of commands to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse
## (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    I <- NULL
      set <- function(y) {
        x <<- y
        I <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse) I <<- inverse
      getinverse <- function() I
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the mean
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the
## value of the mean in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    
    I <- x$getinverse()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
    
}
