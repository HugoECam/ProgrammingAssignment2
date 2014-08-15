## The following functions are designed to compute the inverse of a matrix. 
## But before the calculattion it searches in a cache object to see 
## if it was previously calculated in order to avoid the calculation 

## Converts a matrix to a similar object "matrix"
## it is used to calculate the inverse matrix and to maintain the result 
##    x is a numeric matrix
##    returns a "matrix" object (a list)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function performs the calculation on the matrix inverse,
## It verify if existe before execute the calcluation of the inverse
##   x -- it is a "matrix" object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
