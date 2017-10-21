## Functions create a "matrix" object which can cache both the matrix
## and its inverse in memory for later access

## Creates "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<-inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Retrieves "matrix" inverse. If inverse has not yet been calculated, calculates 
## and caches result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}