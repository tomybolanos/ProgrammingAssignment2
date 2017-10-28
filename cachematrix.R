## tbolanos- 20171028

## This function creates a special type of matrix thats caches the inverse (solve function)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setsolve <- function(solve)
    m <<- solve
  getsolve <- function()
    m
  list(
    set = set,
    get = get,
    setsolve  = setsolve,
    getsolve = getsolve
  )
}



## once created the special matrix object, this function returns the inverse . it calculates the first time of uses the cache the following times.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



## tests


##> m <- makeCacheMatrix(x)
##> cacheSolve(m)
##[,1] [,2]
##[1,]    1   -2
##[2,]   -1    3
##> cacheSolve(m)
##getting cached result
##[,1] [,2]
##[1,]    1   -2
##[2,]   -1    3

