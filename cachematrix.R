
## creates a list which can save the inverse of a matrix passed
## to it

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ##sets matrix to invert
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      ##sets the inverse matrix
      setinverse <- function(solve) m <<- solve
      ##retrives inverse matrix
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## CasheSolve function will return cashed inverse matrix if the inverse
## is already available from the makeCasheMatrix function

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      ##returns cashed data if available
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      ## solve function obtains the inverse
      m <- solve(data, ...)
      ## invserse of matrix stored
      x$setinverse(m)
      m
}
