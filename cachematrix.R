## The functions written below allow caching of a matrix and its inverse to bypass the need
## to repeatedly calculate the inverse every time it is needed. Special "Cache Matrices" can be created
## by using makeCacheMatrix function, which can store a matrix and its inverse into a variable; and 
## the inverse of the matrix within "Cache Matrices" can be calculated and cached using cacheSolve function.

## makeCacheMatrix is a function that allows caching of a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    ##check that the new matrix is identical to the old
    if(!identical(x, y)) {
      i <<- NULL
    }
    x <<- y
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that takes the matrix stored in any special variable created by
## makeCacheMatrix above and calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
