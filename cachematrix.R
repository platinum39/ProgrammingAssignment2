## This function creates a special 'matrix' object that can cache its inverse.

##The function contains the following steps:
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the value of the inverse of the matrix.
## 4. get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special 'matrix' returned by the 
## function makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should get the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
    if(!is.null(m)) {
      message('extracting cache data')
      return(m)
    }
  matrix <- x$get()
  m <-solve(matrix)
  x$setinverse(m)
  m
}
