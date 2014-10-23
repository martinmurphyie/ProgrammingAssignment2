## These functions cache(store) a matrix and its inverse

##  makeCacheMatrix
##  This function creates a list of functions to set a matrix, Get (output) the matrix
##  set the inverse of the matrix and get(output) the inverse of the matrix
##  assumes that this uses an invertibile matrix

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

##  CacheSolve function
##  Checks if the inverse of the matrix has been stored already and returns it
##  If not then inverse of the matrix is claculated using solve() function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }else{
    message("calculating...")
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}