
## The makeCachMatrix function does the following: takes a (square) matrix as and
## input, adds it to the cache, gets the inverse of the matrix, and adds this to 
## the cache. The list() function allows this to be indexed when called by the 
## cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mtx) m <<- mtx
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function returns a matrix that is the inverse of x. The inverse 
## matrix is referred to as m. An error message is given if the matrix entered has 
## not been cached or if it contains NA values. This is the function that actually
## finds the inverse of the matrix using the solve() function. 

#Example input:
#makeCacheMatrix(matrix(c(1,2, 3,4), nrow=2, ncol=2))

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m) && !is.na(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



