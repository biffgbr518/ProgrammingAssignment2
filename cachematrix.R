## Caching the inverse of a matrix saves time and computer power therefore a function is used to calculate the inverse of a matrix. 

## makeCacheMatrix is a function that outputs an object that includes the inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve is a function that has an input of the special matrix shown by makeCacheMatrix. It has an output of the inverse of the matrix in question (if the matrix has not been changed). 


cacheSolve <- function(x, ...) {
       m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}