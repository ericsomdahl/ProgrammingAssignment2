##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the matrix
#get the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solv) m <<- solv
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##The following function calculates the inverse matrix of the special "vector" created with the above function. 
##However, it first checks to see if the matrix has already been calculated. 
##If so, it gets the matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverted matrix of the data and sets the value of the matrix into the cache 
##via the setmatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
