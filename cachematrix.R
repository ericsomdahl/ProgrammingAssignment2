##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##set the value of the original matrix to use in the computation
##get the value of the original matrix to use in the computation
##set the value of the computed and cached matrix
#get the value of the computed and cached matrix
makeCacheMatrix <- function(x = matrix()) {
  ##the cached/calculated matrix
  m <- NULL
  
  ##set the original matrix and clear the cached matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##return the original matrix
  get <- function() x
  
  ##store the value in the cache
  setmatrix <- function(solv) m <<- solv
  
  ##return the cached value
  getmatrix <- function() m
  
  ##return the function list
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
  ## access the cached value
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    ##a value has been cached
    message("getting cached data")
    return(m)
  }
  
  ##no cache, grab the original value
  data <- x$get()
  
  ##invert the original value
  m <- solve(data, ...)
  
  ##store that value in cache
  x$setmatrix(m)
  
  #return the calculated value
  m
}
