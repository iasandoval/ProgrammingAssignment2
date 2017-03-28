# The first function, makeCacheMatrix creates a special "matrix", a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value
  i <- NULL
  # set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get the data of matrix
  get <- function() x
  
  # set inverse of the matrix, caculate outside
  setinverse  <- function(t) i <<- t
  
  # get the inverse of the matrix
  getinverse  <- function() i
  
  # return the Cachematrix,a list of 4
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache via the  
## setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the mean of 'x'
  m <- x$getinverse()
  
  # gets the inverse via getinverse function from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # calculates the inverse and sets the value of the inverse
  # get the matrix data
  data <- x$get()
  # calculate the mean of the vector
  i <- solve(data, ...)
  # save it to the cache
  x$setinverse(i)
  # return the inverse
  i
}
