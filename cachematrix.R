##MakeCacheMatrix creates a list from an invertable matrix that includes the matrix and functions needed to 
##put the inverse in cache if it is not there already and get it from the cache if it is. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL # if m is there from another matrix, remove it!
  }
  get <- function() x
  doinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       doinverse = doinverse,
       getinverse = getinverse)
}


## Inverts the matrix passed to the function. Must be an invertable matrix object created with makeCacheMatrix
## if it has already been inverted it will not repeat the calculation but will pull it from the cache (variable m)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$doinverse(m)  # sets m to solve(matrix input)
  m
}

