# The first function makeCacheMatrix creates a matrix that can cache its inverse
#  this caching ability can save signicant processing time for large matrix
# The second function cacheSolve checks if the inverse version of the matrix
#  has already been cached and retrieves it if it has been cached previously
#  and creates the inverse if it has not been cached.

# makeCacheMatrix takes a matrix as a parameter caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve 
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse ,
       getInverse = getInverse )

}


# cacheSolve takes a list of functions as an argument
#  if the inverse of the matrix has already been calculated it is returned
#   if it has not been cached the solve function is used to find the matrix's 
#   inverse and return it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse ()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
