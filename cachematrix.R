
## This function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Set initial inverse to null
  inv <- NULL
  
  ## Set matrix function
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get matrix function
  get <- function() {
    x
  }
  
  ## Set inverse matrix function
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get inverse matrix function
  getinverse <- function () {
    inv
  }
  
  ## List of function
  list (
    set = set,
    get = get ,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## This function computes the inverse 
## of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse 
## has already been calculated (and the 
## matrix has not changed), then the 
##cachesolve should retrieve the inverse
##from the cache.
cacheSolve <- function (x, ...) {
  inv <- x$getinverse()
  
  ## If inverse is cached
  ## return inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  data <- x$get()
  
  ## Use the solve() function 
  ## to Calculate the inverse
  inv <- solve(data, ...)
  
  ## Cache calculated inverse
  x$setinverse(inv)
  
  ## Return calculated inverse
  inv
}