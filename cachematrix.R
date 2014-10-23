## makeCacheMatrix creates a list of 4 functions which
## stores a matrix (set)
## returns a stored matrix (get)
## takes and caches the inverse of a matrix (setMatrixInverse)
## returns the cached inverse of the original matrix (getMatrixInverse)
## pairs the functions with tags so they can be called externally (list)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Store a matrix passed into the function
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## Get the stored matrix
  get <- function() x
  ## Store the inverse 
  setMatrixInverse <- function(inverse) m <<- inverse
  ## Get the stored inverse to return
  getMatrixInverse <- function() m
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## This function uses the capabilities created using
## makeCacheMatrix to either get the inverse of the
## original matrix, store and return it, or retrieve
## and return it if it was already cached.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
  ## Check to see if it was available - if yes, return it
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Get the original matrix 
  data <- x$get()
  ## Calculate the inverse
  m <- solve(data, ...)
  ## Store it
  x$setMatrixInverse(m)
  ## Return the inverse
  m
}

