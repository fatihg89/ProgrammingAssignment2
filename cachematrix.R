##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())   {
  ma <- NULL
  set <- function (y) {
    x <<- y
    ma <<- NULL
  }
  get <- function() x   #inverse matrix
  inverseset <- function(solve) ma <<- solve  #solve(x) --> inverse matrix
  inverseget <- function() ma
  list(set = set, get = get,
       inverseset = inverseset,
       inverseget = inverseget)
}

##This function computes the inverse of the special "matrix" returned by
##makeCacheMatrix above.

## Firts, check if ma has already been calculated, if true, return(ma)
cacheSolve <- function(x, ...) {
  ma <- x$inverseget()
  if(!is.null(ma))  {
    message("getting cached data")  #Message if ma has already been calculated
    return(ma)
  }
  ## if not calculated, solve the matrix and store the data.
  matrix <- x$get()  # uses get vector from makeCacheMatriz function to 
  ma <- solve(matrix, ...)
  x$inverseset(ma)
  ma
}
