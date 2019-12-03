## Assignment 2 is to create a cachedMatrix for its inverse. The cache gets reset as soon the content will change.

## makeCacheMatrix creates a cached matrix for a given matrix that caches the inverse of its content

makeCacheMatrix <- function(x = matrix()) {
  inverted_mtrx <- NULL
  set <- function(mtrx) {
    x <<- mtrx
    inverted_mtrx <<- NULL
  }
  get <- function() x
  cacheInvertedMatrix <- function(inverse) inverted_mtrx <<- inverse
  getInvertedMatrix <- function() inverted_mtrx
  list(set = set, get = get, cacheInvertedMatrix = cacheInvertedMatrix, getInvertedMatrix = getInvertedMatrix)
}


## cacheSolve returns the inverse of a cacheMatrix using its cache if exist or fills it on first usage.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted_matrix <- x$getInvertedMatrix()
  if(!is.null(inverted_matrix)) {
    message("using cached data")
    return(inverted_matrix)
  }
  matrix <- x$get()
  inverted_matrix <- solve(matrix, ...)
  x$cacheInvertedMatrix(inverted_matrix)
  inverted_matrix
}
