## Here we make two functions: makeCacheMatrix and cacheSolve. The first one
## creates a CacheMatrix from a common matrix. The second one, can operate on a
## CacheMatrix. If the inverse of the matrix in CacheMatrix has already been 
## calculated, it fetches this value. Else, it calculates the inverse and caches
## this value in the CacheMatrix with the provided function, 'setInverse'. If the
## matrix is not invertible, it ouputs "Error in solve.default(data) ..."

# This function let us create a CacheMatrix provided a common matrix
makeCacheMatrix <- function(x = matrix()) {
  # Optional: you can change (set) the value of your CacheMatrix
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # To get the value of the matrix in your CacheMatrix
  get <- function() x
  
  # If the inverse hasn't yet been calculated, we provide to the function CacheSolve
  # a method for caching the inverse of the matrix in our CacheMatrix.
  # Note: here, inv is a free variable
  setInverse <- function(inv) inverse <<- inv
  
  # We also provide a function to get the inverse (if it was already cache'd)
  getInverse <- function() inverse
  
  # Finally, our return value:
  list(set = set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function solves the inverse of a CacheMatrix
cacheSolve <- function(x, ...) {
  
  ## Check if inverse(x) was calculated. 
  inverse <- x$getInverse()
  
  ## If it was calculated, return inverse(x)
  if(!is.null(inverse)) {
    message("getting cached data...")
    return(inverse)
  }
  
  ## If it wasn't calculated, calculate and cache it
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}

