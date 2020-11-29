# Matrix inversion is usually a time-consuming computation and it makes sense to 
# cache the value of the output so that when we need it again, it can be looked up 
# in the cache rather than recomputed.
# Below are a two functions that cache the inverse of a matrix.
# It is assumed that the matrix supplied is always invertible.

# The makeCacheMatrix() function creates a special “matrix” object that can cache its inverse.
# What it does :
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


# The second function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the inverse from the cache 
# and skips the computation. Otherwise, it computes the inverse and sets the value 
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv     
}
