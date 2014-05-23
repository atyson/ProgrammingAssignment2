## These functions will compute the inverse of a matrix and cache it 
## and then call it from the cache, if it exists in cache, 
## or calculate it, if it does not exist in cache


## The first function creates a special matrix and contains function
## to set value of matrix and inverse and return value of matrix
## and inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- inverse
      getinv <- function () m
      list(set = set, get =get, 
           setinv = setinv, getinv = getinv)
}


## Function below calculates inverse of matrix created above, but first checks
## to see if it has already been calculated. If so, will retrieve
## inverse of matrix from cache and skip calc. Otherwise, will calc.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if (!is.null(m)) {
            message ("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}

