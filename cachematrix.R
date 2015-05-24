## By combining the following functions it is possible to compute and cache
## the inverse of an inversible matrix.
##
## Example:
##
## m=rbind(c(1, -1/4), c(-1/4, 1))
## x<-makeCacheMatrix(m)
## cacheSolve(x)

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL    ## Initialize inv as NULL
   
   ## Initialize object
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   
   get <- function() x    ## Return original matrix
   
   setinv <- function(inverse) inv <<- inverse    ## Save inv
   
   getinv <- function() inv    ## Return inv
   
   ## Return a list of functions that will be used to interact with 
   ## this object
   list(set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
   inv <- x$getinv()    ## Get cached value if it exists, NULL if it does not
   
   ## Check if inverse is cached and return it if it is
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)    ## Return cached inverse
   }
   
   matrix <- x$get()    ## Get original matrix
   
   inv <- solve(matrix)    ## Compute inverse matrix
   
   x$setinv(inv)    ## Cache the computed inverse matrix
   
   inv    ## Return a matrix that is the inverse of 'x'
}
