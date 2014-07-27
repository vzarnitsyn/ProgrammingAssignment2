## Functions in this module allows to set the matrix the way which
## allows an access to cashed inverse matrix
## It is an exercise  so matrixes are not checked for being inversible
## rather the sove function is meing applied to whatever matrix is used

## make Cahse Matrix is a list of 4 functions set, get, setinverse and getinverse
## as an exercise for cashed results


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cashe solve return inverse matrix for its argument matrix 
## It tries to save machine time by returning cashed invers

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}

