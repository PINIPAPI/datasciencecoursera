## This is the secong HW and is about Lexical scope
## Calculates the inverse of a matrix and cache the result

## The first function creates a matrix and a serie of functions to 
## calculate the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
       x <<- y
       inv <<- NULL
  }
       get <- function() x
       setinv <- function(solve) inv <<- solve(x)
       getinv <- function() inv
       list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes the result cached from the first funtion and if the 
## result is not cached, it calculates the inverse of a matrix a cache the 
## result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(x)
  x$setinv(inv)
  inv
}
