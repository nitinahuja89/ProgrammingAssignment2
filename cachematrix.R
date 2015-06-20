## Functions to create a special object that stores a square matrix
## and cache's its inverse

## This function creates a special matrix thereby
## returning a list containing functions to :-
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y){
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


## This function calculates the inverse of the special matrix created by makeCacheMatrix function.
## It checks if the inverse has already been calculated, if yes then gets the cached inverse and
## skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # If the inverse is not null, return the cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If the inverse is null, compute the inverse and set the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  
}
