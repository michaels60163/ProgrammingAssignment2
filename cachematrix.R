## These functions calculate an inverse of a matrix, and if it has already
## been calculated by the first function, the second function retrieves it
## from the cache, instead of calculating it again.

## This function allows to to create a matrix x and then calculates the
## inverse of that matrix i using the function solve()
## I do not understand why the y is needed in the set function. Can someone
## please shed light on this? 

makeCacheMatrix <- function(x = matrix()) { #create matrix
  i <- NULL
  set <- function(y) {
    x <<- y            # Why is y required?
    i <<- NULL
  }
  get <- function() x  #gets the original matrix x
  setInverse <- function(solve) i <<- solve #computes the inverse matrix i
  getInverse <- function() i #returns the inverse matrix i 
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## In this second function, it firstly attempts to retrieve a maxtrix inverse
## that has already been calculated, from the cache (in which case it alse
## returns the message. If it doesn't exist, it will then calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) #returns i if cached (i is not null)
  }
  data <- x$get() 
  i <- solve(data, ...) #otherwise calculate and set m
  x$setInverse(i)
  i
}

## To test the functions:
mat <- makeCacheMatrix(matrix(1:4, 2, 2))
mat$getInverse()  ## will return NULL (not calculated yet)
cacheSolve(mat) ## solves for the inverse
cacheSolve(mat) ## doing this again will return the inverse from the cache
mat$getInverse() ## no longer returns NULL
