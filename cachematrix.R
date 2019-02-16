## This code will create two functions, "makeCacheMatrix" & "cacheSolve".
## The first function will create a matrx, invert it and place in cache. 
## The second function will invert a matrix, but checks first to see if it has already been done 
## and the matrix is the same that needs to be inverted.

## Create a matrix, invert it, cache it:

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
## Set the matrix to cache, not inverse matrix.
    set <- function(y) {
      x <<- y
      invert <<- NULL
    }
## Get the matrix.
    get <- function() x
## Set matrix to inverse.
    setinvert <- function(solve) invert <<- solve
## Get the inverse of the matrix.    
    getinvert <- function() invert
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## Invert matrix, but check first to see if has already been done.
cacheSolve <- function(x, ...) {
## Check to see if inverse of matrix is in cache.
    m <- x$getinvert()
    if(!is.null(m)) {
    message("getting cached data")
    return(m)
    }
## If it has note been done, calculate it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}

## Test it out to see what it looks like.
x <- matrix(1:4, nrow=2, ncol=2)
t <- makeCacheMatrix(x)
y <- cacheSolve(t)
print(y)
print (x)
print(x)
