## makeCacheMatrix creates a list containing a function to:
## set value of the matrix 
## get value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
         invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse) {
    invMatrix <<- inverse
  } 
  getinverse <- function() {
    invMatrix
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of makeCacheMatrix
## it first checks to see if the inverse has already been calculated 
## if so, it gets the inverse from the cache and skips the computation
## otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache, via the setSolve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getinverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  } 
  data <- x$get()
  invMatr <- solve(data, ...)
  x$setinverse(invMatrix)
  invMatrix
}
