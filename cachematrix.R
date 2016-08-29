# Function 1.makeCacheMatrix.
# This function creates a special "matrix" object that can cache its inverse.
# It follows the example given & do the four steps:
# 1.set original matrix
# 2.get original matrix
# 3.set the inverse
# 4.get the inverse

makeCacheMatrix <- function(x = matrix()) {

  n <- NULL
  
  setoriginal <- function(x) {
    y <<- x
    inv <<- NULL
  }
  
  getoriginal <- function() y
  setinverse <- function(inverse) n <<- inverse
  getinverse <- function() n
  list(setoriginal = setoriginal,
       getoriginal = getoriginal,
       setinverse = setinverse,
       getinverse = getinverse)	
}


# Function 2.cacheSolve.
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# It basically follows the format given in the example.

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  
  if(!is.null(n)) {
    message("getting cached matrix")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n   
}
