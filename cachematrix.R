## These functions are written to create the inverse of a Matrix: 

## Function1: "makeCacheMatrix" returns a list of functions including 
# setting and getting a matrix as well as setting and 
# getting the inverse of that Matrix

makeCacheMatrix <- function(x = matrix()) {
  In <- NULL
  set <- function(y) {
    x <<- y
    In <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) In <<- inverse
  getInverse <- function() In
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function2: "cacheSolve" returns the inverse of the Matrix. 
# Assumption: the matrix is always invertible.

cacheSolve <- function(x, ...) {
  In <- x$getInverse()
  
# First, it checks if the inverse of Matrix exist or not. If so, 
  # it just shows the result without any more computation. 

  if(!is.null(In)) {
    message("getting cached data")
    return(In)
  }
# If not, it computes the inverse using "solve" function.

  data <- x$get()
  In <- solve(data, ...)
  x$setInverse(In)
  In
}


## Example: 

# > B
#       [,1] [,2] [,3]
# [1,]   22  -16    3
# [2,]   55    1    9
# [3,]   -4    0   31

# > Nina<- makeCacheMatrix(B)
# > cacheSolve(Nina)
# [,1]        [,2]         [,3]
# [1,]  0.0010858144 0.017373030 -0.005148862
# [2,] -0.0609807356 0.024308231 -0.001155867
# [3,]  0.0001401051 0.002241681  0.031593695

# > cacheSolve(Nina)
# getting cached data
# [,1]        [,2]         [,3]
# [1,]  0.0010858144 0.017373030 -0.005148862
# [2,] -0.0609807356 0.024308231 -0.001155867
# [3,]  0.0001401051 0.002241681  0.031593695