## Create a matrix object and calculate its inverse. 
## Retrieve inverse if previously calculated
##
## Test run
##> testMatrix = rbind(c(4, 5/4, 1/2), c(3/4, 2, 3), c(4, 1/5, 3))
##> matrix = makeCacheMatrix(testMatrix)
##> matrix$get()
##[,1] [,2] [,3]
##[1,] 4.00 1.25  0.5
##[2,] 0.75 2.00  3.0
##[3,] 4.00 0.20  3.0
##
## First Run
##> cacheSolve(matrix)
##[,1]       [,2]        [,3]
##[1,]  0.1808288 -0.1222269  0.09208874
##[2,]  0.3264964  0.3348681 -0.38928422
##[3,] -0.2628715  0.1406446  0.23650063
##
## Second Run - call value in cache
##> cacheSolve(matrix)
##Getting cached matrix inverse
##[,1]       [,2]        [,3]
##[1,]  0.1808288 -0.1222269  0.09208874
##[2,]  0.3264964  0.3348681 -0.38928422
##[3,] -0.2628715  0.1406446  0.23650063

## makeCacheMatrix function: 
## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(myMatrix = matrix()) {
  ## Setting up the constructor
  myInverse <- NULL #Initializing myInverse to NULL
  setMatrix <- function(constMatrix) { ## set method
    myMatrix <<- y ## assign constrMatrix to myMatrix in above env
    myInverse <<- NULL ## reset inverse previously calculated if any
  }
  getMatrix <- function() myMatrix ## get method
  setInverse <- function(inv) myInverse <<- inv ## set Inverse method
  getInverse <- function() myInverse ## get Inverse method
  list(set = setMatrix, get = getMatrix,
       setinv = setInverse,
       getinv = getInverse) ## List of methods
}

## cacheSolve function: 
## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(myMatrix, ...) {
  myInverse <- myMatrix$getinv() ## call get Inverse method
  if(!is.null(myInverse)) { ## determine if previously calculated
    message("Getting cached matrix inverse")
    return(myInverse)
  }
  data <- myMatrix$get() ## Save Matrix in local var
  myInverse <- solve(data) ## Inverse matrix
  myMatrix$setinv(myInverse) ## Cache matrix in different environment
  myInverse ## Display result
}
