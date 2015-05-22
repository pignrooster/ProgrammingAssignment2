## calculating the matrix inverse can be a resource intensive operation.  if   
## we cached the matrix inverse, then cached matrix inverse can be get and the 
## costly matrix inverse calculation can be avoided.

## makeCacheMatrix stores a matrix 'x' and a list of four functions to set 
## matrix 'x', get matrix 'x', set the matrix inverse of 'x', and get the 
## matrix inverse of 'x'.  The set function will only make changes to the 
## matrix 'x' and matrix inverse 'mi' if the new matrix does not equal 'x'.

makeCacheMatrix <- function(x = matrix()) {
      mi <- NULL
      ## evaluate equality of previously stored matrix 'x' against the new 
      ## matrix 'y' and replace only if 'x' does not equal 'y'
      set <- function(y) {
            #if the new matrix y is equal to the old matrix x, keep mi and x
            if(all(x == y) == FALSE) {
                  ## if the new matrix 'y' is not equal to the old matrix 'x',
                  ## replace 'x' with 'y' and set matrix inverse 'mi' to NULL. 
                  x <<- y
                  mi <<- NULL
            }
      }
      ## get the matrix 'x' fed into makeCacheMatrix(x)
      get <- function() x
      ## set the inverser matrix 'mi' equal to the matrix inverse 'inv'
      setinv <- function(inv) mi <<- inv
      ## get the inverse matrix 'mi'
      getinv <- function() mi
      ## return a list of four functions defined above
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes a list 'x' of four functions: set, get, 
## setinv, getinv which are defined in MakeCacheMatrix.  If x$getinv is NULL 
## then there is no matrix inverse for the matrix returned by x$get, so
## cacheSolve will recalculate and store the matrix inverse with x$setinv. If
## x$getinv is NOT NULL then cacheSolve will return the cached matrix inverse
## stored in 'mi'

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      mi <- x$getinv()
      if(!is.null(mi)) {
            ## if the matrix inverse 'mi' is NOT NULL, use the cached 'mi'
            message("getting cached data")
            ## exit the cacheSolve function
            return(mi)
      }
      ## if 'mi' is NULL then get the matrix from the enclosure 'x' 
      data <- x$get()
      ## calculate the inverse of the matrix 'data'
      mi <- solve(data, ...)
      ## store the matrix inverse value 'mi' in the enclosure 'x'
      x$setinv(mi)
      ## return the newly calculated matrix inverse 'mi'
      mi
}

## EXAMPLE CODE
## 
## create a matrix 'm1'
## m1 <- matrix(rnorm(16,8,2),nrow=4,ncol=4)
##
## create 'matrixObject' containing a special matrix vector containing the four 
## makeCacheMatrix functions and the matrix 'm1'
## matObject <- makeCahceMatrix(m1)
## 
## cache the matrix inverse of 'm1' within 'matObject' using cacheSolve
## cacheSolve(matObject)
## 
## if the 'm1' is replaced with an equal matrix 'm1' using matObject$set(m1)
## then the cached matrix inverse is retained and used by  
## 
##
## if a new matrix 'm2' is stored within 'matObject' using matObject$set(m2)
## then cached matrix inverse is set to NULL and the matrix inverse will be 
## recalculated when cacheSolve(matObject) is called
