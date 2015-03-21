## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

 mInv <- NULL
  setmatrix <- function(y)
  {
    x <<- y
    mInv <<- NULL
  }
  getmatrix <- function() x
  
  setmatrixInverse <- function(matInverse) mInv <<- matInverse
  
  getmatrixInverse <- function() mInv
  
  list(set = setmatrix, get = getmatrix, setInverse = setmatrixInverse, getInverse = getmatrixInverse)
 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  mInv <- x$getInverse()
  if(!is.null(mInv))
  {
    message("Getting Inverse from the Cache")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data)
  x$setInverse(mInv)
  mInv
}
