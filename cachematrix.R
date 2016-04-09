makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
  setMatrix <- function(y) {  ## Function defined to store Matrix. Function's argument is the matrix to be stored.
          x <<- y
         m <<- NULL
    }
  getMatrix <- function() x  ## Function that recalls the stored matrix by setMatrix
  setInverse <- function(solve) m <<- solve ## Function defined to store the inverse of the matrix. Function's argument is the inverse to be stored.
  getInverse <- function() m ## Function that recalls the stored inverse by setInverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse) ##Saves all stored parameters in a list.
}

cacheSolve <- function(x, ...) {
     m <- x$getInverse()  ## function recalls inverse from cache using the makecachematrix function
  if(!is.null(m)){    ## function checks if any inverse is actually stored in cache. 
    
    message("getting cached data") 
    return(m)  ## If inverse is located in cache, then it recalls the inverse and prints it. End of function.
      }
  data <- x$getMatrix()  ## If inverse wasn't stored then it will atempt to calcute it using the solve() function.
  m <- solve(data, ...)
  x$setInverse(m) ##Function stores calculated inverse in cache.
  return(m)  ## Function returns inverse.
}
