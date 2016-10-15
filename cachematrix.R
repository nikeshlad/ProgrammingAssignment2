## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  # Creates a list a list containing a function to
  # 1.set the value of the vector
  # 2.get the value of the vector
  # 3.set the value of the mean
  # 4. the value of the mean        
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...){
  i <- x$getinverse()
  
  # If the inverse of the matrix has already been calculated
  if(!is.null(i)){
    
    # Get it from the cache and skip the computation of the inverse
    message("getting cached data")
    return(i)
  }
  
  # Else compute the inverse of the matrix
  data <- x$get()
  i <- solve(data, ...)
  
  # Setting value of the inverse in the cache through the setinv function
  x$setinverse(i)
  i
}
