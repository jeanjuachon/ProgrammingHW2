makeCacheMatrix <- function(x = matrix()) {
  #inverse of matrix that would be cached
  inv <- NULL
  
  #get and set of matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
 #get and set of matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  #list
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  #computed matrix inverse
  if (!is.null(inv)) {
    message("Cached inverse")
    return(inv)
  }
  #compute
  m <- x$get()
  inv <- solve(m)
  

  x$setinv(inv)
  

  return(inv)
}
#done