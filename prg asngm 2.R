makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}





cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("gets cache data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}
