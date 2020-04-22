#makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(inver) inver <<- inver
  getinver <- function() inver
  list(set = set,
       get = get,
       setinver = setinver,
       getinver = getinver)
}

#cacheSolve
cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}
