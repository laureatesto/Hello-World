#makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.

#steps
#1 - set the matrix
#2 - get the matrix
#3 - set inverse of the matrix
#4 - get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {                        #1
    x <<- y
    inver <<- NULL
  }
  get <- function() x                         #2
  setinver <- function(inver) inver <<- inver #3
  getinver <- function() inver                #4
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}

#cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}

