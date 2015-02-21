##The purpose of this functions is to calculate the inverse of a matrix in an efficient way.
##The process is as follows:
##1. One will calculate the inverse of a matrix with the function cacheSolve()
##2. Its inverse will be stored in the cache memory with function makeCacheMatrix() as below
##3. If one wants to calculate of a matrix againg, one will test with function cacheSolve() 
##   if the inverse of the matrix  was already calculated and if so it will retrieve its inverse
##   from the cache, if not, it will calculate its inverse

## Function designated to store the in cache the inverse of a 'matrix' object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

## Function designated to test if there is already the inverse of the desired matrix 
## stored in the memory or not, and thus retrieving or returning its inverse

cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  newmatrix <- x$solve()
  m <- solve(newmatrix, ...)
  x$setsolve(m)
  m
}