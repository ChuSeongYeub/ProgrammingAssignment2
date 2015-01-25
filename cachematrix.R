## My first function will create a special "matrix" object that can cache its inverse. 
## First, it will set the value of the matrix, and then it will get the value of the matrix.
## Third, it will set the value of the inverse of the matrix, and finally it will get the value of the inverse of the matrix.
## So the first function can be called as the first step or first stairs to get into the cache.
## The next function will calculate the inverse of the matrix posterior to the cache.
## If the inverse of the matrix has been already caculated and piled up in the cache, it doesn't have to calculate again.
## Though if the content has not been piled up in the cache, then it should calculate it newly.

##I just rewrote some part that I have written above.
## First, it will set the value of the matrix, and then it will get the value of the matrix.
## Third, it will set the value of the inverse of the matrix, and finally it will get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }    ##setting the value of the matrix
  get <- function()x    ##getting the value of the matrix
  setsolve <- function(solve) m <<- solve    ##setting the value of the inverse of the matrix
  getsolve <- function()m    ##getting the value of the inverse of the matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This is also rewritten from the contents above.
## The next function will calculate the inverse of the matrix posterior to the cache.
## If the inverse of the matrix has been already caculated and piled up in the cache, it doesn't have to calculate again.
## Though if the content has not been piled up in the cache, then it should calculate it newly.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.nul(m)) {
    message("getting cached data")    ##seeing whether the inverse of the matrix has already been calculated.
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m    ##If the inverse of the matrix has already been calculated within cache, it gets the inverse of the matrix from the cache and skips the computation
}

