## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
##

makeCacheMatrix <- function(x = matrix()) {
  #deveria funcionar assim mesmo, mesma estrutura do exemplo com vetor
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverser) Inv <<- inverse
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
# `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
##


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  # Exemplo média de um vetor
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
  
}
