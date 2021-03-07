## The function makeCacheMatrix caches the inverse of a matrix and the function
## cacheSolve computes the inverse of the matrix

## The below makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  minv <- NULL
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setInv <- function(solveM) minv <<- solveM
  getInv <- function() minv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}

## The below cacheSolve function computes the inverse of the special “matrix” 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  minv <- x$getInv()
  ## If the inverse has already been calculated(and the matrix has not changed),
  ## then the inverse of the matrix is retrieved from the cache. 
  if(!is.null(minv)){
    message("getting the inverse of the matrix from the cache")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setInv(minv)
  minv      
  
}
