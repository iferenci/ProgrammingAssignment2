## Put comments here that give an overall description of what your
## functions do

## This function returns a list of functions for caching and inverting the input Matrix

makeCacheMatrix <- function(x = matrix()) {

  
  m <<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  setInv <- function(MInv) m <<- MInv
  get <- function() x
  getInv <- function() m
  list (set = set, setInv = setInv, get = get, getInv = getInv)
  
}


## Returns the inverted Matrix, a check is done to see whether it is already cached before inverting
## The cached matrix will be returned if present

cacheSolve <- function(x, ...) {
        ## 
  
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  datarow = nrow(data)
  datacol = ncol(data)
  
  if (datarow == datacol){
    m <- solve(data)
  } else { 
    message("MASS library required")
    m <- ginv(data)
  }
  
  x$setInv(m)
  m
}
