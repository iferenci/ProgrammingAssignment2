## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
