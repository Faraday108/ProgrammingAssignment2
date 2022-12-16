## These two functions in conjunction create a special object
## that stores a numeric vector and caches its inverse. 

## Write a short comment describing this function
## This function creates a special matrix of four functions: 
## [[set,setinv][get,getinv]]
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  matrix(c(set,get,setinv,getinv),ncol = 2, nrow = 2)
}

## This function calculates the inverse of the special 'matrix'
## created with the above function. Checks if the mean has 
## already been calculated, if so it gets the inverse from cache. 

cacheSolve <- function(x, ...) {
  i <- x[[2,2]]() #getinv
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x[[2,1]]() #get
  i <- solve(data,...)
  x[[1,2]](i) #setinv
  i
}
