## I approached this at fist waaaay over thinking it.  The light bulb turned turn on after ## walking through the example step by step and realizing how close it needed to be to the ## actual function.
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  ## The function below assess a matrix

  
  i <- NULL  ## You want to set i to null, so the parent environment doesn’t overweight this with a value you don’t want
  set <- function(y) {  ## Sets the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x  ## Calls upon the value of the matrix
  setinv <- function(solve) i <<- solve  ## Takes value of matrix, inverts it
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)  ## Calls upon the value of the inverses matrix
  
}


## This was much easier after completing the first one:

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinv()  ##get the inverse previs matrix
  if(!is.null(i)) {  ## this double checks if the matrix is in the environment 
    message("Getting Cached Data, Lazy!!")
    return(i)
  }
  data <- x$get() ## If the matrix isn’t present, it gets the inverse of it for you
  i<- solve(data, ...)
  x$setinv(i)  ## sets the inverse of the matrix
  i
  
}
