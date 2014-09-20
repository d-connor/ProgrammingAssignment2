## I approached this at fist waaaay over thinking it.  The light bulb turned turn on after ## walking through the example step by step and realizing how close it needed to be to the ## actual function.
## 

## Beginning with the first part of the assignment:

makeCacheMatrix <- function(x = matrix()) {  ## The function below assess a matrix, needs to be a square matrix (1:4, 2).

  
  i <- NULL  ## You want to set i and x to null, so the parent environment doesn’t overweight this with a value you don’t want 
  set <- function(y) {  ## Sets the value of the matrix
    x <<- y
    i <<- NULL
  }
  get <- function() x  ## Calls upon the value of the matrix
  setinv <- function(solve) i <<- solve  ## Takes value of matrix, inverts it, sets it in the environment
  getinv <- function() i  ## gets the inversed matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)  ## Calls upon the value of the inverse matrix
  
}


## This was much easier after completing the first one. This double checks if the environment has cached the matrix/inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinv()  ##get the inverse previs matrix
  if(!is.null(i)) {  ## this double checks if the matrix is in the environment, specifically the 'opposite' of i being Null (aka present)
    message("Getting Cached Data, Lazy!!")  ## Communicates to the user that the data is being retrieved
    return(i)  ## Displays i (matrix)
  }
  data <- x$get() ## If the matrix isn’t present, it gets it, and prepares it to be inverted
  i<- solve(data, ...) #inverts matrix
  x$setinv(i)  ## sets the inverse of the matrix
  i
  
}
