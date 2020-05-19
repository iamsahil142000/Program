# Program
makeCacheMatrix <- function(x = matrix()) {    ## define the argument with default mode of "matrix"
  ## This function creates a special "matrix" object that can cache its inverse
  
  i <- NULL                                    ## initialize inv as NULL; will hold value of matrix inverse
  set <- function(y) {                         ## define the set function to assign new
    x <<- y                                    ## value of matrix in parent environment
    i <<- NULL                                 ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                         ## define the get fucntion - returns value of the matrix argument
  setinverse <- function(inverse) i <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() i                  ## gets the value of inv where called
  list(set = set,                            ## you need this in order to refe
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ## to the functions with the $ operator
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
