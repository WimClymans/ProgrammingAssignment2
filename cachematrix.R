## Two functions are created to calculate and cache inverse square matrices: 
##1) makeCachematrix: Creates a list with functions creating, and storing a matrix and its inverse.
##2) cacheSolve: Returns the inverse matrix but checks first if it has already been calculated before returning it.

## Creates a list containing the matrix (x) and its inverse (detailed stepwise comments given below)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                  ##sets an empty inverse matrix
  set <- function(y) {                       ##assigns created matrix, here y, to x & resets inverse matrix i
    x <<- y
    i <<- NULL
  }
  get <- function() x                        ##returns matrix x
  setinverse <- function(solve) i <<- solve  ##set and calculates the inverse using solve
  getinverse <- function() i                 ##returns the calculate inverse
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)   ##returns the "special matrix"
}

## Return the inverse, by either returning a cached value or recalculating the inverse matrix of a given input matrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()                        ##sets the i-matrix to the inverse matrix available in the special "matrix", i.e. NULL or previously calculate inverse
  if(!is.null(i)) {                          ##check if i is NOT an empty matrix
    message("getting cached inverse matrix") ##message indicating that the inverse matrix was previously stored
    return(i)                                ##breaks the function and returns the stored i value
  }
  data <- x$get()                            ##identifies the new matrix for which no inverse was available
  i <- solve(data,...)                       ##calculates a inverse matrix, and afterwards includes it in the special matrix, and returns it
  x$setinverse(i)
  i
}
