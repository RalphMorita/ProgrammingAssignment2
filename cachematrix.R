## The following two functions are used to demonstrate caching in R.
## Their purposes are to  create a special object that stores a matrix and cache's its inverse.
## for more information about inverting a matrix, I found the following link helpful:
## http://www.purplemath.com/modules/mtrxinvr.htm
## please note, as per the course assinment: "assume that the matrix supplied is always invertible."
## The two functions will allow you to define a matrix say:
##      [,1] [,2] [,3]
## [1,]    1    3    3
## [2,]    1    4    3
## [3,]    1    3    4
##
## and return its inverse
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## I have also included some test code for the example above


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# The function returns a list with 4 components: get, set, setinverse, and getinverse
# w is the original matrix 
# m is the inverted matrix (since w kinda looks like an inverted m)
makeCacheMatrix <- function(w = matrix()) {
  m <- NULL
  # The set function sets the matrix w and sets the inverted matrix m to NULL 
  # this uses the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.
  set <- function(y) {
    w <<- y
    m <<- NULL
  }
  # the get function returns the matrix x
  get <- function() w
  # the setinverse sets the inverted matrix m  with input x 
  # for this assignment w will be the output (m) of the solve function in cacheSolve()
  setinverse <- function(x) m <<- x
  # the getinverse function returns the inverted matrix m
  getinverse <- function() m
  # create a list output for matrix w
  # This will allow for the w$getinverse(), w$get, w$setinverse in the cacheSolve function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
#The following function calculates the inverted matrix of the special "matrix" (actually a list) created with the above function (makeCacheMatrix()). 
#However, it first checks to see if the inverted matrix has already been calculated. 
#If so, it gets the inverted matrix from the cache and skips the computation. 
#Otherwise, it calculates the inverted matrix using the solve() function of the data and sets the value of the inverted matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  # get the current value of the inverted matrix
  m <- x$getinverse()
  #check to see if m is NULL. if it's not, the return the cached matrix. 
  #if is is NULL then the function must continue to calculate the inverted matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # set data to the value of the matrix
  data <- x$get()
  # use the solve function to determine the inverse of the matrix
  m <- solve(data, ...)
  # set the inverted matrix 'm' in cache
  x$setinverse(m)
  # return the inverted matrix 'm'
  m
}


### for testing uncomment the following...
#x <- c(1,1,1,3,4,3,3,3,4)
#xmatrix <- matrix(x, nrow = 3, ncol = 3)
#x1 <- makeCacheMatrix(xmatrix)
#x2 <- cacheSolve(x1)
#x2 # should display the inverted matrix
#x2 <- cacheSolve(x1) # should report "getting cached data"
