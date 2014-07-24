## The following functions allows a user to setup a variable (makeCacheMatrix) that 
## incorporates both functions and data. Other R objects may interact with this variable 
## and its data by calling the exposed functions.  The second function (cacheSolve) uses 
## this variable as a cache for an inverted matrix. If the inverted matrix has not 
## been populated it is calculated.  Once populated, it pulls from the stored version
## rather than perfomring the calculation again.

## The makeCacheMatrix function setup the variables and builds functions to allow the 
## user/other funcitons to 
##     store an input matrix (setMatrix),
##     retrieve the input matrix (getMatrix)
##     store an inverted matrix (setInverse)
##     retrieve the inverted matrix (getInverse)
makeCacheMatrix <- function(x = matrix()) {
      
      ## Clear the result inverted matrix
      m <- NULL
      
      ## Store the matrix passed by the user if it is a square matrix
      ## If the Matrix is not square, return an error
      setMatrix <- function(y) {
            nrows <- nrow(y)
            ncols <- ncol(y)
            
            if (nrows == ncols) {
                  x <<- y
                  m <<- NULL
            }
            else {
                  stop("Matrix is not Square")
            }
      }
      
      ## Allow the user to Retrive the matrix they passed into the function
      getMatrix <- function() {
            x
      }
      
      ## Assign the inverted matrix into the variable
      ## Note, this function is not called by the user; it is called by cacheSolve
      setInverse <- function(inverse) {
            m <<- inverse
      }
      
      ## Allow the user to retrieve the Inverted Matrix
      getInverse <- function() {
            m
      }
      
      ## The following returns the list of functions available to the user.
      list(setMatrix = setMatrix, 
           getMatrix = getMatrix,
           setInverse = setInverse,
           getInverse = getInverse)
      
}

## The cacheSolve function returns an inverted matrix based on a matrix input by the user.
## When the inverted matrix is first calculated, it is stored in "cache".  If the procedure
## is called and a "cache" exists, the calculation is not performed and the "cached"
## matrix is returned.

cacheSolve <- function(x, ...) {
      
      #If no input matrix exists, stop execution
      userMatrix <- x$getMatrix()
      if(is.null(userMatrix)) {
            stop("Input Matrix is empty")
      }
      
      
      #Retrieve the value of the Inverted Matrix
      #If the matrix is null then the inverted matrix has not been computed
      #If it is not null the inverted matrix has been computed and should be
      #returned without recomputing
      m <- x$getInverse()
      if(!is.null(m)) {
            message("Getting cached data")
            return(m)
      }
      
      
      #This code is executed when the inverted matrix has not been computed
      #Compute the inverted matrix with the solve function
      data <- x$getMatrix()
      m <- solve(data)
      x$setInverse(m)
      message("Creating Cache")
      m
}