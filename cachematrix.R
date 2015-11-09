## The functions makeCacheMatrix and cacheSolve create a derivative of the
## standard matrix() datatype that allows the caching of the inverse of the
## matrix. This is beneficial for large matrices when the inverse is repeatedly
## called as the inversion only has to be done once.


## MakeCacheMatrix function initiates the enhanced matrix and adds the functionality to it
## by adding subfunctions to set and get both the actual matrix as well as the
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse
  inv <- NULL
  # Set to add the actual matrix values
  set <- function(y) {
    # This sets the matrix to the value given in y and the inverse to NULL. Operator <<- so x and inv are set on
    # a higher environment than just the sub-function and can be used by parent function.
    x <<- y
    inv <<- NULL
  }
  # Get to return the orignal matrix
  get <- function() x
  # Set the inverse
  setinv <- function(solve.output) inv <<- solve.output
  # Get the inverse
  getinv <- function() inv
  # Function returns a list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function checks whether the inverse in a makeCacheMatrix object
## exist. If it does, it will return the cached value. If no values exists, it
## will compute the inverse of the matrix and add it to the object.

cacheSolve <- function(x, ...) {
  # Gets the inv from the makeCacheMatrix object
  inv <- x$getinv()
  # If a value exists, return that value and break out of function.
  if(!is.null(inv)) {
    # Print message that cached data was used
    message("getting cached data")
    return(inv)
  }
  # This is the part when a cached inverse does not exist yet.
  # Get the data from the object
  message("computing matrix inverse")
  data <- x$get()
  # Compute the inverse using solve-function
  inv <- solve(data, ...)
  # Set the inverse in the onject
  x$setinv(inv)
  # Return a matrix that is the inverse of 'x'
  inv
}

## Example of usage
# Initialize new object
mat.obj <- makeCacheMatrix()
# Add 3x3 matrix data to object
mat <- matrix(c(8,9,3,5,4,7,6,3,4), nrow=3)
mat.obj$set(mat)

# Get Inv - should be NULL
print(mat.obj$getinv())
# Now run cacheSolve
cacheSolve(mat.obj)

# Get Inv - should be computed now
print(mat.obj$getinv())

# Run cacheSolve again to see if it pulls from the cache
cacheSolve(mat.obj)
print(mat.obj$getinv())

              
