## Put comments here that give an overall description of what your
## functions do

# The functions store an inversed matrix in the enrivonment once it 
# has been inversed. This means the first time the matrix will be 
# solved but after the first time the result of the inversion will 
# be stored and accessible when needed.

## Write a short comment describing this function

# This function receives a matrix as parameter and exposes the
# set, get, setInverseOfMatrix and getInverseOfMatrix function 
# which let you set and get the original and inversed matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseOfMatrix <- function(inverse) m <<- inverse
  getInverseOfMatrix <- function() m
  list(set = set, get = get, setInverseOfMatrix = setInverseOfMatrix, getInverseOfMatrix = getInverseOfMatrix)
}


## Write a short comment describing this function

# This function will solve the matrix but will check before 
# resolving if the matrix is already cached. If this is not 
# the case the original matrix will be solved and the 
# inversed matrix will be cached and returned. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseOfMatrix()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseOfMatrix(m)
        m
}
