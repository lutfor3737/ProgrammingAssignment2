## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # mcache is the variable where to store inverse matrix results
  mcache <- NULL
  # setter function
  set <- function(y) {
    x <<- y
    mcache <<- NULL
  }
  # Getter function
  get <- function() x
  # Inverse setter
  setInverse <- function(inverse) mcache <<- inverse
  # inverse Getter
  getInverse <- function() mcache
  # returns list with 4 member functions(set,get,setInverse,getInverse)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # get inverse matrix from object x into mcache variable
  mcache <- x$getInv()
  # checking if already calculated, if already calculated then return with earlier results
  if(!is.null(mcache)) { 
    message("getting cached data")
    return(mcache) 
  }
  # get object x into data variable
  data <- x$get() 
  # solve it
  mcache <- solve(data)
  # set inverted martix in cache
  x$setInv(mcache) 
  ## Return a matrix that is the inverse of 'x'
  mcache 
       
}
