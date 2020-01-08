## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#returns a vector consisting of (setmatrix, getmatrix, setmean, getmean)
makeCacheMatrix <- function(x = matrix()) {
  #clear any cached inverse
  i <- NULL
  #define setmatrix and setinverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  setinverse <- function(solved) i <<- solved
  #define getmatrix and getinverse, which return matrix and inverse, respectively
  get <- function() x
  getinverse <- function() i
  #create list with named elements
  list( set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

## checks if there is an inversematrix stored for x(a matrix), and returns it. 
##if none is stored, calculates and stores the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## check cache. if null, nothing is cached. 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## as there is nothing cached, solve matrix and save it in cache
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
