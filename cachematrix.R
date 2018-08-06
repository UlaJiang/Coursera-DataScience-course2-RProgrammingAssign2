## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #declare the inver value as NULL
  inver <- NULL
  #set new matrix with the inver as NULL again
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  #get matrix
  get <- function() x
  #set inverse value by modify the containing inver value in this function
  setinver <- function(inverse) inver<<-inverse
  #get inverse vaue
  getinver <- function() inver
  #makeCacheMatrix return a list with four elements in it
  list(set = set, get = get, setinver = setinver, getinver = getinver)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #check whether the inverse value has been calculated and cached
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  #if not, calculated inverse value and set the value in setinver()
  data <- x$get()
  inver <- solve(data)
  x$setinver(inver, ...)
  #this function return inver value
  inver
}
