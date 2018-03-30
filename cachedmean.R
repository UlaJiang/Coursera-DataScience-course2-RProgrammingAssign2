makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL #containing environment m value, not local
  }
  get <- function() x
  setmean <- function(mean) m <<- mean #containing environment m value, not local
  getmean <- function() m #调用的m值是global的，but why???
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()    #local m value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)     #local m value
  x$setmean(m)  #setmean里修改的是containing的m
  m     #local m value
}