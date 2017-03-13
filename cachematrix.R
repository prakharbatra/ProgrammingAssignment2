## The following functions are used to cache the inverse of a matrix

## makeCacheMatrix creates a list

makeCacheMatrix <- function(x = matrix()) {
  
  invr<-NULL
  set<-function(y) {
    x<<-y
    invr<<-NULL
  }

  get<-function()x
  setinvr<-function(inverse) invr<<-inverse
  getinvr<-function() invr
  list(set=set,get=get, setinvr=setinvr, getinvr=getinvr)
}


## Write a short comment describing this function
## This function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  invr<-x$getinvr()
  if(!is.null(invr)) {
    message("Getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setinvr(invr)
  invr
}


x = rbind(c(2, -5), c(-5, 2))
m = makeCacheMatrix(x)
m$get()

cacheSolve(m)
cacheSolve(m)
solve(x)
