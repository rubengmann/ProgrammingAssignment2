
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                                     #initializing inverse as NULL
set <- function(y){
  x <<- y
  inv <<- NULL
}
get <- function() {x}                           #function to get matrix x
setInv <- function(inverse) {inv <<- inverse}
getInv <- function() {inv}                      #function to obtain inverse of the matrix
list( set = set, 
      get = get, 
      setInv = setInv, 
      getInv = getInv)
}

cacheSolve <- function(x, ...) {                #gets cache data
  inv <- x$getInv()
  if(!is.null(inv)){message("Getting Cached Data")  #checks if inverse is null
    return(inv)                                 #inverse value is returned
    }
  data <- x$get()
    inv <- solve(data,...)                      #inverse value is calculated
    x&setInv(inv)
    inv
}
  
