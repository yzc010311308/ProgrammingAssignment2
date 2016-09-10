
makeCacheMatrix <- function(x= matrix()){
     inv <- NULL
     set <- function(y){
       x <<- y
       inv <<- NULL
     }
     get <- function()x
     setinverse <- function(inv)inv <<- inverse
     getinverse <- function()inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cachesolve <- function(x, ...){
     inv <- x$getinverse()
     if(!is.NULL(inv)){
       message("getting cache data")
       return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
