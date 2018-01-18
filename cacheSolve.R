cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
}

##inverse of "matrix" returned by makeChacheMatrix(), return inverse from cache if already calculated