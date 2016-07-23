
## Cache a matrix 
## set value of matrix, set value of inverse, get value of matrix get value of inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
         x <<- y
        inv <<- NULL
       }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
   }


##checks to see if the cached inverse exists if it does not calculates it if it does it grabs it from the cache

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting cached data.")
         return(inv)
       }
     print("Calculating")
     data <- x$get()
     inv <- solve(data)
     x$setinverse(inv)
     inv
   }
