## These functions will create an object to maintain a matrix and its
## inverse and also will solve the matrix to get the inverse

## This function creates an object of class list that maintains the
## matrix provided initially as well as the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
     ## Initiate objects for data and inverse of matrix
     inv <- NULL
     data <- matrix()
     
     ## Checks to determine that argument is indeed a matrix
     ## If not matrix then stop with message
     if(class(x)=="matrix"){
          data <-x
          inv <- NULL
     }else
          stop("Argument is not a matrix.")
     
     ## Additional functions to get the data and set/get inverse of matrix
     get <- function() data
     setinv <- function(inv) inv <<- inv
     getinv <- function() inv
     
     ## List of elements to return
     list(get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function will solve to get the inverse of a given matrix
## It will first check if the inverse of the matrix already exists 
## and retrieve it
cacheSolve <- function(x, ...) {
     ## Get the inverse stored in cache
     i <- x$getinv()
     if(!is.null(i)) { ## If the value returned is not null then print message
                       ## and return value
          message("getting cached data")
          return(i)
     }
     ## Inverse of matrix not in cache, obtain data
     data <- x$get()
     ## Solve matrix
     i <- solve(data, ...)
     ## Store inverse of matrix in cache
     x$setinv(i)
     ## Return the solution
     i
}
