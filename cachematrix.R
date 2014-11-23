## This program allows us to calculate the invserse of a matrix and store the result in cache
## so that you can retrieve form the cache whenever yo call the function 

## make CacheMatrix()
## input parameters= matrix x to be created
## output parameters= list of functions -get,set,getInverse, setInverse-
## get = gets the data in the matrix x.
## set = sets the data of the matrix x.
## getinv = gets the inverse of the matrix x.
## setinv = sets the inverse of the matrix x.

makeCacheMatrix <- function(x = matrix()){ 
   inv <- NULL
   set <- function(y){
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function () inv
   list (set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve()
## input parameters= matrix x whise inverse needs to be computed
## output parameters= the inverse of the matrix x 

cacheSolve <- function (x, ...) { 
## return a matrix that is the inverse of x 
inv <- x$getinv()
if(!is.null(inv)) {
   message("getting cached data")
   return(inv)
   }
data <- x$get()
inv <- solve(data)
x$setinv(inv)
inv
}
