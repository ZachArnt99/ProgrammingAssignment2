## My functions are very similar to makeVector. I just changed some names
## and commented out a few lines of makeCacheMatrix because the local variable
## called "set" doesn't seem to be necessary. 
## Each time makeCacheMatrix runs, the value of its local variable called 
## "MCMinverse.m" gets assigned the value of NULL, and cacheSolve has to 
## re-calculate the inverse matrix instead of using a cached inverse.

makeCacheMatrix <- function(some.matrix = matrix()) {
  MCMinverse.m <- NULL
#  set <- function(some.matrix){
#    my.matrix <<- some.matrix
#    MCMinverse.m <<- NULL
#  }
  get <- function() some.matrix
  setinverse <- function(my.inverse.matrix){
    MCMinverse.m <<- my.inverse.matrix
  }
  getinverse <- function() MCMinverse.m
  list(
#    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}
## cacheSolve takes the output from makeCacheMatrix as its formal argument 
## and creates an inverse matrix. If cacheSolve runs more than once without
## makeCacheMatrix running again, it will use the cached inverse instead of
## re-calculating the inverse. So, any changes to the matrix "tryOne" should 
## be run through solve(tryOne) to make sure that an inverse exists, and also
## through makeCacheMatrix(tryOne) to clear the cache, before running cacheSolve.

  cacheSolve <- function(some.list, ...) {
    CSinverse.m <- some.list$getinverse()
    if(!is.null(CSinverse.m)) {
      message("getting cached data")
      return(CSinverse.m)
    }
    data <- some.list$get()
    CSinverse.m <- solve(data, ...)
    some.list$setinverse(CSinverse.m)
    CSinverse.m
}
        ## Return a matrix that is the inverse of 'some.matrix'

tryOne <- matrix(c(2,2,2,4,5,6,7,8,8),nrow=3,ncol=3)
solve(tryOne) ## Make sure an inverse to tryOne exists
myList <- makeCacheMatrix(tryOne) #Generate appropriate input for cacheSolve
cacheSolve(myList) #Get previously calculated inverse, or calculate a new one
myInv <- cacheSolve(myList) #Prepare to test results
tryOne %*% myInv #A matrix times its inverse should generate an identity matrix
myInv %*% tryOne #An inverse times its matrix should also be an identity matrix
myList #Check for objects in another environment
