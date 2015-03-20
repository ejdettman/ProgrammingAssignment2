## These functions basically show how you can cache a value in one function to make available to other functions

## This first function establishes a list containing functions, set, get, setinverse, and getinverse
## When a matrix is input into this function with the set function, y and inverse (as null) are put into the cache
## get simply returns the matrix in the object, setinverse caches an inputed value (invert)
## getinverse returns the value of inverse

makeCacheMatrix <- function(x = matrix()){
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(invert) inverse <<- invert 
      getinverse <- function() inverse  
      list(set = set, get=get, setinverse=setinverse, getinverse = getinverse)
}


## This function basically uses a makeCacheMatrix object and checks what the cached value of inverse is
## if inverse is null, then it pulls the matrix from the makeCacheMatrix object and uses the solve() function
## Then it assigns the inverted matrix and then sets the invert value in the makeCacheMatrix object
## finally, returns the inverse

cacheSolve <- function(x,...){
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setinverse(inverse)
      inverse
}

## if you want to test the action of these functions, uncomment these lines and then you can see how a value is cached
## upon the second running of cacheSolve

# a <- matrix(c(1,0,0,1,1,0,1,1,1), nr=3, nc=3, byrow=T)
# 
# hhh <- makeCacheMatrix()
# hhh$set(a)
# cacheSolve(hhh)
# cacheSolve(hhh)
