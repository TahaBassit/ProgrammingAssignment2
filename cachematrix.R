## I followed the outline of the example of the mean of vectors
## 

## The function MakeCacheMatrix defines a function that takes a matrix as an argument
## The function also creates a set of functions that allows the following:
##-1 set the value of a matrix
##-2 get the value of a matrix
##-3 set the value of the inverse of a matrix
##-4 get the value of the inverse of a matrix
MakeCacheMatrix <-function(x=matrix()){
      inv <-NULL
      set <- function(y) {
            x <<-y
            invM<<-NULL
      }
      get <- function() x
      setinverse <-function(inverse) inv <<-inverse
      getinverse <- function() inv
      list( get = get , set = set , getinverse = getinverse , setinverse = setinverse)
}



## Function CacheSolve returns a matrix that is the inverse of 'x' by either 
## calculating the inverse ( through the solve function)
## if there is no inverse in the cache already or alternatively calling the existing inverse from the cache  
CacheSolve <- function(x,...){
      inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}