
## first function creates an object with 2 values

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  
  setinv <- function(invertedMatrix) inv<<-invertedMatrix
    
  getinv <- function() inv
  list(setinv=setinv, getinv=getinv)

}


## second function checks if a cached inv matrix exists
## if it doesn't, it computs it and saves it for next use

cacheSolve <- function(x, ...) {
  i<-x$getinv()                #saves value of getinv into i
  if (inv(!is.null(i))){       #if inv has already been computed, return it
    return(i)
  }
  else {              #if inv hasn't yet been computed, 
    invx<-solve(x)    #compute it and save it into invx 
    x$setinv <-invx   #save this value into inv through setinv function
    invx              #then return the inverted matrix
  }     
}
