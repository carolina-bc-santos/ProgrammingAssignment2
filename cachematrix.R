## The following description is based on Greski's artcile "Demystifying makeVector()"

## The cachematrix.R file contains two functions: makeCacheMatrix() and cacheSolve()
## The first function in the file,makeCacheMatrix(), creates an R object that stores a matrix
## and its inverse. The second function, cacheSolve(), requires an argument that is returned 
## by makeCacheMatrix() to retrieve the inverse from the cached value that is stored in 
## the makeCacheMatrix() object's environment.


## The makeCacheMatrix() builds a group of functions that are then returned within a list
## to the parent environment

makeCacheMatrix <- function(x = matrix()) { ##Initialization of x object
  ##Initialize the inverse object
  inverse <- NULL 
  
  ##Define the set() function, which takes an argument named y
  set <- function(y){
    ##Assign the input argument to the x object in the parent environment
    x <<- y 
    ##Assign the value of NULL to the inverse object in the parent environment
    inverse <<- NULL 
  }
  
  ##Define the getter for the matrix x
  get <- function() x 
  
  ##Define the setter for inverse
  setinverse <- function(solve) inverse <<- solve 
  
  ##Define the getter of the inverse
  getinverse <- function() inverse 
  
  ##Assign each of the aforementioned functions as an element within a list()
  ##and returns this list to the parent environment
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve() function is necessary to populate or retrieve the inverse 
## of an object of the type makeCacheMatrix()

cacheSolve <- function(x, ...) { ##Initialization of x object
  
  ## Return a matrix that is the inverse of 'x'
  ##The function tries to retrieve the inverse of the object x
  ##First by calling the getinverse() function on the input x
  inverse <- x$getinverse()
  
  ##And then it checks if the inverse is NULL. If this value is not NULL, then there is a valid
  ##cached inverse and we can return it to the parent environment
  if(!is.null(inverse)){
    message("getting cached inverse")
    return(inverse)
  }
  
  ##If the result of !is.null(inverse) is FALSE, cacheSolve() gets the matrix from the input
  ##object, calculates the inverse and uses the setinverse() function on the input object 
  ## and then returns the value of the inverse to the parent environment by printing the 
  ##inverse object
  mymatrix <- x$get()
  inverse <- solve(mymatrix,...)
  x$setinverse(inverse)
  inverse
}




