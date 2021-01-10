## Put comments here that give an overall description of what your
## functions do

#Below are 2 functions that are used to create a special object
#that stores a numeric matrix and cache's its inverse.

## Write a short comment describing this function

#The first function, makeCacheMatrix creates a special "matrix",
#which is really a list containing a function to
#1.Set the value of the matrix
#2.Get the value of the matrix 
#3.Set the value of the inverse
#4.Get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  
  #Initialize objects
  s <- NULL
  
  #Assign the input argument to the x object
  #Assign the value of NULL to the s object
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  #Define the getter for the matrix x
  get <- function() x
  
  #Define the setter for the inverse s
  setsolve <- function(solve) s <<- solve
  
  #Define the getter for the solve s
  getsolve <- function() s
  
  #Assign each of these functions
  #as an element within a list
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function

#The following function calculates the inverse of
#the special "matrix" created with the above function.
#First check if the inverse has already been calculated.
#If so, it gets the inverse from the cache
#and skip the calculation.
#Otherwise, it calculates the inverse of the data
#and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #Attempt to retrieve an inverse
  s <- x$getsolve()
  
  #Check that s isn't NULL
  #And if it's TURE, return s
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  #If s is NULL, get the matrix from the input object
  data <- x$get()
  
  #Calculate the inverse
  s <- solve(data, ...)
  
  #Set the inverse in the input object
  x$setsolve(s)
  
  #Return the value of the inverse
  s
  
}
