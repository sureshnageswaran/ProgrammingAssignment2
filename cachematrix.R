## Author : Suresh Nageswaran
## Email: sureshnageswaran@yahoo.com

## Function makeCacheMatrix  takes in a matrix and 
## and returns an object who's inverse can be cached.

## Invoke like so: 
##
## matrixT <- matrix(c(-1, 0, 1, 2), 2, 2)
##
## objTest <- makeCacheMatrix(matrixT)
##
## If the determinant of the input matrix is zero
## or if it is a non-square matrix, the execution terminates
## 
## The returned object has 4 "public" functions that can
## modify it, namely : set(), get(), setInverse() and getInverse()

makeCacheMatrix <- function(x = matrix())
{
  # Test if the input is a valid matrix
  if(!is.matrix(x)) 
  {
    stop("Input must be a matrix object only")
  }
  # Test if the input is a square matrix
  if (nrow(x) != ncol(x))
  {
    stop("Matrix inverse cannot be calculated for non-square matrices")
  }
  
  # Test if the determinant is zero
  iDet <- det(x)	
  if( iDet == 0)
  {
    stop("Matrix determinant is zero. This is a non-invertible matrix")
  }
  # Initialize the variable (private)
  iInv <- NULL
  
  # Set the matrix
  set <- function(matrix)
  {
    # Unlike a C++ constructor, this function is not invoked 
    # during object instantiation. 
    # If it is invoked explicitly, then we need to set
    # the matrix inverse to NULL and store it.
    
    # Do the standard checks to ensure the matrix is usable
    x <<- matrix
    iDet <- det(x)
    if( iDet == 0 || nrow(x) != ncol(x) || !is.matrix(x))
    {
      stop("Unusable matrix")
    }
    # solve for the inverse and store it
    iInv <<- NULL
  }
  # Get the matrix - return the matrix
  get <- function()
  {
    return(x)
  }
  # Store the inverse into the private variable
  setInverse <- function(Inverse)
  {
    iInv <<- Inverse
  }
  #Retrieve the inverse from the private variable
  getInverse <- function()
  {
    return(iInv)
  }
  # declare the list of invokable functions
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## cacheSolve is a function that operates on an object that is
## of the type created by the makeCacheMatrix() call.
## The first time cacheSolve is invoked, it calculates the matrix
## inverse. 
## The second time onwards whenever cacheSolve is invoked, the
## cached matrix inverse stored in the object instance is 
## returned.

cacheSolve <- function(objX, ...)
{
  # Invoke the getInverse method on the object
  Inv <- objX$getInverse()
  # If a valid answer came back, you have just retrieved a cached value 
  if(!is.null(Inv))
  {
    message("Printing cached matrix inverse: ")
    return(Inv)
  }
  # Get the object - stored value
  x <- objX$get()
  # Calculate the inverse
  Inv <- solve(x)
  # Store the inverse into the obkect instance
  objX$setInverse(Inv)
  print(Inv)
}
