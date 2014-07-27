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
  if(!is.matrix(x)) 
  {
    stop("Input must be a matrix object only")
  }
  if (nrow(x) != ncol(x))
  {
    stop("Matrix inverse cannot be calculated for non-square matrices")
  }
  iDet <- det(x)	
  if( iDet == 0)
  {
    stop("Matrix determinant is zero. This is a non-invertible matrix")
  }
  iInv <- NULL
  
  set <- function(matrix)
  {
    x <<- matrix
    iDet <- det(x)
    if( iDet == 0 || nrow(x) != ncol(x) || !is.matrix(x))
    {
      stop("Unusable matrix")
    }
    iInv <<- NULL	
  }
  get <- function()
  {
    return(x)
  }
  setInverse <- function(Inverse)
  {
    iInv <<- Inverse
  }
  getInverse <- function()
  {
    return(iInv)
  }
  
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
  Inv <- objX$getInverse()
  if(!is.null(Inv))
  {
    message("Printing cached matrix inverse: ")
    return(Inv)
  }
  x <- objX$get()
  Inv <- solve(x)
  objX$setInverse(Inv)
  print(Inv)
}
