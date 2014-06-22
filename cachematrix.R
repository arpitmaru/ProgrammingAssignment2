## 
## The below functions are aimed at saving time and computational 
## costs by using the cached value of an inverse of a matrix. 
## Using the below two functions in combination achieves this goal. 

## The first function, 'makeCacheMatrix' takes a Matrix 'M' as an
## input. It creates four functions which get the stored value of the 
## Matrix (M in the function) and its inverse 'I'. It outputs a list 
## containing thes funcitons, which would act as an input for the next 
## function 'cacheSolve'. 

makeCacheMatrix <- function(M = matrix()) {
  I <- NULL
  set <- function(y) {
    M <<- y
    I <<- NULL
  }
  get <- function() M
  setinverse <- function(Inverse) I <<- Inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 'cacheSolve achieves two things: get the cached value of the 
## matrix inverse if it already exists; if not calculate the inverse
## value by using solve(). This functions takes a List of four
## functions created by makeCacheMatrix as an input. It updates the 
## value of inverse by executing the setinverse() function and outputs 
## the inverse either cached value or calculated value. 

cacheSolve <- function(L, ...) {
  I <- L$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  Matrix <- L$get()
  I <- solve(Matrix, ...)
  L$setinverse(I)
  I
}

## The execution steps are as follows:
> M <- matrix(data=c(1,24,5,4,3,67,0,1,1), nrow=3,ncol=3)
> L <- makeCacheMatrix(M)
> cacheSolve(L)
 [,1]         [,2]         [,3]
[1,]   0.4571429  0.028571429 -0.028571429
[2,]   0.1357143 -0.007142857  0.007142857
[3,] -11.3785714  0.335714286  0.664285714
> cacheSolve(L)
getting cached data
            [,1]         [,2]         [,3]
[1,]   0.4571429  0.028571429 -0.028571429
[2,]   0.1357143 -0.007142857  0.007142857
[3,] -11.3785714  0.335714286  0.664285714
