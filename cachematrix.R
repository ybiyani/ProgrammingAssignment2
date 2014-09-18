## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        message("makeCacheMatrix called")
        
        setMatrix <- function(y) {
                message("makeCacheMatrix set called ")
                x <<- y
                inv <<- NULL
        }
        getMatrix <- function() {
                message("makeCacheMatrix get called; returns the matrix") 
                x
        }
        setInverse <- function(inverse){
                message("makeCacheMatrix setInverse called;sets the inverse of matrix x") 
                inv <<- inverse
        }
        getInverse <- function() {
                message("makeCacheMatrix getMatrixInverse called; gets the cached matrix of x") 
                inv
        }
        list(
                setMatrix = setMatrix, 
                getMatrix = getMatrix,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()# check to see if inverse exists
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        else
        {      message("get the matix, calculate the inverse, attach it to the matrix")  
                matrix <- x$getMatrix()
                inv <- solve(matrix)
                x$setInverse(inv)
        }
        inv
        
}

B<-matrix(c(2,3,1,5),nrow=2,ncol=2)

