## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a special "vector", which is
# really a list containing a function to
# 
# 1. setMatrix- set the value of the Matrix
# 2. getMatrix- get the value of the Matrix
# 3. setInverse- set the value of the Matrix Inverse
# 4. getInverse- get the value of the cached Matrix Inverse

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
        # return a list of functions
        list(
                setMatrix = setMatrix, 
                getMatrix = getMatrix,
                setInverse = setInverse,
                getInverse = getInverse)
        

}


## Write a short comment describing this function
# The following function calculates the inverse of the matrix specified in the special "vector"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inv in the cache via the `setInverse`
# function.

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

# Following is a test case to show the workings

print("Test case")
print("create  matrix(c(2,3,1,5),nrow=2,ncol=2)")

B<-makeCacheMatrix(matrix(c(2,3,1,5),nrow=2,ncol=2))

print(B$getMatrix())

print("Calculate Inverse")

Binv<-cacheSolve(B)

print (Binv)

print("Confirm the inverse is correct by checking if the multiplication") 
print("of it matrix and inverse results in an Identity Martix ")

r<-B$getMatrix()%*%Binv
print(r)

print("Check to see the cached Inverse is returned")

Binv1<-cacheSolve(B)

