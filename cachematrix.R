## R Programming - Assignment 2
## The functions makeCacheMatrix and cacheSolve can be used together to cache
## and retrieve the inverse of a matrix

##  MakeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #input must be square inveritble matrix
    #function returns a list of functions to: get the matrix, set the matrix, set the inverse, get the inverse 
    mi  <- NULL
    set <- function(y) {
        x <<- y  ## note: <<- assigns value from a different environment 
        mi <<- NULL
    }

    get <- function () x
    setinverse <- function(mInverse) mi <<- mInverse
    getinverse <- function() mi
    list(set=set, get=get, 
         setinverse=setinverse,
         getinverse=getinverse) 
       
}

## cacheSolve: Computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will return the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <-x$getinverse()
        
        if(!is.null(mi)) {
            message("Getting Cached Matrix")
            return(mi)
        }
        message ("Computing Inverse") 
        matrix <- x$get()
        mi <- solve(matrix, ...)
        x$setinverse(mi)
        mi
}

# Test - performs a  test of the MakeCacheMatrix and cacheSolve functions
test <- function() {
testMatrix <- matrix(c(4,1,4,11,16,32,12,4,1,3,4,5,8,23,14,16), nrow = 4, ncol = 4)
message("Original Matrix")
print(testMatrix)
m <- makeCacheMatrix(testMatrix)
#print(m)
mi1 <- cacheSolve(m)
print(mi1)
mi2 <- cacheSolve(m)
print(mi2)
mi3 <- cacheSolve(m)
print(mi3)
# Lines below are sample testMatrix values that can be used.
#testMatrix <- matrix(1;4, nrow=2)
#testMatrix <- matrix(c(1,2,3,7,8,9,13,14,18), nrow = 3, ncol = 3)
#testMatrix <- matrix(c(4,1,4,11,16,32,12,4,1,3,4,5,8,23,14,16), nrow = 4, ncol = 4)
}