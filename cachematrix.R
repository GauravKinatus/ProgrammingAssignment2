## R programming Coursera week 3, programming assignment 2
## Gaurav Garg (gaurav_garg@yahoo.com)

## this program has two functions to calculate and cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        #setting the object to NULL, by default
        i <- NULL
        
        #defining the set and get funcitons for the object. 
        #we interact with this object using the get to read and set to write the values in the object
        set <- function(y) {
                # x is a local variable where we store the incoming matrix, y
                x <<-y
                # i is the local variable where we will retain a copy of the inverse of the matrix
                i <<- NULL
        }
        get <- function() x
        
        # when the setinverse function is called on the object, we calculate the inverse of the matrix and
        # store it in the local instance of 'i'
        setinverse <- function(solve) i <<- solve
        # use getinverse function to get the value of the inverse of the matrix
        getinverse <- function() i
        
        # define a list of functions supported by this object (think of this as the interfaces or WSDL)
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

## Assumption: assignment asks us to assume that the matrix supplied is always invertible. No validations in the code

# this is a wrapper function to interact with the object
# usage -       step 1: call MakeCacheMatrix() with a square matrix (or data) to create an object
#                       e.g.  a<-makeCacheMatrix(matrix(round(rnorm(16),1),4,4)) matrix
#               step 2: check the values in the matrix object
#                       e.g. a$get()
#               step 3: get the inverse of the matrix
#                       e.g. cacheSolve(a)
cacheSolve <- function(x, ...) {
        
        ## function below calls the getinverse() on the object created by the MakeCacheMatrix object
        i <- x$getinverse()
        # if the inverse matrix is already present, return the object i
        if (!is.null(i)){
                message ("getting cached data")
                return (i)
        }
        # if the inverse was not calculated previously, calculate inverse
        data <- x$get() 
        i <- solve (data,...)
        # and store it in the private instance of i for future use
        x$setinverse(i)
        i
}
