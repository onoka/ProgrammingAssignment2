## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        ## i contains the NULL
        i <- NULL
        
        ##makeCacheMatrix has 4 functions; get, set, getinverse, setinverse
        ##set function changes the matrix stored in the main function
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        ## get function returns the matrix stored inthe main function
        get <- function() x
        
        ##setinverse store the value of the input in a variable i into the main function
        setinverse <- function(inv) i <<- inv
        
        ##getinverse store the value of the input in a variable i and return it
        getinverse <- function() i
        
        ##make a list of all the stored functions in the main function
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##cachesolve first checks that the value i stored in getinverse exists and is not NULL
        i <- x$getinverse()
                if(!is.null(i)){
                        message("getting cached data")
                        return(i)
                }
        ##else the matrix in makeCacheMatrix, the i calculates the inverse of the matrix
        ##x$setinverse(i) strores this inverse
        ## the return is i
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
