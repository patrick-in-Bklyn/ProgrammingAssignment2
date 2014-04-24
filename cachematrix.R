## This function takes a matrix and sets up a series of parameters that can be called by other functions. 
## The first function takes a matrix and puts the matrix and its inverse into the cache.

## The second function checks if the matrix inverse is in cache.  If not it calculates that inverse and puts it there.

makeCacheMatrix <- function(x = matrix()) { #load up the matrix
        m <- NULL
        set <- function(y) {
                x <<-  y        #taking in new matrix
                m <<- NULL      # resetting the memory space for new matrix inverse.
        }
        
        get <- function() x     # send back the original matrix when requested
        setinverse <- function(inverse) m <<- inverse   #take in the inverse and save to cache.
        getinverse <- function() m #send back the inverse when requested.
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()     #check in the makeCacheMatrix function if the inverse has been set
        if(!is.null(m))         # if it has been set, then take from cache and return the value.
                {
                message("Getting Cached inverse of Matrix")
                return(m)
                }        
        data <- x$get()         # if the get$inverse is null(it has not been set), then get the original matrix
        m <- solve(data, ...)   # solve that matrix
        x$setinverse(m)         #set the inverse parameter in the makecacheMatrix function.
        m                       # return the mean. 
}
