## Put comments here that give an overall description of what your
## functions do

## The following function return an empty matrix in which the inverse is stored

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function(y){
                x<<-y
                invr <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invr <<- inverse 
        getinverse <- function() invr 
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## The following function checks for a solution in the cache. If solution is
## found, it returns directly from the cache. If not, it compute the inverse and
## updates the cache. 

cacheSolve <- function(x, ...) {
        invr <- x$getinverse()
        if(!is.null(invr)){
                message("Getting Cached Data")
                return(invr)
        }
        matrix <- x$get()
        invr <-solve(matrix,....)
        x$setinverse(invr)
        invr
        
        ## Return a matrix that is the inverse of 'x'
}
