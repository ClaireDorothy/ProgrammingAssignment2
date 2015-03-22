## Creates a special matrix that caches and retrieves the inverse

## Creates special matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        ## set value of matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        ## get value of matrix
        get <- function() x
        ##set value of inverse
        setinv <- function(solve) inv <<-solve
        ##get value of inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## solves matrix or retrieves cached matrix

cacheSolve <- function(x=matrix(), ...) {
        ##checks if inverse has been calculated
        inv <- x$getinv()
        ## if calculated skips computation
        if(!is.null(inv)){
                message("getting ccached data")
                return(inv)
        }
        ##calculates inverse and sets values
        data <- x$get()
        inv  <- solve(data, ...)
        x$setinv(inv)
        inv
        
        ## Return a matrix that is the inverse of 'x'
}
