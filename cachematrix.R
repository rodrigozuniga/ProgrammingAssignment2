####################################################################
## makeCacheMatrix function creates a list with a null matrix 
##  and internal functions to set and get a given matrix as well as 
##  set and get the the inverse of that matrix 
####################################################################

makeCacheMatrix <- function(x = matrix()) {
        inv <-matrix()
        set <-function(y){
                x<<-y
                inv <-matrix()
        }
        get <- function() x
        setinv <- function(inverse) {inv <<- inverse}
        getinv <- function() inv
        
        list(set = set, 
             get = get,
             setinv=setinv,
             getinv=getinv)
}

#####################################################################
## CacheSolve takes a list created by makeCacheMatrix and checks 
## if the inverse of the matrix already exists in the list. If the 
## inverse already exists then it returns that matrix with a message. 
## If the inverse does not exist then it calculates the inverse 
## of the matrix using the solve function and returns it 
#####################################################################

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!all(is.na(inv))) {
                message("getting inverse from cache")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinv(inv)
        inv
}
