## These function will allow you to create a matrix
## and cache the inverse of the matrix. 
## As long as the matrix remains unchanged
## the inverse will not have to be recalculated
## allowing to save computation time

## This function creates a  matrix which is
## a list of functions that can be used to 
##(1)set or (2)get a matrix, and (3)set or (4)get
## the inverse of that matrix

MakeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set <- function(y){
                x<<-y
                i<<-NULL
        }
        get<- function() x
        setinv<-function(inv) i<<- inv
        getinv<-function() i
        
        list(set=set, get=get, setingetv=setinv, getinv = getinv)
}


## this function checks if the inverse has already been calculated
## for the given matrix. If not the inverse is calculated and handed
## to the setinv function above

CacheSolve <- function(x, ...) {
        ## check the cache for the inverse matrix and return it
        i <- x$getinv()
        if(!is.null(i)){
                message("the inverse matrix is")
                return(i)
        }
        ## if the inversee is not yet stored in the cache
        ## calculate it, store it in teh cache, and return it
        data<-x$get()
        i <- solve(data)
        x$setinv(i)
        i
        
}
