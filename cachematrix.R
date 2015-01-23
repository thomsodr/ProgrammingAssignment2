#These two functions work together to create a special form of an
#invertible square matrix. The general purpose is to calculate the inverse
#of the matrix, but to first check the cache to see if the solution is already
#stored there - if so, the result is returned from the cache - if not, the 
#solution is computed, stored in the cache, and then returned to the user.

#this function creates a square invertible matrix (given user
#input). This matrix can then be passed to the cacheSolve function

makeCacheMatrix <- function(x = matrix()){
        i<-NULL
        set <- function(y){
                x<<-y
                i<<-NULL #indicate that the inverse of the matrix is not in the cache
        }
        get<-function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

#this function takes the specially created square matrix from the
#makeCacheMatrix function. If the inverse of this matrix has already been 
#calculated, this function returns the inverse matrix from the cache. If not
#then this function calculates the inverse of the matrix and returns it.


cacheSolve <- function(x, ...){
        i <- x$getinv()
        if(!is.null(i)){  #check to see if inverse of matrix is in the cache
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)#calculate inverse of matrix and assign to i
        x$setinv(i)
        i
}
