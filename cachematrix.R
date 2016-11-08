## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL # creates a variable Inv with value NULL  
        set <- function(y) {
                
                x <<- y #assign input variable to x in the parent environment 
                Inv <<- NULL # assign NULL to Inv in the parent env 
                
                # this means that if there is already a cached value for the inverse, it will be cleared rather than reissued incorrectly 
                
        }
        # x is retrieved from the parent environment        
        get <- function() x 
        
        #define the setter for the inverse 
        setInverse <- function(solve) Inv <<- solve 
        
        getInverse <- function() Inv # getter for the inverse 
        
        #define a list of varibles containing the functions we have created so that it is accessible later in the parent environment 
        
        list(set = set, get = get,  
             setInverse = setInverse,
             getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` should retrieve the inverse from the cache. Use the solve(x) function

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse() #looks for a value for Inv from makeCacheMatrix
        if(!is.null(Inv)) { #if there is a value, it is returned along with the message 
                message("getting cached data")
                return(Inv)
        }
        data <- x$get() #assigns the input to the variable 'data'
        Inv <- solve(data, ...) #calculates the inverse and assigns it to Inv 
        x$setInverse(Inv) # puts Inv into the setInverse variable 
        return(Inv)
}
