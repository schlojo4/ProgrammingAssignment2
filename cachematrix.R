## makeCacheMatrix: creates a special matrix object that can cache its inverse. Input is x= matrix. returns a list of defined functions.
## 1) hold empty inverse object inv
## 2) sets matrix
## 3) gets matrix
## 4) sets inverse
## 5) gets inverse
## 6) return list with functions


makeCacheMatrix <- function(x = matrix()) {
        
        # hold cached inverse
        inv <- NULL 
        
        # set matrix
        set <- function(y){
                x<<-y
                inv <<- NULL #reset
        }
        
        # get matrix
        get <- function() x
        
        
        # set the inverse
        setInverse <- function(inverse) inv <<- inverse
        
        
        # get the inverse
        getInverse <- function() inv
        
        
        # Return a list of the above functions
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve: computes inverse of special matrix returned by makeCacheMatrix.
## 1) Try to find cached inverse
## 2) If inv is cached, return message and return cached inv
## 3) If no cached inverse is found, it retrieves the matrix data
## 4) Computes the inverse using solve()
## 5) Caches new inverse in the special matrix object.
## 6) Returns the computed inverse


cacheSolve <- function(x, ...) {
        
        # get the cached inverse
        inv <- x$getInverse()
        
        
        if(!is.null(inv)){
                message("getting cached data") # Inform that cached data is used
                return(inv) # Return the cached inverse
        }
        
        # Get the original matrix
        data <- x$get() 
        
        # Compute the inverse
        inv <- solve(data, ...) 
        
        # Cache the inverse
        x$setInverse(inv)
        
        inv 
}
