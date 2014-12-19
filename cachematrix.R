## The whole program contains two functions for caching the inverse matrix
## of a input matrix. The first function makeCacheMatrix() creats a list
## of objects that can be accessed by another function(). makeCacheMatrix() is
## also used for storing/caching the calculated inverse matrix. 


## The makeCacheMatrix()  creats a list of objects. Four fucntions included
## It  stores the input matrix itself and cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) { ##input an inversible matrix
  
    inversem <- NULL ## set inversem (the one caching the inverse matrix)to null
    
    set <- function(y) { #This set function replace the previous input matrix
      x <<- ynop
      inversem <<- NULL
    }
    
    get <- function() x #This function return the original inputmatrix
    
    setinverse <- function(inversematrix) {
            inversem <<- inversematrix #It caches calculated inverse matrix            
    }
            
            
    
    getinverse <- function() inversem # Return the cached inverse matrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


##cacheSolve() does several things when called
##It first gets the value of "inversem" from makeCacheMatrix() by calling 
##getinverse()
##It then checks if that value is NULL or not
##If that's NULL, cacheSolve() solves for inverse matrix and
##pass it to the function setinverse() defined within makeCacheMatrix()
##the process is so-called caching
##If value of "inversem" retrieved is not NULL,implying there's a cached matrix
##cacheSolve() simply returns the retrieved value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inversem <- x$getinverse() #Get value of inversm in makeCacheMatrix()
        
        if(!is.null(inversem)){ #if it is not NULL, simply return that matrix
                message("getting the cached inverse matrix")
                return(inversem)        
        }
        
        #if that value is NULL, calculate the inverse matrix and cache it
        originalmat <- x$get()
        inversem <- solve(originalmat)
        x$setinverse(inversem)
        inversem
}
