## makeCacheMatrix creates functions for storing and getting inverse of the matrix from free variable 
## cacheSolve function is to get the cached inverse matrix if already exists else create the inverse of the matrix and store in the free variable.  Finally it retruns inverse matrix.
 
##This function  creates set, get, setinverse and getinverse functions and returns 
makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
        
        # set the value of the matrix(free variable) with the input and
        # and  set inverse of the matrix(free variable) with NULL  
        set <- function (y) {
               x <<- y
               i <<- NULL
        }
        
        # get the value of the matrix (free variable)
        get <- function() x
        
        # sets the inverse matrix (free variable) with input matrix
        setinverse <- function (inverse)  i <<- inverse
        
        # gets the inverse matrix (free variable)
        getinverse <- function () i
  
        # returns set, get, setinverse and getinverse functions
        list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)

}

 
##This function returns the inverse of  'x'  if already exists in the free variable. If it doesn't, creates the ##invers of the matrix, set's the free variable and returns the inverse 

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'

       # Get the inverse of the matrix from free varibale
       i <- x$getinverse()
     
       # If matrix inverse already exists, return the inverse matrix
       if (!is.null(i)) {
         message ("getting catched data")
         return(i)
      
       }
     
       # If matrix inverrse doesn't exists, get the matrix  
       data <- x$get()
     
       # use solve function to create inverse of the matrix
       i <- solve(data)
     
       # set the inverse of the matrix to free variable 
       x$setinverse(i)
     
       # Return the inverse of the matrix  
       i

}
