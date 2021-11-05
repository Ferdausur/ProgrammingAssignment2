## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function returns a list of functions 

makeCacheMatrix <- function(x = matrix()) {
        
        # inv is set to NULL as initially no inverse matrix is available 
        inv<- NULL
        
        # function set assigns y to the variable x at global environment 
        set<- function(y){
                x<<- y
                # and inv remains NULL as no inverse matrix is created
                inv<- NULL
        }
        
        # gets the value of x from the environment 
        # x is a free variable and the variable is called from the environment
        # where it was called
        get<- function() x
         
        # sets a matrix "inv_matrix" as inv in global environment
        set_inverse<- function(inv_matrix){
                inv<<- inv_matrix
        }
        
        # gets the value of inv from the environment 
        get_inverse<- function() inv
        
        
        # returns a list of functions 
        list(set= set, 
             get= get,
             set_inverse = set_inverse,
             get_inverse = get_inverse
             )
        

}


## Write a short comment describing this function

# this function takes "makeCacheMatrix" function as an input
# and first checks if the inverse matrix is in the global environment to return
# if not present, it makes an inverse matrix from the matrix
# the value of matrix is called from the "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # gets the value of inv with get_inverse function
        inv<- x$get_inverse()
        
        # if inv is not NULL,
        # value for the inverse matrix is already present in the environment 
        if(!is.null(inv)){
                # value retrieved from cache
                message("getting cached data")
                # inv returned
                return(inv)
        }
        
        # if inv is NULL,
        # value for the matrix is get from the "makeCacheMatrix" function
        Matrix<- x$get()
        
        # solve function creates inverse matrix of the matrix 
        inv<- solve(Matrix)
        
        # set_inverse is used to store the value to cache 
        x$set_inverse(inv)
        
        # inv is returned
        inv
        
}
