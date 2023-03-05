## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix() and cacheSolve() are functions that work together to find the 
#inverse of an input matrix, and then cache that inverse matrix. They do this 
#through the variable "s" that is first used in makeCacheMatrix(), but is called
#and modified in both functions. "s" is initially an empty matrix, that is created
#within makeCacheMatrix, but is called to store the inverse matrix in cacheSolve(),
#and then returned to makeCacheMatrix() with the inverse matrix still inside it.
#Both functions take advantage of free variables and lexical scoping in different
#areas to transfer data between functions.

## Write a short comment describing this function

#makeCacheMatrix() function primarily defines two variables and 4 functions
#so that they can later be called and acted upon by cachesolve. Each function
#contains x or s as free variables, but through lexical scoping, their 
#values are found in the parent environment of makeCacheMatrix. set() has special
#powers through the <<- operator because that allows it to change the value of 
#x and s in the parent environment, and therefore the values that all of the other
#functions act on. 
 
makeCacheMatrix <- function(x = matrix()) {
        s <- matrix()   
        set <- function(y) {
                x <<- y
                s <<- matrix()
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

#cachesolve is able to access x and s, and change the value of s, when makeCacheMatrix
#is passed to cacheSolve as an argument, but only if x is defined in makeCacheMatrix
#first through either the set() function, or just calling makeCacheMatrix. 
#cacheSolve first gets the value of s from makeCacheMatrix and checks if it the
#matrix is empty or not. If it is not empty, then it returns a message and the value
#of the s matrix. If s is empty, then cacheSolve fills it with the inverse matrix
#of matrix "x", using the solve() function. Once s has a value, cacheSolve calls the
#setsolve() function to set the value of s within makeCacheMatrix. Thus the inverse
#matrix of x, is cached within s, and is available for calling and modification, 
#while also being protected from outside modifications since the value of s will 
#never be looked for in the global environment. 

cacheSolve <- function(x) {
        s <- x$getsolve()
        if(all(!is.na(s))) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
