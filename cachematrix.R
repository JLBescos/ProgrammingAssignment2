## These functions create several functions and objects enabling the 
## computation and cache of a given matrix and its inverse. 
## makeCacheMatrix creates the placeholders and the bridge to
## get the calculated value of the inverse matrix value into its environment
## cacheSolve checks if cache exists and updates 
## the calculation if required


## The function makeCacheMatrix takes a matrix as a formal argument; 
## then sets m -the place holder for x inverse value- to NULL 
## and it creates 4 functions which returns as a named list:

## 1) set:it sets the matrix to be cached and resets m to NULL
## we can use set also to modify the original matrix after sourcing the 
## functions- see the example below-. This is due to the use
## of the special assignment operator, <<-, to change the values associated
## with x (the original matrix) and m (its inverse) within the 
## makeCacheMatrix environment. 
## By resetting m to NULL, it forces R to update the cache by applying the
## solve function to y (the new value of x/the new matrix) which is done
## via cacheSolve function

## 2) get: it calls the original matrix when needed

## 3) setinvmatrix: it sets the value of m. This value is calculated under
## cacheSolve function (within a different environment) and, then, linked to 
## matrix x -as its inverse m- via the x$setinvmatrix(m) code
## which calls <<- to alter m. In other words, this function opens the
## makeCacheMatrix-environment's door for getting in the value of m calculated in 
## cacheSolve-environment.

## 4) getinvmatrix: it gets the inverse when needed

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinvmatrix<-function(solve) m<<- solve
    getinvmatrix<-function() m
    list(set=set, get=get,
         setinvmatrix=setinvmatrix,
         getinvmatrix=getinvmatrix)
}


## Write a short comment describing this function

## The function cacheSolve starts by calling m
## via the getinvmatrix function defined above: if m is not NULL
## it returns the cached value of m. If no previous value of m exists 
## (different from NULL) then it makes the inversion, pass it to the 
## appropriate envir via x$setinvmatrix(m), and delivers its value.

cacheSolve <- function(x, ...) {
    m<-x$getinvmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinvmatrix(m)
    m
}

##########################################################################################

## Example: after saving this file into your wd (please rename it to "renamedw3a.R")
## run the following code

# source("renamedw3a.R")
# x<-matrix(c(1,3,5,7), nrow=2, ncol=2)
# m<-makeCacheMatrix(x)
# cacheSolve(m)
# cacheSolve(m)

## defining a new matrix

# y<-matrix(c(21,35,53,17), nrow=2, ncol=2)

## applying set to modify the original matrix

# m$set(y)

## asking for the new value of m

#cacheSolve(m)
#m$get()
##########################################################################################
